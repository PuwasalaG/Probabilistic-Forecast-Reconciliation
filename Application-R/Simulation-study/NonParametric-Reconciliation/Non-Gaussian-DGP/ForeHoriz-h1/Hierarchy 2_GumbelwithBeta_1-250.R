#Bootstrap coherent future paths obtained using numerical optimization method.
#Method 1: Estimating W
#Method 2: Estimating through the reparameterisation using cholesky decomposition
#Method 3: Estimating P directly, imposing the constraint PS=I


library("forecast")
require("Matrix")
require("hts")
require(miscTools)
require("MASS")
require("hydroGOF")
require("numDeriv")
require("psych")
require(Rsolnp)
require(VineCopula)
require(copula)
require(tidyverse)

##This hierarchical structure contains 2 bottom level series with one disaggregation
#level

#m - Number of bottom level series
#N - Length of the original data dagenerated
#L - Length of the training data set
#n - Total number of series
#B - The size of the future paths generated 
#C - Number of replicates

set.seed(1989)
start<-Sys.time()

C <- 1
N <- 2500
L <- 500
r <- 500
m <- 4
B <- 2500
H <- 1

ES_reconciled_meth1_Opt.W_1_250 <- numeric(1000)
ES_reconciled_meth3_Opt.P_1_250 <- numeric(1000)
ES_reconciled_MinT.shr_1_250 <- numeric(1000)
ES_reconciled_MinT.wls_1_250 <- numeric(1000)
ES_reconciled_OLS_1_250 <- numeric(1000)
ES_unrecon_1_250 <- numeric(1000)

VS_reconciled_meth1_Opt.W_1_250 <- numeric(1000)
VS_reconciled_meth3_Opt.P_1_250 <- numeric(1000)
VS_reconciled_MinT.shr_1_250 <- numeric(1000)
VS_reconciled_MinT.wls_1_250 <- numeric(1000)
VS_reconciled_OLS_1_250 <- numeric(1000)
VS_unrecon_1_250 <- numeric(1000)


Bottom_level <- read.csv("Bottom_level.csv")[,-1]

#Generating the hierarchy

Hierarchy <- suppressMessages(hts(Bottom_level, list(2, c(2,2))))
AllTS <- allts(Hierarchy)
n <- ncol(AllTS)

#Generating the summing matrix
S <- smatrix(Hierarchy)

#Function to Obtain the shrinkage covariance matrix estimate of the based 
#forecast errors (From MinT package)

lowerD <- function(x)
{
  n2 <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n2))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!")
  p1 <- ncol(x)
  n2 <- nrow(x)
  covm <- crossprod(x) / n2
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n2 * (n2 - 1))) * (crossprod(xs^2) - 1/n2 * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}

#Simulation starts from here.

for (j in 1:2)
  
{
  
  AllTS_a <- AllTS[j : (1100+j),]
  
  Testing <- matrix(NA,r,n)
  Training <- list()
  Unreconciled_future_paths <- list()
  
  for (q in 1:r)
  {
    Training[[q]] <- AllTS_a[q:(L+q-1), ]
    Testing[q,] <- AllTS_a[(L+q), ]
  } 
  #End_future.path_train1 <- Sys.time()
  
  #Model fitting, forecasting and obtaining residuals
  
  Residuals_all_training <- matrix(NA, nrow = nrow(Training[[q]]), ncol = n)
  fit_training <- list()
  Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
  
  Start_future.path_train2 <- Sys.time()  
  for (q in 1:r)
  {  
    for(i in 1:n)
    {
      fit_training[[i]] <- auto.arima(Training[[q]][,i])
      Base_forecasts_training[,i] <- forecast(fit_training[[i]], h=1)$mean
      Residuals_all_training[,i] <- Training[[q]][,i] - fitted(fit_training[[i]])
    }
    
    Index <- base::sample(c(1:nrow(Residuals_all_training)), size = B, replace = TRUE)
    
    BS <- matrix(rep(Base_forecasts_training, B), nrow = B, ncol = n, byrow = TRUE)
    
    Unreconciled_future_paths[[q]] <- BS + Residuals_all_training[Index,]
    
  }
  
  ###Method 1: Estimating W
  
  Energy_score_method1 <- function(Vec_W, Future_paths, Test, n1, B1, r1, S1)
  {
    Inv_W <- miscTools::symMatrix(Vec_W, nrow = n1, byrow = FALSE)
    a <- t(S1)%*%Inv_W%*%S1
    b <- t(S1)%*%Inv_W
    P <- solve(a, b)
    
    Proj.mat <- S1%*%P
    
    Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
    
    ES_Reconciled <- numeric(r1)
    
    for (q in 1:r1)
    {
      
      d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
      
      ES_1 <- sqrt(rowSums(d1^2))
      
      d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
      
      ES_2 <- sqrt(rowSums(d2^2))
      
      ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
      
    }
    
    mean(ES_Reconciled)
    
  }
  
  #Function to return the gradient
  
  Grad_method1 <- function(x, Future_paths, Test, n1, B1, r1, S1)
  {
    Inv_W <- miscTools::symMatrix(x, nrow = n1, byrow = FALSE)
    
    a <- t(S1)%*%Inv_W%*%S1
    b <- t(S1)%*%Inv_W
    P <- solve(a, b)
    Inv <- solve(a)
    
    Proj.mat <- S1%*%P
    
    Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
    
    ES_grad <- list(r1)
    
    for (q in 1:r1)
    {
      d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
      es_1 <- sqrt(rowSums(d1^2))
      
      d1 <- d1/es_1
      
      D1 <- S1 %*% Inv %*% t(S1) %*% t(d1) %*% Future_paths[[q]] %*% (diag(1,n1,n1) - t(Proj.mat))
      
      b2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
      es_2 <- sqrt(rowSums(na.omit(b2)^2))
      d2 <- Future_paths[[q]][1:(B1-1),] - Future_paths[[q]][2:B1,]
      
      d21 <- (d2/es_2)
      d21[is.na(d21)] <- 0
      
      D2 <- S1 %*% Inv %*% t(S1) %*% Proj.mat %*% t(d21) %*% d2 %*% (diag(1,n1,n1) - t(Proj.mat))
      
      ES_grad[[q]] <- D1/nrow(d1) - D2/(2*nrow(d21))
      
    }
    
    mean.W <- popbio::mean.list(ES_grad)
    
    return(gdata::upperTriangle(mean.W, diag = TRUE, byrow = TRUE))
    
  }
  
  #Calculating initial Inv.weight matrix for the optimization problem
  
  Int_par_w <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  start.opt.W <- Sys.time()
  OPT_method1 <- optim(Int_par_w, Energy_score_method1, gr = Grad_method1, method = "BFGS", 
                       Future_paths = Unreconciled_future_paths, Test = Testing, n1 = n, B1 = B, 
                       r1 = r, S1=S)
  end.opt.W <- Sys.time()
  Opt_Vec_W <- OPT_method1$par
  
  Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
  
  Method1_Opt_P <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
  
  
  #Method 3: Estimating P directly, imposing the constraint PS=I
  
  #Following function calculates the Energy score of coherent future paths
  #where the future paths are reconciled by a suitable P matrix 
  
  Energy_score_method3 <- function(Vec_P, Future_paths, Test, m1, n1, B1, r1, S1)
  {
    P <- matrix(Vec_P, m1, n1, byrow = TRUE)
    Proj.mat <- S1%*%P
    
    Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
    
    ES_Reconciled <- numeric(r1)
    
    for (q in 1:r1)
    {
      
      d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
      ES_1 <- sqrt(rowSums(d1^2))
      
      d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
      
      ES_2 <- sqrt(rowSums(d2^2))
      ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
      
    }
    
    
    mean(ES_Reconciled)
    
  }
  
  #Function that imposes the constraint PS=I into the optimization.
  
  eqn1 <- function(Vec_P, Future_paths, Test, m1, n1, B1, r1, S1)
  {
    D <- bdiag(t(S1), t(S1), t(S1), t(S1))
    A <-  D %*% Vec_P 
    
    return(rep(t(A)))
  }
  
  #Constraint vector (RHS)
  Eq <- c(rep(diag(1, m, m)))
  
  #OLS P
  
  OLS_P <- solve(t(S)%*%S)%*%t(S)
  
  Int_par_P <- as.vector(t(OLS_P))
  
  start.opt.P <- Sys.time()
  OPT_method3 <- solnp(Int_par_P, Energy_score_method3, eqfun = eqn1, eqB = Eq, Future_paths = Unreconciled_future_paths,
                       Test = Testing, m1 = m,  n1 = n, B1 = B, r1 = r, S1=S)
  end.opt.P <- Sys.time()
  
  Method3_Opt_P <- matrix(OPT_method3$pars, nrow = m, ncol = n, byrow = TRUE)
  
  
  ##Evaluation
  
  Training_eval <- AllTS_a[(r+1):(L+r),]
  Testing_eval <- AllTS_a[(L+r+1),]
  
  
  #Model fitting, forecasting and obtaining residuals
  
  Residuals_all_eval <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  fit_eval <- list()
  Base_forecasts_eval <- matrix(NA, nrow = H, ncol = n)
  
  
  for(i in 1:n)
  {
    fit_eval[[i]] <-auto.arima(Training_eval[,i])
    Base_forecasts_eval[,i] <- forecast(fit_eval[[i]], h=1)$mean
    Residuals_all_eval[,i] <- Training_eval[,i] - fitted(fit_eval[[i]])
  }
  
  #Obtaining the shrinkage estimator for the covariance matrix of the based
  #forecast errors
  
  
  targ1 <- lowerD(Residuals_all_eval)
  shrink1 <- shrink.estim(Residuals_all_eval,targ1)
  W.h_eval <- shrink1[[1]]
  
  #MinT_P.shr matrix
  
  Inv_W.h <- solve(W.h_eval)
  MinT_P.shr <- solve(t(S)%*%Inv_W.h%*%S)%*%t(S)%*%Inv_W.h
  
  #WLS P 
  n1 <- nrow(Residuals_all_eval)
  Sigma_sample <- crossprod(Residuals_all_eval)/n1
  Sigma_WLS <- diag(diag(Sigma_sample),n,n)
  
  Inv_WLS <- solve(Sigma_WLS)
  MinT_P.wls <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS
  
  #Generating future paths for 1-step ahead forecast horizon using 
  #bootsrapped errors
  
  Future_paths_eval <- matrix(0, nrow = B, ncol = n)
  
  for(k in 1:B)
  {
    Index_eval <- base::sample(c(1:nrow(Residuals_all_eval)), size = H, replace = TRUE)
    
    Future_paths_eval[k, ] <- Base_forecasts_eval + Residuals_all_eval[Index_eval,]
    
  }
  
  
  #Obtaining reconciled future paths using Optimal_P, MinT.shrink P, MinT.sam and OLS P 
  
  Reconciled_meth1_Opt.W_future_paths <- t(S%*%Method1_Opt_P%*%t(Future_paths_eval))
  Reconciled_meth3_Opt.P_future_paths <- t(S%*%Method3_Opt_P%*%t(Future_paths_eval))
  Reconciled_MinT.shr_future_paths <- t(S%*%MinT_P.shr%*%t(Future_paths_eval))
  Reconciled_MinT.wls_future_paths <- t(S%*%MinT_P.wls%*%t(Future_paths_eval))
  Reconciled_OLS_future_paths <- t(S%*%OLS_P%*%t(Future_paths_eval))
  
  
  #Calculating Enery score to evaluate
  
  Energy_score <- function(Data, Rel, B, n)
  {
    
    d1_eval <- Data - matrix(rep(Rel,B),B,n,byrow = TRUE)
    ES_1_eval <- sqrt(rowSums(d1_eval^2))
    
    d2_eval <- Data[1:(B-1),] - Data[2:B,]
    
    ES_2_eval <- sqrt(rowSums(d2_eval^2))
    ES_eval <- mean(ES_1_eval)-mean(ES_2_eval)/2
    
    return(ES_eval)
    
  }
  
  
  #Calculating Energy score for predicive densities
  
  ES_reconciled_meth1_Opt.W_1_250[j] <- Energy_score(Reconciled_meth1_Opt.W_future_paths, Rel = Testing_eval,
                                               B = B, n = n)
  ES_reconciled_meth3_Opt.P_1_250[j] <- Energy_score(Reconciled_meth3_Opt.P_future_paths, Rel = Testing_eval,
                                               B = B, n = n)
  ES_reconciled_MinT.shr_1_250[j] <- Energy_score(Reconciled_MinT.shr_future_paths, Rel = Testing_eval,
                                            B = B, n = n)
  ES_reconciled_MinT.wls_1_250[j] <- Energy_score(Reconciled_MinT.wls_future_paths, Rel = Testing_eval,
                                                  B = B, n = n)
  ES_reconciled_OLS_1_250[j] <- Energy_score(Reconciled_OLS_future_paths, Rel = Testing_eval, B = B, n = n)
  ES_unrecon_1_250[j] <- Energy_score(Future_paths_eval, Rel = Testing_eval, B = B, n = n)
  
  
  Variogram_score<-function(Data, rel)
  {
    
    Y_tilde_diff <- t(apply(Data, 1, function(x)
      (abs(outer(x,x,"-")[lower.tri(outer(x,x,"-"))]))^(0.5)))
    
    Y_tilde_diff_mean <- apply(Y_tilde_diff, 2, mean)
    
    z <- abs(outer(rel,rel,'-'))
    
    y_diff <- z[lower.tri(z)]
    y_diff <- y_diff^0.5
    
    C<-(y_diff - Y_tilde_diff_mean)^2
    
    sum(C)
  }
  
  VS_reconciled_meth1_Opt.W_1_250[j] <- Variogram_score(Reconciled_meth1_Opt.W_future_paths, rel = Testing_eval)
  VS_reconciled_meth3_Opt.P_1_250[j] <- Variogram_score(Reconciled_meth3_Opt.P_future_paths, rel = Testing_eval)
  VS_reconciled_MinT.shr_1_250[j] <- Variogram_score(Reconciled_MinT.shr_future_paths, rel = Testing_eval)
  VS_reconciled_MinT.wls_1_250[j] <- Variogram_score(Reconciled_MinT.wls_future_paths, rel = Testing_eval)
  VS_reconciled_OLS_1_250[j] <- Variogram_score(Reconciled_OLS_future_paths, rel = Testing_eval)
  VS_unrecon_1_250[j] <- Variogram_score(Future_paths_eval, rel = Testing_eval)
  
  
  
}




end <- Sys.time()



end-start



ES_1_250 <- data.frame(ES_reconciled_meth1_Opt.W_1_250, ES_reconciled_meth3_Opt.P_1_250,
                        ES_reconciled_MinT.shr_1_250, ES_reconciled_MinT.wls_1_250,
                        ES_reconciled_OLS_1_250, ES_unrecon_1_250)
write.csv(x=ES_1_250, file = "Results/ES_1-250.csv")

VS_1_250 <- data.frame(VS_reconciled_meth1_Opt.W_1_250, VS_reconciled_meth3_Opt.P_1_250,
                       VS_reconciled_MinT.shr_1_250, VS_reconciled_MinT.wls_1_250, VS_reconciled_OLS_1_250,
                       VS_unrecon_1_250)
write.csv(x=VS_1_250, file = "Results/VS_1-250.csv")

