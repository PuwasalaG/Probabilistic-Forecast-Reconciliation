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

start<-Sys.time()

C <- 1
N <- 2500
L <- 500
r <- 500
m <- 2
B <- 2500
H <- 1

ES_reconciled_meth1_Opt.W <- numeric()
ES_reconciled_meth2_Opt.chol <- numeric()
ES_reconciled_meth2b_Opt.chol.Rest <- numeric()
ES_reconciled_meth3_Opt.P <- numeric()
ES_reconciled_MinT.shr <- numeric()
ES_reconciled_MinT.wls <- numeric()
ES_reconciled_OLS <- numeric()
ES_unrecon <- numeric()


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

for (j in 1:C)

{
  
  AllTS_a <- AllTS[j : (1100+j),]
  
  Testing <- matrix(NA,r,n)
  Unreconciled_future_paths <- list()
  #MinT_P_Training <- list()
  
  Start_future.path_train <- Sys.time()
  for (q in 1:r)
  {
    Training <- AllTS_a[q:(L+q-1), ]
    Testing[q,] <- AllTS_a[(L+q), ]
    
    #Model fitting, forecasting and obtaining residuals
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
    fit_training <- list()
    Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
    
    
    for(i in 1:n)
    {
      fit_training[[i]] <- auto.arima(Training[,i])
      Base_forecasts_training[,i] <- forecast(fit_training[[i]], h=1)$mean
      Residuals_all_training[,i] <- Training[,i] - fitted(fit_training[[i]])
    }
    

    Future_paths_training <- matrix(0, nrow = B, ncol = n)
    
    for(k in 1:B)
    {
      Index <- base::sample(c(1:nrow(Residuals_all_training)), size = H, replace = TRUE)
      
      Future_paths_training[k, ] <- (Base_forecasts_training) + Residuals_all_training[Index,]
      
    }
    
    Unreconciled_future_paths[[q]] <- Future_paths_training
    
  }
  End_future.path_train <- Sys.time()
  
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
  
  
  ###Method 2: Estimating through the reparameterisation using cholesky decomposition
  
  Energy_score_method2 <- function(Vec_cholesky,  Future_paths, Test, n1, B1, r1, S1)
  {
    R <- matrix(0, n1, n1)
    gdata::upperTriangle(R, diag = TRUE, byrow=TRUE) <- Vec_cholesky
    
    Inv_W <- t(R)%*%R
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
  
  
  
  #Calculating initial cholesky matrix for the optimization problem
  
  Int_par_chol <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)
  
  start.opt.chol <- Sys.time()
  OPT_method2 <- optim(Int_par_chol, Energy_score_method2, method = "BFGS", 
                       Future_paths = Unreconciled_future_paths, Test = Testing, n1 = n, B1 = B, r1 = r, 
                       S1=S)
  end.opt.chol <- Sys.time()
  
  Opt_Vec_cholesky <- OPT_method2$par
  
  OPT_R <- matrix(0, n, n)
  gdata::upperTriangle(OPT_R, diag = TRUE, byrow=TRUE) <- Opt_Vec_cholesky
  
  Opt_W_meth2 <- t(OPT_R) %*% OPT_R
  #Opt_Inv_W_meth2 <- solve(Opt_W_meth2) 
  
  Method2_Opt_P <- solve(t(S) %*% Opt_W_meth2 %*% S) %*% t(S) %*% Opt_W_meth2
  

  ###Method 2b: Estimating through the reparameterisation using cholesky decomposition - restricted for rotations
  
  Energy_score_method2b <- function(Vec_cholesky, Future_paths, Test, n1, B1, r1, S1)
  {
    Vec_cholesky <- c(1, Vec_cholesky)
    R <- matrix(0, n1, n1)
    gdata::upperTriangle(R, diag = TRUE, byrow=TRUE) <- Vec_cholesky
    #diag(R) <- c(rep(1, n1))
    
    Inv_W <- t(R)%*%R
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
  
  
  #Calculating initial cholesky matrix for the optimization problem
  
  #Int_par <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)
  Int_par_chol.Restrict <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)[-1]
  
  
  start.opt.chol.Rest <- Sys.time()
  OPT_method2b <- optim(Int_par_chol.Restrict, Energy_score_method2b, method = "BFGS", Future_paths = Unreconciled_future_paths,
               Test = Testing,  n1 = n, B1 = B, r1 = r, S1=S)
  end.opt.chol.Rest <- Sys.time()
  
  Opt_Vec_chol.Restrict <- c(1, OPT_method2b$par)

  OPT_chol.Restrict_R <- matrix(0, n, n)
  gdata::upperTriangle(OPT_chol.Restrict_R, diag = TRUE, byrow=TRUE) <- Opt_Vec_chol.Restrict

  Opt_W_meth2b <- t(OPT_chol.Restrict_R) %*% OPT_chol.Restrict_R
  #Opt_Inv_W <- solve(Opt_W) 
  
  Method2b_Opt_P <- solve(t(S) %*% Opt_W_meth2b %*% S) %*% t(S) %*% Opt_W_meth2b
  
  
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
  
  #OLS P
  
  OLS_P <- solve(t(S)%*%S)%*%t(S)
  

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
  Reconciled_meth2_Opt.chol_future_paths <- t(S%*%Method2_Opt_P%*%t(Future_paths_eval))
  Reconciled_meth2b_Opt.chol.Rest_future_paths <- t(S%*%Method2b_Opt_P%*%t(Future_paths_eval))
  Reconciled_meth3_Opt.P_future_paths <- t(S%*%Method3_Opt_P%*%t(Future_paths_eval))
  Reconciled_MinT.shr_future_paths <- t(S%*%MinT_P.shr%*%t(Future_paths_eval))
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
  
  ES_reconciled_meth1_Opt.W[j] <- Energy_score(Reconciled_meth1_Opt.W_future_paths, Rel = Testing_eval,
                                               B = B, n = n)
  ES_reconciled_meth2_Opt.chol[j] <- Energy_score(Reconciled_meth2_Opt.chol_future_paths, Rel = Testing_eval,
                                                  B = B, n = n)
  ES_reconciled_meth2b_Opt.chol.Rest[j] <- Energy_score(Reconciled_meth2b_Opt.chol.Rest_future_paths, 
                                                        Rel = Testing_eval, B = B, n = n)
  ES_reconciled_meth3_Opt.P[j] <- Energy_score(Reconciled_meth3_Opt.P_future_paths, Rel = Testing_eval,
                                               B = B, n = n)
  ES_reconciled_MinT.shr[j] <- Energy_score(Reconciled_MinT.shr_future_paths, Rel = Testing_eval,
                                            B = B, n = n)
  ES_reconciled_OLS[j] <- Energy_score(Reconciled_OLS_future_paths, Rel = Testing_eval, B = B, n = n)
  ES_unrecon[j] <- Energy_score(Future_paths_eval, Rel = Testing_eval, B = B, n = n)


 
}

Mean_ES_reconciled_meth1_Opt.W <- mean(ES_reconciled_meth1_Opt.W)
Mean_ES_reconciled_meth2_Opt.chol <- mean(ES_reconciled_meth2_Opt.chol)
Mean_ES_reconciled_meth2b_Opt.chol.Restrict <- mean(ES_reconciled_meth2b_Opt.chol.Rest)
Mean_ES_reconciled_meth3_Opt.P <- mean(ES_reconciled_meth3_Opt.P)
Mean_ES_reconciled_MinT.shr <- mean(ES_reconciled_MinT.shr)
Mean_ES_reconciled_OLS <- mean(ES_reconciled_OLS)
Mean_ES_unrecon <- mean(ES_unrecon)

end <- Sys.time()



end-start

Method <- c("Method1_Opt.W", "Method2_Opt.chol", "Method2b_Opt.chol.Restricted", "Method3_Opt.P", "MinT.shr", "OLS", "Unreconciled")
h1 <- c(Mean_ES_reconciled_meth1_Opt.W, Mean_ES_reconciled_meth2_Opt.chol, Mean_ES_reconciled_meth2b_Opt.chol.Restrict,
        Mean_ES_reconciled_meth3_Opt.P, Mean_ES_reconciled_MinT.shr, Mean_ES_reconciled_OLS,
        Mean_ES_unrecon)
Comparison <- data.frame(Method,h1)

