#Bootstrap coherent future paths obtained using numerical optimization method.
#Method 1: Estimating W
#Method 2: Estimating through the reparameterisation using cholesky decomposition
#Method 3: Estimating P directly, imposing the constraint PS=I


library(forecast)
library(Matrix)
library(hts)
library(miscTools)
library(MASS)
library(hydroGOF)
library(numDeriv)
library(psych)
library(Rsolnp)
library(VineCopula)
library(copula)
library(tidyverse)
library(plyr)
library(purrr)
library(furrr)
library(popbio)
library(gdata)


source("Optim-functions.R")
source("Score-functions.R")


##This hierarchical structure contains 2 bottom level series with one disaggregation level

set.seed(1989)

start<-Sys.time()

Bottom_level <- read.csv("Bottom_level.csv")[,-1]

# C <- 1                  #C - Length of the outer rolling window
N <- nrow(Bottom_level)   #N - Length of the original data dagenerated
L <- 500                  #L - Length of the inner rolling window 
r <- 100                  #r - Length of the training set using to learn G matrix
m <- 4                    #m - Number of bottom level series
B <- 1000                 #B - The size of the future paths generated 
H <- 3                    #H - Forecast horizons


#Generating the hierarchy

Hierarchy <- suppressMessages(hts(Bottom_level, list(2, c(2,2))))
AllTS <- allts(Hierarchy)
n <- ncol(AllTS)  #n - Total number of series

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

#To store the score results

DF_MultiV <- tibble("Replication" = integer(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "Energy score" = numeric(),
                    "Variogram score" = numeric())



#Simulation starts from here.
Start_sim <- Sys.time()

for (j in 601:700)#1:250
  
{
  DF_MultiV <- DF_MultiV %>% add_row("Replication" = j)
  
  AllTS_a <- AllTS[j : (602+j),]
  
  Testing_h1 <- matrix(0,r,n)
  Testing_h2 <- matrix(0,r,n)
  Testing_h3 <- matrix(0,r,n)
  
  
  Unreconciled_future_paths_h1 <- list()
  Unreconciled_future_paths_h2 <- list()
  Unreconciled_future_paths_h3 <- list()
  
  # #Fitting arima models for the 1st training window and using these models to get future paths for 
  # #following training sets
  # 
  # Training <- AllTS_a[1:(L+r+2), ]
  # 
  # Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
  # fit_training <- list()
  # #Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
  # 
  # 
  # for(i in 1:n) {
  #   fit_training[[i]] <- auto.arima(Training[,i])
  #   #Base_forecasts_training[,i] <- forecast(fit_training[[i]], h=1)$mean
  #   Residuals_all_training[,i] <- Training[,i] - fitted(fit_training[[i]])
  # }
  
  #A function to obtain future paths
  FP_train_func <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the
    #k^th future path for H forecast horizons for all n series. In the returning matrix, 
    #rows represents forecast horizons columns represents the series
    
    fit_training <- fit
    Residuals_all_training <- Resid
    Index <- Index
    Index_seq <- Index_seq
    H <- H
    n <- n
    
    Innov <- as.list(as.data.frame(Residuals_all_training[Index_seq[k,],]))
    # mylist <- list(object = fit_training, innov = Innov)
    # 
    # pmap_dfc(mylist, simulate, future = TRUE, nsim = H) %>% as.matrix() %>% return()
    
    return(mapply(simulate, fit_training, future = TRUE, nsim = H, innov = Innov))
    
  }
  
  Start_train <-  Sys.time()
  for (q in 1:r)
  {
    Training <- AllTS_a[q:(L+q-1), ]
    Testing_h1[q,] <- AllTS_a[(L+q), ]
    Testing_h2[q,] <- AllTS_a[(L+q+1), ]
    Testing_h3[q,] <- AllTS_a[(L+q+2), ]
    
    #Model fitting, forecasting and obtaining residuals
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
    fit_training <- list()
    #Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
    
    
    for(i in 1:n) {
      fit_training[[i]] <- auto.arima(Training[,i])
      #Base_forecasts_training[,i] <- forecast(fit_training[[i]], h=1)$mean
      Residuals_all_training[,i] <- Training[,i] - fitted(fit_training[[i]])
    }
    
    
    Index <- base::sample(c(1:(nrow(Residuals_all_training)-(H-1))), size = B, 
                          replace = TRUE)
    Index_seq <- matrix(0, B, H)
    
    for (k in 1:B) {
      
      Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
      
    }
    
    
    
    # Start_FP <- Sys.time()
    # future_paths <-  lapply(c(1:B), FP_train_func, fit = fit_training,
    #                         Resid = Residuals_all_training, Index = Index,
    #                         Index_seq = Index_seq, H=1, n=n)
    # End_FP <- Sys.time()
    
    Start_FP <- Sys.time()
    future_paths <-  furrr::future_map(c(1:B), FP_train_func, fit = fit_training,
                                       Resid = Residuals_all_training, Index = Index,
                                       Index_seq = Index_seq, H=H, n=n)
    End_FP <- Sys.time()
    
    Unreconciled_future_paths_h1[[q]] <- laply(future_paths, function(y) y[1,])
    Unreconciled_future_paths_h2[[q]] <- laply(future_paths, function(y) y[2,])
    Unreconciled_future_paths_h3[[q]] <- laply(future_paths, function(y) y[3,])
    
    
    
    # # Start3 <- Sys.time()
    # Unrecon_future_paths_h1<-matrix(0, nrow = B, ncol = n)
    # Unrecon_future_paths_h2<-matrix(0, nrow = B, ncol = n)
    # Unrecon_future_paths_h3<-matrix(0, nrow = B, ncol = n)
    # # Unrecon_future_paths_h4<-matrix(0, nrow = B, ncol = n)
    # # Unrecon_future_paths_h5<-matrix(0, nrow = B, ncol = n)
    # 
    # 
    # Index <- base::sample(c(1:(nrow(Residuals_all)-(H-1))), size = B , replace = TRUE)
    # Index_seq <- matrix(0, B, H)
    # 
    # Future_paths <- matrix(0, nrow = H, ncol = n)
    # 
    # for(k in 1:B)
    # {
    #   Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
    #   
    #   for(i in 1:n)
    #   {
    #     Future_paths[,i] <- simulate(fit_training[[i]], nsim = H, future = TRUE,
    #                                  innov = Residuals_all_training[Index_seq[k,],i])
    #   }
    #   Unrecon_future_paths_h1[k,]<-Future_paths[1,]
    #   Unrecon_future_paths_h2[k,]<-Future_paths[2,]
    #   Unrecon_future_paths_h3[k,]<-Future_paths[3,]
    #   # Unrecon_future_paths_h4[k,]<-Future_paths[4,]
    #   # Unrecon_future_paths_h5[k,]<-Future_paths[5,]
    #   # 
    # }
    # 
    # # End3 <- Sys.time()
    
  }
  
  End_train <-  Sys.time()
  
  FP_training_list <- list(Unreconciled_future_paths_h1, Unreconciled_future_paths_h2, 
                           Unreconciled_future_paths_h3)
  
  Testing <- list(Testing_h1, Testing_h2, Testing_h3)
  
  
  ###Method 1: Estimating W_h (W_1, W_2, W_3)
  
  
  #Calculating initial Inv.weight matrix for the optimization problem
  
  Int_par_W <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  Method1_Opt_G <- list() #Stores Optimal G from method 1 for H forecast horizons
  
  for (h in 1:H) {
    
    Opt_Vec_W <- optim(Int_par_W, Energy_score_method1, gr = Grad_method1, method = "BFGS",
                       Future_paths = FP_training_list[[h]], Test = Testing[[h]], n1 = n, B1 = B, 
                       r1 = r, S1=S)$par
    Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
    Method1_Opt_G[[h]] <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
    
  }
  
  
  ###Method 2: Estimating through the reparameterisation using cholesky decomposition
  
  #Calculating initial cholesky matrix for the optimization problem
  
  Int_par_chol <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)
  
  Method2a_Opt_G <- list() #Stores Optimal G from method 2a for H forecast horizons
  
  for (h in 1:H) {
    
    Opt_Vec_cholesky <- optim(Int_par_chol, Energy_score_method2, method = "BFGS", 
                              Future_paths = FP_training_list[[h]], Test = Testing[[h]], n1 = n, B1 = B, r1 = r, 
                              S1=S)$par
    OPT_R <- matrix(0, n, n)
    gdata::upperTriangle(OPT_R, diag = TRUE, byrow=TRUE) <- Opt_Vec_cholesky
    
    Opt_W_meth2 <- t(OPT_R) %*% OPT_R
    #Opt_Inv_W_meth2 <- solve(Opt_W_meth2) 
    
    Method2a_Opt_G[[h]] <- solve(t(S) %*% Opt_W_meth2 %*% S) %*% t(S) %*% Opt_W_meth2
    
    
  }
  
  
  
  # ###Method 2b: Estimating through the reparameterisation using cholesky decomposition - restricted for rotations
  #Calculating initial cholesky matrix for the optimization problem
  
  #Int_par <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)
  Int_par_chol.Restrict <- gdata::upperTriangle(chol(diag(1,n,n)), diag = TRUE, byrow = TRUE)[-1]
  
  Method2b_Opt_G <- list() #Stores Optimal G from method 2b for H forecast horizons
  
  for (h in 1:H) {
    
    OPT_method2b <- optim(Int_par_chol.Restrict, Energy_score_method2b, method = "BFGS", 
                          Future_paths = FP_training_list[[h]], Test = Testing[[h]],  n1 = n, B1 = B, 
                          r1 = r, S1=S)
    Opt_Vec_chol.Restrict <- c(1, OPT_method2b$par)
    
    OPT_chol.Restrict_R <- matrix(0, n, n)
    gdata::upperTriangle(OPT_chol.Restrict_R, diag = TRUE, byrow=TRUE) <- Opt_Vec_chol.Restrict
    
    Opt_W_meth2b <- t(OPT_chol.Restrict_R) %*% OPT_chol.Restrict_R
    #Opt_Inv_W <- solve(Opt_W) 
    
    Method2b_Opt_G[[h]] <- solve(t(S) %*% Opt_W_meth2b %*% S) %*% t(S) %*% Opt_W_meth2b
    
    
  }
  
  
  
  #Method 3: Estimating P directly, imposing the constraint PS=I
  
  #OLS P
  
  OLS_P <- solve(t(S)%*%S)%*%t(S)
  #Constraint vector (RHS)
  Eq <- c(rep(diag(1, m, m)))
  
  Int_par_P <- as.vector(t(OLS_P))
  
  Method3_Opt_G <- list() #Stores Optimal G from method 3 for H forecast horizons
  
  for (h in 1:H) {
    
    OPT_method3 <- solnp(Int_par_P, Energy_score_method3, eqfun = eqn1, eqB = Eq, 
                         Future_paths = FP_training_list[[h]], 
                         Test = Testing[[h]], m1 = m,  n1 = n, B1 = B, r1 = r, S1=S, control = list(trace = 0))
    
    Method3_Opt_G[[h]] <- matrix(OPT_method3$pars, nrow = m, ncol = n, byrow = TRUE)
    
    
  }
  
  
  ##Evaluation
  
  Training_eval <- AllTS_a[(r+1):(L+r),]
  Testing_eval <- AllTS_a[(L+r+1):(L+r+H),]
  
  
  #Model fitting, forecasting and obtaining residuals
  
  Residuals_all_eval <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  fit_eval <- list()
  #Base_forecasts_eval <- matrix(NA, nrow = H, ncol = n)
  
  
  for(i in 1:n)
  {
    fit_eval[[i]] <- auto.arima(Training_eval[,i])
    #Base_forecasts_eval[,i] <- forecast(fit_eval[[i]], h=1)$mean
    Residuals_all_eval[,i] <- Training_eval[,i] - fitted(fit_eval[[i]])
  }
  
  #Obtaining the shrinkage estimator for the covariance matrix of the based forecast errors
  
  targ1 <- lowerD(Residuals_all_eval)
  shrink1 <- shrink.estim(Residuals_all_eval,targ1)
  W.h_eval <- shrink1[[1]]
  
  #MinT_G.shr matrix
  
  Inv_W.h <- solve(W.h_eval)
  MinT_G.shr <- solve(t(S)%*%Inv_W.h%*%S)%*%t(S)%*%Inv_W.h
  
  #WLS P 
  n1 <- nrow(Residuals_all_eval)
  Sigma_sample <- crossprod(Residuals_all_eval)/n1
  Sigma_WLS <- diag(diag(Sigma_sample),n,n)
  
  Inv_WLS <- solve(Sigma_WLS)
  WLS_P <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS
  
  
  #OLS P (Obtained under Method 3 as initial parameters)
  
  #Generating future paths for 1-step ahead forecast horizon using bootsrapped errors
  
  Future_paths_eval <- list()
  
  Index_eval <- base::sample(c(1:(nrow(Residuals_all_eval)-(H-1))), size = B , replace = TRUE)
  Index_seq <- matrix(0, B, H)
  
  for (k in 1:B) {
    
    Index_seq[k,] <- seq(from = Index_eval[k], to = (Index_eval[k]+H-1), by = 1)
    
  }
  
  FP_eval_func <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the k^th future
    #path for H forecast horizons for all n series. In the returning matrix, rows represents forecast horizons
    #columns represents the series
    
    fit_eval <- fit
    Residuals_all_eval <- Resid
    Index_eval <- Index
    Index_seq <- Index_seq
    H <- H
    n <- n
    
    Innov <- as.list(as.data.frame(Residuals_all_eval[Index_seq[k,],]))
    
    return(mapply(simulate, fit_eval, future = TRUE, nsim = H, innov = Innov))
    
  }
  
  future_paths <-  lapply(c(1:B), FP_eval_func, fit = fit_eval, 
                          Resid = Residuals_all_eval, Index = Index_eval, 
                          Index_seq = Index_seq, H=H, n=n)
  
  for (h in 1:H) {
    
    Future_paths_eval[[h]] <- plyr::laply(future_paths, function(y) y[h,])
    
  }
  
  #To store reconciled future paths - each element in the list corresponds to H forecast horizon
  
  Reconciled_meth1_Opt.W_future_paths <- list(H)
  Reconciled_meth2a_Opt.chol_future_paths <- list(H)
  Reconciled_meth2b_Opt.chol.Rest_future_paths <- list(H)
  Reconciled_meth3_Opt.G_future_paths <- list(H)
  Reconciled_MinT.shr_future_paths <- list(H)
  Reconciled_WLS_future_paths <- list(H)
  Reconciled_OLS_future_paths <- list(H)
  
  #To store scores
  
  ES_reconciled_meth1_Opt.W <- numeric(H)
  ES_reconciled_meth2a_Opt.chol <- numeric(H)
  ES_reconciled_meth2b_Opt.chol.Rest <- numeric(H)
  ES_reconciled_meth3_Opt.G <- numeric(H)
  ES_reconciled_MinT.shr <- numeric(H)
  ES_reconciled_WLS <- numeric(H)
  ES_reconciled_OLS <- numeric(H)
  ES_unrecon <- numeric(H)
  
  VS_reconciled_meth1_Opt.W <- numeric(H)
  VS_reconciled_meth2a_Opt.chol <- numeric(H)
  VS_reconciled_meth2b_Opt.chol.Rest <- numeric(H)
  VS_reconciled_meth3_Opt.G <- numeric(H)
  VS_reconciled_MinT.shr <- numeric(H)
  VS_reconciled_WLS <- numeric(H)
  VS_reconciled_OLS <- numeric(H)
  VS_unrecon <- numeric(H)
  
  
  #Following loop will calculate the reconciled future paths for each forecast horizon and then calculate the
  #scores
  
  for (h in 1:H) {
    
    #Obtaining reconciled future paths using Optimal_G, MinT.shrink G, OLS and WLS
    
    Reconciled_meth1_Opt.W_future_paths[[h]] <- t(S %*% Method1_Opt_G[[h]] %*% t(Future_paths_eval[[h]]))
    Reconciled_meth2a_Opt.chol_future_paths[[h]] <- t(S %*% Method2a_Opt_G[[h]] %*% t(Future_paths_eval[[h]]))
    Reconciled_meth2b_Opt.chol.Rest_future_paths[[h]] <- t(S %*% Method2b_Opt_G[[h]] %*% t(Future_paths_eval[[h]]))
    Reconciled_meth3_Opt.G_future_paths[[h]] <- t(S %*% Method3_Opt_G[[h]] %*% t(Future_paths_eval[[h]]))
    Reconciled_MinT.shr_future_paths[[h]] <- t(S %*% MinT_G.shr %*% t(Future_paths_eval[[h]]))
    Reconciled_WLS_future_paths[[h]] <- t(S %*% WLS_P %*% t(Future_paths_eval[[h]]))
    Reconciled_OLS_future_paths[[h]] <- t(S %*% OLS_P %*% t(Future_paths_eval[[h]]))
    
    # Calculating scores for evaluation
    
    #Calculating Energy score for predicive densities
    
    ES_reconciled_meth1_Opt.W[h] <- Energy_score(Reconciled_meth1_Opt.W_future_paths[[h]], Rel = Testing_eval[h,])
    ES_reconciled_meth2a_Opt.chol[h] <- Energy_score(Reconciled_meth2a_Opt.chol_future_paths[[h]], 
                                                     Rel = Testing_eval[h,])
    ES_reconciled_meth2b_Opt.chol.Rest[h] <- Energy_score(Reconciled_meth2b_Opt.chol.Rest_future_paths[[h]], 
                                                          Rel = Testing_eval[h,])
    ES_reconciled_meth3_Opt.G[h] <- Energy_score(Reconciled_meth3_Opt.G_future_paths[[h]], Rel = Testing_eval[h,])
    ES_reconciled_MinT.shr[h] <- Energy_score(Reconciled_MinT.shr_future_paths[[h]], Rel = Testing_eval[h,])
    ES_reconciled_WLS[h] <- Energy_score(Reconciled_WLS_future_paths[[h]], Rel = Testing_eval[h,])
    ES_reconciled_OLS[h] <- Energy_score(Reconciled_OLS_future_paths[[h]], Rel = Testing_eval[h,])
    ES_unrecon[h] <- Energy_score(Future_paths_eval[[h]], Rel = Testing_eval[h,])
    
    
    #Calculating Variogram score for predicive densities
    
    VS_reconciled_meth1_Opt.W[h] <- Variogram_score(Reconciled_meth1_Opt.W_future_paths[[h]], rel = Testing_eval[h,])
    VS_reconciled_meth2a_Opt.chol[h] <- Variogram_score(Reconciled_meth2a_Opt.chol_future_paths[[h]], 
                                                        rel = Testing_eval[h,])
    VS_reconciled_meth2b_Opt.chol.Rest[h] <- Variogram_score(Reconciled_meth2b_Opt.chol.Rest_future_paths[[h]], 
                                                             rel = Testing_eval[h,])
    VS_reconciled_meth3_Opt.G[h] <- Variogram_score(Reconciled_meth3_Opt.G_future_paths[[h]], rel = Testing_eval[h,])
    VS_reconciled_MinT.shr[h] <- Variogram_score(Reconciled_MinT.shr_future_paths[[h]], rel = Testing_eval[h,])
    VS_reconciled_WLS[h] <- Variogram_score(Reconciled_WLS_future_paths[[h]], rel = Testing_eval[h,])
    VS_reconciled_OLS[h] <- Variogram_score(Reconciled_OLS_future_paths[[h]], rel = Testing_eval[h,])
    VS_unrecon[h] <- Variogram_score(Future_paths_eval[[h]], rel = Testing_eval[h,])
    
    
    
  }
  
  DF_MultiV %>% filter(`Replication`==j) %>% dplyr::select("Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: H), "Energy score" = ES_unrecon, 
        "Variogram score" = VS_unrecon) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Opt-G-Method-1", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_meth1_Opt.W, 
        "Variogram score" = VS_reconciled_meth1_Opt.W) -> DF_Opt_G_Meth1
  DF_Opt_G_Meth1[names(DF_MultiV)] -> DF_Opt_G_Meth1
  DF_MultiV <- rbind(DF_MultiV, DF_Opt_G_Meth1)
  
  cbind(Fltr, "R-method" = "Opt-G-Method-2a", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_meth2a_Opt.chol, 
        "Variogram score" = VS_reconciled_meth2a_Opt.chol) -> DF_Opt_G_Meth2a
  DF_Opt_G_Meth2a[names(DF_MultiV)] -> DF_Opt_G_Meth2a
  DF_MultiV <- rbind(DF_MultiV, DF_Opt_G_Meth2a)
  
  cbind(Fltr, "R-method" = "Opt-G-Method-2b", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_meth2b_Opt.chol.Rest, 
        "Variogram score" = VS_reconciled_meth2b_Opt.chol.Rest) -> DF_Opt_G_Meth2b
  DF_Opt_G_Meth2b[names(DF_MultiV)] -> DF_Opt_G_Meth2b
  DF_MultiV <- rbind(DF_MultiV, DF_Opt_G_Meth2b)
  
  cbind(Fltr, "R-method" = "Opt-G-Method-3", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_meth3_Opt.G, 
        "Variogram score" = VS_reconciled_meth3_Opt.G) -> DF_Opt_G_Meth3
  DF_Opt_G_Meth3[names(DF_MultiV)] -> DF_Opt_G_Meth3
  DF_MultiV <- rbind(DF_MultiV, DF_Opt_G_Meth3)
  
  cbind(Fltr, "R-method" = "MinT(Shrink)", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_MinT.shr, 
        "Variogram score" = VS_reconciled_MinT.shr) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_WLS, 
        "Variogram score" = VS_reconciled_WLS) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: H), "Energy score" = ES_reconciled_OLS, 
        "Variogram score" = VS_reconciled_WLS) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  
}

End_sim <- Sys.time()

DF_MultiV[complete.cases(DF_MultiV[ , "R-method"]),] -> DF_MultiV

write.csv(x=DF_MultiV, file = "Results/DF_MultiV_601-700.csv")
save.image("Results/Hierarchy-2_GaussianDGP_601_700.Rdata")
#View(DF_MultiV)


# DF_MultiV %>% dplyr::select(-"Replication") -> DF_MultScores
# 
# DF_MultScores %>% group_by(`Forecast Horizon`, `R-method`) %>% 
#  dplyr::summarise(E.ES = mean(`Energy score`), 
#                   E.VS = mean(`Variogram score`)) -> DF_MultScores


