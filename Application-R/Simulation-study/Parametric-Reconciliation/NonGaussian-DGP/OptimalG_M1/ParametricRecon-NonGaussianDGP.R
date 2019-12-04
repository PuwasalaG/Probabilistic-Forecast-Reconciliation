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
library(scoringRules)


source("Optim-functions.R")
source("Score-functions.R")


##This hierarchical structure contains 2 bottom level series with one disaggregation level

set.seed(1989)

start <- Sys.time()

Bottom_level <- read.csv("Bottom_level.csv")[,-1]

# C <- 1                  #C - Length of the outer rolling window
N <- nrow(Bottom_level)   #N - Length of the original data dagenerated
L <- 500                  #L - Length of the inner rolling window 
r <- 100                  #r - Length of the training set using to learn G matrix
m <- 4                    #m - Number of bottom level series
B <- 2500                 #B - The size of the future paths generated 
H <- 3                    #H - Forecast horizons


#Generating the hierarchy

Hierarchy <- suppressMessages(hts(Bottom_level, list(2, c(2,2))))
AllTS <- allts(Hierarchy)
AllTS <- AllTS[-(1:500),] # To avoid impact from initial values

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

#This function is used to generate random numbers from a degenerate Gaussian distributions
rnorm_degenerate <- function(mu, Sigma, n, k)
{
  Sigma <- matrix(Sigma, n, n)
  SVD <- svd((Sigma + t(Sigma))/2)
  
  SVD$d <- abs(zapsmall(SVD$d))
  U <- SVD$u
  D <- diag(sqrt(SVD$d))
  m1 <- sum(SVD$d > 0)
  
  X <- mvtnorm::rmvnorm(k, mean = rep(0, m1), diag(1,m1,m1))
  X <- cbind(X, matrix(0, nrow = k, ncol = n-m1))
  
  Mu <- matrix(rep(mu, k), k, n, byrow = TRUE)
  
  Y <- t(U %*% D %*% t(X)) + Mu
  
  return(Y)
  
}


#To store the score results

#All the forecasts and related information are stored in the DF dataframe

DF_MultiV_Full <- tibble("F-method" = character(),
                         "R-method" = character(),
                         "Forecast Horizon" = integer(),
                         "Energy score" = numeric(),
                         "Variogram score" = numeric(),
                         "Replication" = integer())


DF_MultiV_Bot <- tibble("F-method" = character(),
                        "R-method" = character(),
                        "Forecast Horizon" = integer(),
                        "Energy score" = numeric(),
                        "Variogram score" = numeric(),
                        "Log score" = numeric(),
                        "Replication" = integer())

DF_UniV <- tibble("F-method" = character(),
                  "R-method" = character(),
                  "Series" = character(),
                  "Forecast Horizon" = integer(),
                  "CRPS" = numeric(),
                  "LS" = numeric(),
                  "Replication" = integer())


#Simulation starts from here.
Start_sim <- Sys.time()

for (j in 1:1000)#1:250
  
{
    
  AllTS_a <- AllTS[j : (602+j),]
  
  Testing_h1 <- matrix(0,r,n)
  Testing_h2 <- matrix(0,r,n)
  Testing_h3 <- matrix(0,r,n)
  
  Base_mean_h1 <- matrix(0, r, n)
  Base_mean_h2 <- matrix(0, r, n)
  Base_mean_h3 <- matrix(0, r, n)
  
  Base_Sigma_Cov <- list()

  Start_train <-  Sys.time()
  for (q in 1:r)
  {
    Training <- AllTS_a[q:(L+q-1), ]
    Testing_h1[q,] <- AllTS_a[(L+q), ]
    Testing_h2[q,] <- AllTS_a[(L+q+1), ]
    Testing_h3[q,] <- AllTS_a[(L+q+2), ]
    
    ##-Model fitting, forecasting and obtaining residuals-##
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
    fit_training <- list()
    Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
    
    
    for(i in 1:n) {
      fit_training[[i]] <- auto.arima(Training[,i])
      Base_forecasts_training[,i] <- forecast(fit_training[[i]], h = H)$mean
      Residuals_all_training[,i] <- Training[,i] - fitted(fit_training[[i]])

    }
    
    targ <- diag(diag(var(Residuals_all_training)), n, n)
    shrink <- shrink.estim(Residuals_all_training,targ)
    Base_Sigma_Cov[[q]] <- shrink[[1]]
    
    Base_mean_h1[q,] <- Base_forecasts_training[1,]
    Base_mean_h2[q,] <- Base_forecasts_training[2,]
    Base_mean_h3[q,] <- Base_forecasts_training[3,]
    
  }
  
  End_train <-  Sys.time()
  
  # FP_training_list <- list(Unreconciled_future_paths_h1, Unreconciled_future_paths_h2, 
  #                          Unreconciled_future_paths_h3)
  
  Testing <- list(Testing_h1, Testing_h2, Testing_h3)
  Base_mean <- list(Base_mean_h1, Base_mean_h2, Base_mean_h3)
  
  
  ###Method 1: Estimating W_h (W_1, W_2, W_3)
  
  
  #Calculating initial Inv.weight matrix for the optimization problem
  
  Int_par_W <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  Opt_G <- list() #Stores Optimal G from method 1 for H forecast horizons
  
  Start_opt <- Sys.time()
  for (h in 1:H) {
    
    Opt_Vec_W <- optim(Int_par_W, Obj_func_LS, method = "BFGS", Sigma_hat = Base_Sigma_Cov, 
                       Mu_hat = Base_mean[[h]], Test = Testing[[h]], m1 = m, n1 = n,                       r1 = r, S1=S)$par
    Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
    Opt_G[[h]] <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
    
  }
  End_opt <- Sys.time()
  
  
  ##Evaluation
  
  Training_eval <- AllTS_a[(r+1):(L+r),]
  Testing_eval <- AllTS_a[(L+r+1):(L+r+H),]
  
  
  #Model fitting, forecasting and obtaining residuals
  
  Residuals_all_eval <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  fit_eval <- list()
  Base_forecasts_eval <- matrix(NA, nrow = H, ncol = n)
  
  
  for(i in 1:n)
  {
    fit_eval[[i]] <- auto.arima(Training_eval[,i])
    Base_forecasts_eval[,i] <- forecast(fit_eval[[i]], h=h)$mean
    Residuals_all_eval[,i] <- Training_eval[,i] - fitted(fit_eval[[i]])
  }
  
  #Obtaining the shrinkage estimator for the covariance matrix of the based forecast errors
  
  targ1 <- diag(diag(var(Residuals_all_eval)), n, n)
  shrink1 <- shrink.estim(Residuals_all_eval,targ1)
  W.h_eval <- shrink1[[1]]
  Sigma_shr <- W.h_eval
  
  #MinT_G.shr matrix
  
  Inv_W.h <- solve(W.h_eval)
  MinT.Shr_G <- solve(t(S)%*%Inv_W.h%*%S)%*%t(S)%*%Inv_W.h
  
  #MinT sample G
  Sam.cov_full <- var(Residuals_all_eval)
  Inv_Sam.cov <- solve(Sam.cov_full)
  
  MinT.Sam_G <- solve(t(S) %*% Inv_Sam.cov %*% S) %*% t(S) %*% Inv_Sam.cov
  
  
  #WLS G 
  n1 <- nrow(Residuals_all_eval)
  Sigma_sample <- crossprod(Residuals_all_eval)/n1
  Sigma_WLS <- diag(diag(Sigma_sample),n,n)
  
  Inv_WLS <- solve(Sigma_WLS)
  WLS_G <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS
  
  # OLS G
  OLS_G <- solve(t(S)%*%S)%*%t(S)
  
  #Bottom-up G 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #######################################  
  ###Mean forecast reconciliation###
  #######################################  
  
  #Reconciled point forecasts for the full hierarchy 
  Recon_PointF_full_BU <- t(S %*% BU_G %*% t(Base_forecasts_eval))
  Recon_PointF_full_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_eval))
  Recon_PointF_full_WLS <- t(S %*% WLS_G %*% t(Base_forecasts_eval))
  Recon_PointF_full_MinT.Shr <- t(S %*% MinT.Shr_G %*% t(Base_forecasts_eval))
  Recon_PointF_full_MinT.Sam <- t(S %*% MinT.Sam_G %*% t(Base_forecasts_eval))
  
  Recon_PointF_full_Opt <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_full_Opt[h,] <- S %*% Opt_G[[h]] %*% Base_forecasts_eval[h,]
  }
  
  
  #Reconciled bottom level point forecasts#
  Recon_PointF_Bot_BU <- t(BU_G %*% t(Base_forecasts_eval))
  Recon_PointF_Bot_OLS <- t(OLS_G %*% t(Base_forecasts_eval))
  Recon_PointF_Bot_WLS <- t(WLS_G %*% t(Base_forecasts_eval))
  Recon_PointF_Bot_MinT.Shr <- t(MinT.Shr_G %*% t(Base_forecasts_eval))
  Recon_PointF_Bot_MinT.Sam <- t(MinT.Sam_G %*% t(Base_forecasts_eval))
  
  Recon_PointF_Bot_Opt <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = m)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_Bot_Opt[h,] <- Opt_G[[h]] %*% Base_forecasts_eval[h,]
  }
  
  
  
  ############################################    
  ###Variance forecast reconciliation###
  ############################################  
  
  #Reconciled variance forecasts for the full hierarchy#
  Recon_Var.Cov_full_BU <- S %*% BU_G %*% Sigma_shr %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS <- S %*% OLS_G %*% Sigma_shr %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS <- S %*% WLS_G %*% Sigma_shr %*% t(S %*% WLS_G)
  Recon_Var.Cov_full_MinT.Shr <- S %*% MinT.Shr_G %*% Sigma_shr %*% t(S %*% MinT.Shr_G)
  Recon_Var.Cov_full_MinT.Sam <- S %*% MinT.Sam_G %*% Sigma_shr %*% t(S %*% MinT.Sam_G)
  
  Recon_Var.Cov_full_Opt <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_full_Opt[[h]] <- S %*% Opt_G[[h]] %*% Sigma_shr %*% t(S %*% Opt_G[[h]])
  }
  
  #Reconciled bottom level variance forecasts#
  Recon_Var.Cov_Bot_BU <- BU_G %*% Sigma_shr %*% t(BU_G)
  Recon_Var.Cov_Bot_OLS <- OLS_G %*% Sigma_shr %*% t(OLS_G)
  Recon_Var.Cov_Bot_WLS <- WLS_G %*% Sigma_shr %*% t(WLS_G)
  Recon_Var.Cov_Bot_MinT.Shr <- MinT.Shr_G %*% Sigma_shr %*% t(MinT.Shr_G)
  Recon_Var.Cov_Bot_MinT.Sam <- MinT.Sam_G %*% Sigma_shr %*% t(MinT.Sam_G)
  
  Recon_Var.Cov_Bot_Opt <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_Bot_Opt[[h]] <- Opt_G[[h]] %*% Sigma_shr %*% t(Opt_G[[h]])
  }
  
  ## Storing lists and matrices ##
  
  #List to store random samples from reconciled Gauss distribution of the full hierarchy
  X_full_BU <- list(min(H, nrow(Testing_eval)))
  X_full_OLS <- list(min(H, nrow(Testing_eval)))
  X_full_WLS <- list(min(H, nrow(Testing_eval)))
  X_full_MinT.Shr <- list(min(H, nrow(Testing_eval)))
  X_full_MinT.Sam <- list(min(H, nrow(Testing_eval)))
  X_full_Opt <- list(min(H, nrow(Testing_eval)))
  X_full_unrecon <- list(min(H, nrow(Testing_eval)))
  
  
  #List to store random samples from reconciled Gauss distribution of the bottom levels
  X_Bot_BU <- list(min(H, nrow(Testing_eval)))
  X_Bot_OLS <- list(min(H, nrow(Testing_eval)))
  X_Bot_WLS <- list(min(H, nrow(Testing_eval)))
  X_Bot_MinT.Shr <- list(min(H, nrow(Testing_eval)))
  X_Bot_MinT.Sam <- list(min(H, nrow(Testing_eval)))
  X_Bot_Opt <- list(min(H, nrow(Testing_eval)))
  X_Bot_unrecon <- list(min(H, nrow(Testing_eval)))
  
  #To store univariate scores
  CRPS_BU <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_OLS <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_WLS <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_MinT.Shr <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_MinT.Sam <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_Opt <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_Unrecon <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  
  LS_BU <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_OLS <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_WLS <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_MinT.Shr <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_MinT.Sam <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_Opt <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  LS_Unrecon <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  
  
  ###################################
  #Random sample generation#
  ################################### 
  
  #Obtaining random samples from the possible forecast Gaussian densities of the full 
  #hierarchy
  #(Since the Guassian distribution for the full hierarchy is degenerate, we use the 
  #rnorm_degenerate function to generate random samples)
  
  
  
  for (h in 1: min(H, nrow(Testing_eval))) {
    
    X_full_BU[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_BU[h,], 
                                       Sigma = Recon_Var.Cov_full_BU, k = B, n = n)
    
    X_full_OLS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_OLS[h,], 
                                        Sigma = Recon_Var.Cov_full_OLS, k = B, n = n)
    
    X_full_WLS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_WLS[h,], 
                                        Sigma = Recon_Var.Cov_full_WLS, k = B, n = n)
    
    X_full_MinT.Shr[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Shr[h,], 
                                             Sigma = Recon_Var.Cov_full_MinT.Shr, k = B, n = n)
    
    X_full_MinT.Sam[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Sam[h,], 
                                             Sigma = Recon_Var.Cov_full_MinT.Sam, k = B, n = n)
    
    X_full_Opt[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_Opt[h,], 
                                             Sigma = Recon_Var.Cov_full_Opt[[h]], k = B, n = n)
    
    X_full_unrecon[[h]] <- mvrnorm(n = B, mu = Base_forecasts_eval[h,], Sigma = Sigma_shr)
    
    
    
    
    #Obtaining random samples from the possible forecast Gaussian densities 
    #of bottom level of the hierarchy
    
    X_Bot_BU[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_BU[h,], 
                             Sigma = Recon_Var.Cov_Bot_BU)
    
    X_Bot_OLS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_OLS[h,], 
                              Sigma = Recon_Var.Cov_Bot_OLS)
    
    X_Bot_WLS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_WLS[h,], 
                              Sigma = Recon_Var.Cov_Bot_WLS)
    
    X_Bot_MinT.Shr[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_MinT.Shr[h,], 
                                   Sigma = Recon_Var.Cov_Bot_MinT.Shr)
    
    X_Bot_MinT.Sam[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_MinT.Sam[h,], 
                                   Sigma = Recon_Var.Cov_Bot_MinT.Sam)
    
    X_Bot_Opt[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_Opt[h,], 
                                   Sigma = Recon_Var.Cov_Bot_Opt[[h]])
    
    X_Bot_unrecon[[h]] <- mvrnorm(n = B, mu = Base_forecasts_eval[h, (n-m+1):n], 
                                  Sigma = Sigma_shr[(n-m+1):n, (n-m+1):n])
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_unrecon[[h]][,i],
                                       method = "edf")
      CRPS_BU[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_BU[[h]][,i],
                                  method = "edf")
      CRPS_OLS[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_OLS[[h]][,i],
                                   method = "edf")
      CRPS_WLS[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_WLS[[h]][,i],
                                   method = "edf")
      CRPS_MinT.Shr[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_MinT.Shr[[h]][,i],
                                        method = "edf")
      CRPS_MinT.Sam[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_MinT.Sam[[h]][,i],
                                        method = "edf")
      CRPS_Opt[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_Opt[[h]][,i],
                                        method = "edf")
      
      
      #Calculating LS for univariate predictive densities
      
      LS_Unrecon[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Base_forecasts_eval[h,i], 
                                sd = sqrt(Sigma_shr[i,i]),log = TRUE)
      LS_BU[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_BU[h,i], 
                           sd = sqrt(Recon_Var.Cov_full_BU[i,i]),log = TRUE)
      LS_OLS[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_OLS[h,i], 
                            sd = sqrt(Recon_Var.Cov_full_OLS[i,i]),log = TRUE)
      LS_WLS[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_WLS[h,i], 
                            sd = sqrt(Recon_Var.Cov_full_WLS[i,i]),log = TRUE)
      LS_MinT.Shr[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_MinT.Shr[h,i], 
                                 sd = sqrt(Recon_Var.Cov_full_MinT.Shr[i,i]),log = TRUE)
      LS_MinT.Sam[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_MinT.Sam[h,i], 
                                 sd = sqrt(Recon_Var.Cov_full_MinT.Sam[i,i]),log = TRUE)
      LS_Opt[h,i] <- -dnorm(as.numeric(Testing_eval[h,i]), mean = Recon_PointF_full_Opt[h,i], 
                                 sd = sqrt(Recon_Var.Cov_full_Opt[[h]][i,i]),log = TRUE)
      
      
    }
    
    DF_MultiV_Full <- DF_MultiV_Full %>% add_row("F-method" = "ARIMA",
                                                 "Replication" = j)
    
    DF_MultiV_Bot <- DF_MultiV_Bot %>% add_row("F-method" = "ARIMA",
                                               "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("Series" = rep(colnames(AllTS), each = min(H, nrow(Testing_eval))),
                                   "F-method" = "ARIMA",
                                   "Replication" = j)
    
    
    
  }
  
  Test.list_full <- split(Testing_eval[1:min(H, nrow(Testing_eval)),], 1:min(H, nrow(Testing_eval)))
  Test.list_Bot <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(n-m+1):n], 1:min(H, nrow(Testing_eval)))
  
  #Calculating Energy score for full predicive densities
  ES_full_BU <- mapply(Energy_score, Data = X_full_BU, Real = Test.list_full)
  ES_full_OLS <- mapply(Energy_score, X_full_OLS, Real = Test.list_full)
  ES_full_WLS <- mapply(Energy_score, X_full_WLS, Real = Test.list_full)
  ES_full_MinT.Shr <- mapply(Energy_score, X_full_MinT.Shr, Real = Test.list_full)
  ES_full_MinT.Sam <- mapply(Energy_score, X_full_MinT.Sam, Real = Test.list_full)
  ES_full_Opt <- mapply(Energy_score, X_full_Opt, Real = Test.list_full)
  ES_full_Unrecon <- mapply(Energy_score, X_full_unrecon, Real = Test.list_full)
  
  #Calculating Variogram score for full predicive densities
  VS_full_BU <- mapply(Variogram_score, Data = X_full_BU, Real = Test.list_full)
  VS_full_OLS <- mapply(Variogram_score, X_full_OLS, Real = Test.list_full)
  VS_full_WLS <- mapply(Variogram_score, X_full_WLS, Real = Test.list_full)
  VS_full_MinT.Shr <- mapply(Variogram_score, X_full_MinT.Shr, Real = Test.list_full)
  VS_full_MinT.Sam <- mapply(Variogram_score, X_full_MinT.Sam, Real = Test.list_full)
  VS_full_Opt <- mapply(Variogram_score, X_full_Opt, Real = Test.list_full)
  VS_full_Unrecon <- mapply(Variogram_score, X_full_unrecon, Real = Test.list_full)
  
  
  #Calculating Energy score for Bot predicive densities
  ES_Bot_BU <- mapply(Energy_score, Data = X_Bot_BU, Real = Test.list_Bot)
  ES_Bot_OLS <- mapply(Energy_score, X_Bot_OLS, Real = Test.list_Bot)
  ES_Bot_WLS <- mapply(Energy_score, X_Bot_WLS, Real = Test.list_Bot)
  ES_Bot_MinT.Shr <- mapply(Energy_score, X_Bot_MinT.Shr, Real = Test.list_Bot)
  ES_Bot_MinT.Sam <- mapply(Energy_score, X_Bot_MinT.Sam, Real = Test.list_Bot)
  ES_Bot_Opt <- mapply(Energy_score, X_Bot_Opt, Real = Test.list_Bot)
  ES_Bot_Unrecon <- mapply(Energy_score, X_Bot_unrecon, Real = Test.list_Bot)
  
  #Calculating Variogram score for Bot predicive densities
  VS_Bot_BU <- mapply(Variogram_score, Data = X_Bot_BU, Real = Test.list_Bot)
  VS_Bot_OLS <- mapply(Variogram_score, X_Bot_OLS, Real = Test.list_Bot)
  VS_Bot_WLS <- mapply(Variogram_score, X_Bot_WLS, Real = Test.list_Bot)
  VS_Bot_MinT.Shr <- mapply(Variogram_score, X_Bot_MinT.Shr, Real = Test.list_Bot)
  VS_Bot_MinT.Sam <- mapply(Variogram_score, X_Bot_MinT.Sam, Real = Test.list_Bot)
  VS_Bot_Opt <- mapply(Variogram_score, X_Bot_Opt, Real = Test.list_Bot)
  VS_Bot_Unrecon <- mapply(Variogram_score, X_Bot_unrecon, Real = Test.list_Bot)
  
  #Splitting Reconciled means and variances
  Recon_PointF_Bot_BU_list <- split(Recon_PointF_Bot_BU, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_Bot_OLS_list <- split(Recon_PointF_Bot_OLS, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_Bot_WLS_list <- split(Recon_PointF_Bot_WLS, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_Bot_MinT.Shr_list <- split(Recon_PointF_Bot_MinT.Shr, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_Bot_MinT.Sam_list <- split(Recon_PointF_Bot_MinT.Sam, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_Bot_Opt_list <- split(Recon_PointF_Bot_Opt, 1:min(H, nrow(Testing_eval)))

  Recon_Var.Cov_Bot_BU_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                      function(x) Recon_Var.Cov_Bot_BU)
  Recon_Var.Cov_Bot_OLS_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                       function(x) Recon_Var.Cov_Bot_OLS)
  Recon_Var.Cov_Bot_WLS_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                       function(x) Recon_Var.Cov_Bot_WLS)
  Recon_Var.Cov_Bot_MinT.Shr_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                            function(x) Recon_Var.Cov_Bot_MinT.Shr)
  Recon_Var.Cov_Bot_MinT.Sam_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                            function(x) Recon_Var.Cov_Bot_MinT.Sam)
  
  #Calculating Log score for Bot predicive densities
  LS_Bot_BU <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_BU_list, 
                      Mean = Recon_PointF_Bot_BU_list, real = Test.list_Bot)
  LS_Bot_OLS <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_OLS_list, 
                       Mean = Recon_PointF_Bot_OLS_list, real = Test.list_Bot)
  LS_Bot_WLS <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_WLS_list, 
                       Mean = Recon_PointF_Bot_WLS_list, real = Test.list_Bot)
  LS_Bot_MinT.Shr <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_MinT.Shr_list, 
                            Mean = Recon_PointF_Bot_MinT.Shr_list, real = Test.list_Bot)
  LS_Bot_MinT.Sam <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_MinT.Sam_list, 
                            Mean = Recon_PointF_Bot_MinT.Sam_list, real = Test.list_Bot)
  LS_Bot_Opt <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_Opt, 
                            Mean = Recon_PointF_Bot_Opt_list, real = Test.list_Bot)
  
  
  
  #Adding to ES and VS full to data frame
  DF_MultiV_Full %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_Unrecon, 
        "Variogram score" = VS_full_Unrecon) -> DF_Base
  DF_Base[names(DF_MultiV_Full)] -> DF_Base
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_full_BU, 
        "Variogram score" = VS_full_BU) -> DF_BU
  DF_BU[names(DF_MultiV_Full)] -> DF_BU
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_OLS, 
        "Variogram score" = VS_full_OLS) -> DF_OLS
  DF_OLS[names(DF_MultiV_Full)] -> DF_OLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_WLS, 
        "Variogram score" = VS_full_WLS) -> DF_WLS
  DF_WLS[names(DF_MultiV_Full)] -> DF_WLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_MinT.Shr, 
        "Variogram score" = VS_full_MinT.Shr) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Full)] -> DF_MinT.Shr
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_MinT.Sam, 
        "Variogram score" = VS_full_MinT.Sam) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Full)] -> DF_MinT.Sam
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_Opt, 
        "Variogram score" = VS_full_Opt) -> DF_Opt
  DF_Opt[names(DF_MultiV_Full)] -> DF_Opt
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_Opt)
  

  #Adding to ES, VS and LS of Bot to data frame
  
  DF_MultiV_Bot %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_Unrecon, 
        "Variogram score" = VS_Bot_Unrecon, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_Bot)] -> DF_Base
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_Bot_BU, 
        "Variogram score" = VS_Bot_BU, 
        "Log score" = LS_Bot_BU) -> DF_BU
  DF_BU[names(DF_MultiV_Bot)] -> DF_BU
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_OLS, 
        "Variogram score" = VS_Bot_OLS, 
        "Log score" = LS_Bot_OLS) -> DF_OLS
  DF_OLS[names(DF_MultiV_Bot)] -> DF_OLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_WLS, 
        "Variogram score" = VS_Bot_WLS, 
        "Log score" = LS_Bot_WLS) -> DF_WLS
  DF_WLS[names(DF_MultiV_Bot)] -> DF_WLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_MinT.Shr, 
        "Variogram score" = VS_Bot_MinT.Shr, 
        "Log score" = LS_Bot_MinT.Shr) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Bot)] -> DF_MinT.Shr
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_MinT.Sam, 
        "Variogram score" = VS_Bot_MinT.Sam, 
        "Log score" = LS_Bot_MinT.Sam) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Bot)] -> DF_MinT.Sam
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_Bot_Opt, 
        "Variogram score" = VS_Bot_Opt, 
        "Log score" = LS_Bot_Opt) -> DF_Opt
  DF_Opt[names(DF_MultiV_Bot)] -> DF_Opt
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_Opt)
  
  #Adding to CRPS and LS of univariate series to data frame
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication", "Series") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_Unrecon), 
        "LS" = as.numeric(LS_Unrecon)) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_BU), 
        "LS" = as.numeric(LS_BU)) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_OLS), 
        "LS" = as.numeric(LS_OLS)) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_WLS), 
        "LS" = as.numeric(LS_WLS)) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_MinT.Shr), 
        "LS" = as.numeric(LS_MinT.Shr)) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_MinT.Sam), 
        "LS" = as.numeric(LS_MinT.Sam)) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_UniV)] -> DF_MinT.Sam
  DF_UniV <- rbind(DF_UniV, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "CRPS" = as.numeric(CRPS_Opt), 
        "LS" = as.numeric(LS_Opt)) -> DF_Opt
  DF_Opt[names(DF_UniV)] -> DF_Opt
  DF_UniV <- rbind(DF_UniV, DF_Opt)
  
}

End_sim <- Sys.time()

DF_MultiV_Full[complete.cases(DF_MultiV_Full[ , "R-method"]),] -> DF_MultiV_Full
DF_MultiV_Bot[complete.cases(DF_MultiV_Bot[ , "R-method"]),] -> DF_MultiV_Bot
DF_UniV[complete.cases(DF_UniV[ , "R-method"]),] -> DF_UniV


write.csv(DF_MultiV_Full, "Results/DF_MultiV_Full_GaussianDGP.csv")
write.csv(DF_MultiV_Bot, "Results/DF_MultiV_Bot_GaussianDGP.csv")
write.csv(DF_UniV, "Results/DF_UniV_GaussianDGP.csv")

save.image("Results/ParametricRecon-GaussianDGP.RData")