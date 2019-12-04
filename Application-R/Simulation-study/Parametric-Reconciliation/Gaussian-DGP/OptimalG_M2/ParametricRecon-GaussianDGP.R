#Required packages

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

set.seed(1989)

start <- Sys.time()

C <- 1000   #C - Length of the outer rolling window
L <- 500    #L - Length of the rolling window 
r <- 100    #r - Length of the training set to learn G matrix
I <- 398    #I - Length of inner rolling window for training G
m <- 4      #m - Number of bottom level series
B <- 2500   #B - Number of random numbers generated from the predictive distributions
H <- 3      #H - Forecast horizons

Bottom_level <- read.csv("Bottom_level.csv")[,-1]

#Generating the hierarchy

Hierarchy <- suppressMessages(hts(Bottom_level, list(2, c(2,2))))
AllTS <- allts(Hierarchy)
AllTS <- AllTS[-(1:500),] # To avoid impact from initial values

n <- ncol(AllTS)  #n - Total number of series
colnames(AllTS) <- c("Total", "A", "B", "AA", "AB", "BA", "BB")

#Generating the summing matrix
S <- smatrix(Hierarchy)

#Code to get shrinkage estimator

lowerD <- function(x)
{
  n2 <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n2))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!", call. = FALSE)
  p <- ncol(x)
  n <- nrow(x)
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
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

Start <- Sys.time()

for (j in 1:1000) {
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):(L+j+2),]
  
####---Learning G---####
  
  Testing_G_h1 <- matrix(0,r,n)
  Testing_G_h2 <- matrix(0,r,n)
  Testing_G_h3 <- matrix(0,r,n)
  
  Base_mean_h1 <- matrix(0, r, n)
  Base_mean_h2 <- matrix(0, r, n)
  Base_mean_h3 <- matrix(0, r, n)
  
  Base_Sigma_learnG <- list()
  
  Start_train <-  Sys.time()
  for (q in 1:r)
  {
    Training_G <- Train[q:(I+q-1), ]
    Testing_G_h1[q,] <- Train[(I+q), ]
    Testing_G_h2[q,] <- Train[(I+q+1), ]
    Testing_G_h3[q,] <- Train[(I+q+2), ]
    
    ##-Model fitting, forecasting and obtaining residuals-##
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training_G), ncol = n)
    fit_training <- list()
    Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
    
    
    for(i in 1:n) {
      
      TS <- ts(Training_G[,i])
      fit_training[[i]] <- auto.arima(TS)
      Base_forecasts_training[,i] <- forecast(fit_training[[i]], h = H)$mean
      Residuals_all_training[,i] <- Training_G[,i] - fitted(fit_training[[i]])
      
    }
    
    targ <- diag(diag(var(Residuals_all_training)), n, n)
    shrink <- shrink.estim(Residuals_all_training,targ)
    Base_Sigma_learnG[[q]] <- shrink[[1]]
    
    Base_mean_h1[q,] <- Base_forecasts_training[1,]
    Base_mean_h2[q,] <- Base_forecasts_training[2,]
    Base_mean_h3[q,] <- Base_forecasts_training[3,]
    
  }
  
  End_train <-  Sys.time()
  
  # FP_training_list <- list(Unreconciled_future_paths_h1, Unreconciled_future_paths_h2, 
  #                          Unreconciled_future_paths_h3)
  
  Test_learnG <- list(Testing_G_h1, Testing_G_h2, Testing_G_h3)
  Base_fc_learnG <- list(Base_mean_h1, Base_mean_h2, Base_mean_h3)
  
  ######################################
    ## Model fitting to `train` set ##  
  ######################################
  
  #To store base forecasts
  Base_fc <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  #Matrix to store model insample forecast errors.
  ForeError_all <- matrix(NA, nrow = nrow(Train), ncol = n)
  
  for(i in 1:n) {
    
    TS <- ts(Train[,i])
    
    #Forecsting with ARIMA
    fit <- auto.arima(TS) #, stepwise=FALSE,approx=FALSE
    Forecast <- forecast(fit, h = min(H, nrow(Test[,i])))
    Base_fc[,i] <- Forecast$mean
    ForeError_all[,i] <- as.vector(TS - fitted(fit))
    
  }  
  
  
  
  ##########################################################    
    ##Calculating different G matrices including Optimal##
  ##########################################################  
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  #MinT shrink G
  targ <- diag(diag(var(na.omit(ForeError_all))), n, n)
  shrink <- shrink.estim(na.omit(ForeError_all),targ)
  Shr.cov_full <- shrink[[1]]
  Inv_Shr.cov <- solve(Shr.cov_full)
  
  MinT.Shr_G <- solve(t(S) %*% Inv_Shr.cov %*% S) %*% t(S) %*% Inv_Shr.cov
  
  #MinT sample G
  Sam.cov_full <- var(na.omit(ForeError_all))
  Inv_Sam.cov <- solve(Sam.cov_full)
  
  MinT.Sam_G <- solve(t(S) %*% Inv_Sam.cov %*% S) %*% t(S) %*% Inv_Sam.cov
  
  #WLS G
  Inv_WLS <- diag(1/diag(var(na.omit(ForeError_all))), n, n)
  
  WLS_G <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS
  
  
  ###--Optimal W_h (W_1, W_2, W_3)--###
  
  #Note: We will initiate with the Identity matrix. 
   
  
  Int_par_W <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  Opt_G <- list() #Stores Optimal G from method 1 for H forecast horizons
  
  Start_opt <- Sys.time()
  for (h in 1:H) {
    
    Opt_Vec_W <- optim(Int_par_W, Obj_func_LS, method = "BFGS", Sigma_hat = Base_Sigma_learnG, 
                       Mu_hat = Base_fc_learnG[[h]], Test = Test_learnG[[h]], m1 = m, n1 = n,                       r1 = r, S1=S)$par
    Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
    Opt_G[[h]] <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
    
  }
  
  End_opt <- Sys.time()
  
  
  
  
  
  #######################################  
  ###Mean forecast reconciliation###
  #######################################  
  
  ##--Reconciled point forecasts for the full hierarchy--## 
  
  Recon_PointF_full_BU <- t(S %*% BU_G %*% t(Base_fc))
  Recon_PointF_full_OLS <- t(S %*% OLS_G %*% t(Base_fc))
  Recon_PointF_full_WLS <- t(S %*% WLS_G %*% t(Base_fc))
  Recon_PointF_full_MinT.Shr <- t(S %*% MinT.Shr_G %*% t(Base_fc))
  Recon_PointF_full_MinT.Sam <- t(S %*% MinT.Sam_G %*% t(Base_fc))
  
  Recon_PointF_full_Opt <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  for (h in 1:min(H, nrow(Test))) {
    Recon_PointF_full_Opt[h,] <- S %*% Opt_G[[h]] %*% Base_fc[h,]
  }
  
  ##--Reconciled bottom level point forecasts--##
  
  Recon_PointF_Bot_BU <- t(BU_G %*% t(Base_fc))
  Recon_PointF_Bot_OLS <- t(OLS_G %*% t(Base_fc))
  Recon_PointF_Bot_WLS <- t(WLS_G %*% t(Base_fc))
  Recon_PointF_Bot_MinT.Shr <- t(MinT.Shr_G %*% t(Base_fc))
  Recon_PointF_Bot_MinT.Sam <- t(MinT.Sam_G %*% t(Base_fc))
  
  Recon_PointF_Bot_Opt <- matrix(0, nrow = min(H, nrow(Test)), ncol = m)
  for (h in 1:min(H, nrow(Test))) {
    Recon_PointF_Bot_Opt[h,] <- Opt_G[[h]] %*% Base_fc[h,]
  }
  
  ############################################    
  ###Variance forecast reconciliation###
  ############################################  
  
  ##--Reconciled variance forecasts for the full hierarchy--##
  
  Recon_Var.Cov_full_BU <- S %*% BU_G %*% Shr.cov_full %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS <- S %*% OLS_G %*% Shr.cov_full %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS <- S %*% WLS_G %*% Shr.cov_full %*% t(S %*% WLS_G)
  Recon_Var.Cov_full_MinT.Shr <- S %*% MinT.Shr_G %*% Shr.cov_full %*% t(S %*% MinT.Shr_G)
  Recon_Var.Cov_full_MinT.Sam <- S %*% MinT.Sam_G %*% Sam.cov_full %*% t(S %*% MinT.Sam_G)
  
  Recon_Var.Cov_full_Opt <- list()
  for (h in 1:min(H, nrow(Test))) {
    Recon_Var.Cov_full_Opt[[h]] <- S %*% Opt_G[[h]] %*% Shr.cov_full %*% t(S %*% Opt_G[[h]])
  }
  
  ##--Reconciled bottom level variance forecasts--##
  
  Recon_Var.Cov_Bot_BU <- BU_G %*% Shr.cov_full %*% t(BU_G)
  Recon_Var.Cov_Bot_OLS <- OLS_G %*% Shr.cov_full %*% t(OLS_G)
  Recon_Var.Cov_Bot_WLS <- WLS_G %*% Shr.cov_full %*% t(WLS_G)
  Recon_Var.Cov_Bot_MinT.Shr <- MinT.Shr_G %*% Shr.cov_full %*% t(MinT.Shr_G)
  Recon_Var.Cov_Bot_MinT.Sam <- MinT.Sam_G %*% Sam.cov_full %*% t(MinT.Sam_G)
  
  Recon_Var.Cov_Bot_Opt <- list()
  for (h in 1:min(H, nrow(Test))) {
    Recon_Var.Cov_Bot_Opt[[h]] <- Opt_G[[h]] %*% Shr.cov_full %*% t(Opt_G[[h]])
  }
  
  #List to store random samples from reconciled Gauss distribution of the full hierarchy
  
  X_full_BU <- list(min(H, nrow(Test)))
  X_full_OLS <- list(min(H, nrow(Test)))
  X_full_WLS <- list(min(H, nrow(Test)))
  X_full_MinT.Shr <- list(min(H, nrow(Test)))
  X_full_MinT.Sam <- list(min(H, nrow(Test)))
  X_full_Opt <- list(min(H, nrow(Test)))
  X_full_unrecon <- list(min(H, nrow(Test)))
  
  
  #List to store random samples from reconciled Gauss distribution of the bottom levels
  X_Bot_BU <- list(min(H, nrow(Test)))
  X_Bot_OLS <- list(min(H, nrow(Test)))
  X_Bot_WLS <- list(min(H, nrow(Test)))
  X_Bot_MinT.Shr <- list(min(H, nrow(Test)))
  X_Bot_MinT.Sam <- list(min(H, nrow(Test)))
  X_Bot_Opt <- list(min(H, nrow(Test)))
  X_Bot_unrecon <- list(min(H, nrow(Test)))
  
  #To store univariate scores
  CRPS_BU <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Sam <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Opt <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  LS_BU <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_OLS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_WLS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_MinT.Shr <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_MinT.Sam <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_Opt <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_Unrecon <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  
  ###################################
      #Random sample generation#
  ################################### 
  
  #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
  #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
  # function to generate random samples)
  
  
  for (h in 1: min(H, nrow(Test))) {
    
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
    
    X_full_unrecon[[h]] <- mvrnorm(n = B, mu = Base_fc[h,], Sigma = Shr.cov_full)
    
    
    
    
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
    
    X_Bot_unrecon[[h]] <- mvrnorm(n = B, mu = Base_fc[h, (n-m+1):n], 
                                        Sigma = Shr.cov_full[(n-m+1):n, (n-m+1):n])
    
    ##--Calculating CRPS for univariate predictive densities--##

    for (i in 1:n) {

      CRPS_Unrecon[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon[[h]][,i],
                                             method = "edf")
      CRPS_BU[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU[[h]][,i],
                                        method = "edf")
      CRPS_OLS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS[[h]][,i],
                                         method = "edf")
      CRPS_WLS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr[[h]][,i],
                                              method = "edf")
      CRPS_MinT.Sam[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Sam[[h]][,i],
                                              method = "edf")
      CRPS_Opt[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_Opt[[h]][,i],
                                   method = "edf")
      
      #Calculating LS for univariate predictive densities
      
      LS_Unrecon[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Base_fc[h,i], 
                                      sd = sqrt(Shr.cov_full[i,i]),log = TRUE)
      LS_BU[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_BU[h,i], 
                                 sd = sqrt(Recon_Var.Cov_full_BU[i,i]),log = TRUE)
      LS_OLS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_OLS[h,i], 
                                  sd = sqrt(Recon_Var.Cov_full_OLS[i,i]),log = TRUE)
      LS_WLS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_WLS[h,i], 
                                  sd = sqrt(Recon_Var.Cov_full_WLS[i,i]),log = TRUE)
      LS_MinT.Shr[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_MinT.Shr[h,i], 
                                       sd = sqrt(Recon_Var.Cov_full_MinT.Shr[i,i]),log = TRUE)
      LS_MinT.Sam[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_MinT.Sam[h,i], 
                                       sd = sqrt(Recon_Var.Cov_full_MinT.Sam[i,i]),log = TRUE)
      LS_Opt[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_Opt[h,i], 
                            sd = sqrt(Recon_Var.Cov_full_Opt[[h]][i,i]),log = TRUE)
      
    }
    
    
    DF_MultiV_Full <- DF_MultiV_Full %>% add_row("F-method" = "ARIMA",
                                                 "Replication" = j)
    
    DF_MultiV_Bot <- DF_MultiV_Bot %>% add_row("F-method" = "ARIMA",
                                               "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("Series" = rep(colnames(AllTS), each = min(H, nrow(Test))),
                                   "F-method" = "ARIMA",
                                   "Replication" = j)
    
  }
  
  
  Test.list_full <- split(Test[1:min(H, nrow(Test)),], 1:min(H, nrow(Test)))
  Test.list_Bot <- split(Test[1:min(H, nrow(Test)),(n-m+1):n], 1:min(H, nrow(Test)))
  
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
  Recon_PointF_Bot_BU_list <- split(Recon_PointF_Bot_BU, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_OLS_list <- split(Recon_PointF_Bot_OLS, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_WLS_list <- split(Recon_PointF_Bot_WLS, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_MinT.Shr_list <- split(Recon_PointF_Bot_MinT.Shr, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_MinT.Sam_list <- split(Recon_PointF_Bot_MinT.Sam, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_Opt_list <- split(Recon_PointF_Bot_Opt, 1:min(H, nrow(Test)))
  
  Recon_Var.Cov_Bot_BU_list <- lapply(1:min(H, nrow(Test)), 
                                            function(x) Recon_Var.Cov_Bot_BU)
  Recon_Var.Cov_Bot_OLS_list <- lapply(1:min(H, nrow(Test)), 
                                             function(x) Recon_Var.Cov_Bot_OLS)
  Recon_Var.Cov_Bot_WLS_list <- lapply(1:min(H, nrow(Test)), 
                                             function(x) Recon_Var.Cov_Bot_WLS)
  Recon_Var.Cov_Bot_MinT.Shr_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_Bot_MinT.Shr)
  Recon_Var.Cov_Bot_MinT.Sam_list <- lapply(1:min(H, nrow(Test)), 
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
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_Unrecon, 
        "Variogram score" = VS_full_Unrecon) -> DF_Base
  DF_Base[names(DF_MultiV_Full)] -> DF_Base
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
        "Energy score" = ES_full_BU, 
        "Variogram score" = VS_full_BU) -> DF_BU
  DF_BU[names(DF_MultiV_Full)] -> DF_BU
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_OLS, 
        "Variogram score" = VS_full_OLS) -> DF_OLS
  DF_OLS[names(DF_MultiV_Full)] -> DF_OLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_WLS, 
        "Variogram score" = VS_full_WLS) -> DF_WLS
  DF_WLS[names(DF_MultiV_Full)] -> DF_WLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_MinT.Sam, 
        "Variogram score" = VS_full_MinT.Sam) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Full)] -> DF_MinT.Sam
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_MinT.Shr, 
        "Variogram score" = VS_full_MinT.Shr) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Full)] -> DF_MinT.Shr
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_Opt, 
        "Variogram score" = VS_full_Opt) -> DF_Opt
  DF_Opt[names(DF_MultiV_Full)] -> DF_Opt
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_Opt)
  
  
  #Adding to ES, VS and LS of Bot to data frame
  
  DF_MultiV_Bot %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_Unrecon, 
        "Variogram score" = VS_Bot_Unrecon, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_Bot)] -> DF_Base
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
        "Energy score" = ES_Bot_BU, 
        "Variogram score" = VS_Bot_BU, 
        "Log score" = LS_Bot_BU) -> DF_BU
  DF_BU[names(DF_MultiV_Bot)] -> DF_BU
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_OLS, 
        "Variogram score" = VS_Bot_OLS, 
        "Log score" = LS_Bot_OLS) -> DF_OLS
  DF_OLS[names(DF_MultiV_Bot)] -> DF_OLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_WLS, 
        "Variogram score" = VS_Bot_WLS, 
        "Log score" = LS_Bot_WLS) -> DF_WLS
  DF_WLS[names(DF_MultiV_Bot)] -> DF_WLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_MinT.Sam, 
        "Variogram score" = VS_Bot_MinT.Sam, 
        "Log score" = LS_Bot_MinT.Sam) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Bot)] -> DF_MinT.Sam
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_MinT.Shr, 
        "Variogram score" = VS_Bot_MinT.Shr, 
        "Log score" = LS_Bot_MinT.Shr) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Bot)] -> DF_MinT.Shr
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Shr)
  
    
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_Opt, 
        "Variogram score" = VS_Bot_Opt, 
        "Log score" = LS_Bot_Opt) -> DF_Opt
  DF_Opt[names(DF_MultiV_Bot)] -> DF_Opt
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_Opt)
  
  
  
  ##--Adding to CRPS and LS of univariate series to data frame--##
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication", "Series") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_Unrecon), 
        "LS" = as.numeric(LS_Unrecon)) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_BU), 
        "LS" = as.numeric(LS_BU)) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_OLS), 
        "LS" = as.numeric(LS_OLS)) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_WLS), 
        "LS" = as.numeric(LS_WLS)) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_MinT.Sam), 
        "LS" = as.numeric(LS_MinT.Sam)) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_UniV)] -> DF_MinT.Sam
  DF_UniV <- rbind(DF_UniV, DF_MinT.Sam)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_MinT.Shr), 
        "LS" = as.numeric(LS_MinT.Shr)) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_Opt), 
        "LS" = as.numeric(LS_Opt)) -> DF_Opt
  DF_Opt[names(DF_UniV)] -> DF_Opt
  DF_UniV <- rbind(DF_UniV, DF_Opt)
  
}

End <- Sys.time()



DF_MultiV_Full[complete.cases(DF_MultiV_Full[ , "R-method"]),] -> DF_MultiV_Full
DF_MultiV_Bot[complete.cases(DF_MultiV_Bot[ , "R-method"]),] -> DF_MultiV_Bot
DF_UniV[complete.cases(DF_UniV[ , "R-method"]),] -> DF_UniV


write.csv(DF_MultiV_Full, "Results/DF_MultiV_Full_GaussianDGP.csv")
write.csv(DF_MultiV_Bot, "Results/DF_MultiV_Bot_GaussianDGP.csv")
write.csv(DF_UniV, "Results/DF_UniV_GaussianDGP.csv")

save.image("Results/ParametricRecon-GaussianDGP.RData")
