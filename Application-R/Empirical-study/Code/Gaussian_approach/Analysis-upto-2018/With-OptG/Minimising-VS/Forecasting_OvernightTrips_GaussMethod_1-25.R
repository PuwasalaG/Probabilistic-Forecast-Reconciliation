#Required packages

require(tidyverse)
require(fpp2)
require(readxl)
require(hts)
library(zoo)
library(magrittr)
library(Matrix)
library(seasonal)
library(gridExtra)
library(MASS)
library(scoringRules)
library(plyr)
library(purrr)
library(furrr)
library(miscTools)
library(hydroGOF)
library(numDeriv)
library(psych)

set.seed(1989)

source("Optim-functions.R")
source("Score-functions.R")

#Importing data

OvernightTrips_Region <- read_csv("OvernightTrips_2018.csv")[,-(1:3)]

#generating the hierarchy
#Hierarchy<-suppressMessages(hts(Bottom_level, list(7, c(14,21,12,12,5,5,7))))
Hierarchy<-suppressMessages(hts(OvernightTrips_Region, list(7, c(6,5,4,4,3,3,2), 
                                                   c(2,2,1,4,4,1,3,1,3,6,7,3,5,3,2,3,
                                                     3,4,2,3,1,1,1,2,2,3,4))))
AllTS <- allts(Hierarchy) %>% as_tibble()
n <- ncol(AllTS)
l1 <- 1
l2 <- sum(7)
l3 <- sum(6,5,4,4,3,3,2)
m <- ncol(OvernightTrips_Region)

L <- 52             #Size of the training window
r <- 100            #r - Length of the training set using to learn G matrix
#p <- nrow(AllTS) - L #number of replications
B <- 1000           # Number of random numbers generated from the predictive distributions
H <- 12             #Forecast horizones

#Generating the summing matrix
S <- smatrix(Hierarchy)
S_state <- S[(l1+1):(l1+l2),]
S_zone <- S[(l1+l2+1):(l1+l2+l3),]

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
# rnorm_degenerate <- function(mu, Sigma, n, k)
# {
#   Sigma <- matrix(Sigma, n, n)
#   SVD <- svd((Sigma + t(Sigma))/2)
#   
#   SVD$d <- abs(zapsmall(SVD$d))
#   U <- SVD$u
#   D <- diag(sqrt(SVD$d))
#   m1 <- sum(SVD$d > 0)
#   
#   X <- mvtnorm::rmvnorm(k, mean = rep(0, m1), diag(1,m1,m1))
#   X <- cbind(X, matrix(0, nrow = k, ncol = n-m1))
#   
#   Mu <- matrix(rep(mu, k), k, n, byrow = TRUE)
#   
#   Y <- t(U %*% D %*% t(X)) + Mu
#   
#   return(Y)
#   
# }

#All the forecasts and related information are stored in the DF dataframe

DF_MultiV_Total <- tibble("F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "Energy score" = numeric(),
                    "Variogram score" = numeric(),
                    "Replication" = integer())

DF_MultiV_States <- tibble("F-method" = character(),
                          "R-method" = character(),
                          "Forecast Horizon" = integer(),
                          "Energy score" = numeric(),
                          "Variogram score" = numeric(),
                          "Log score" = numeric(),
                          "Replication" = integer())

DF_MultiV_Zones <- tibble("F-method" = character(),
                          "R-method" = character(),
                          "Forecast Horizon" = integer(),
                          "Energy score" = numeric(),
                          "Log score" = numeric(),
                          "Variogram score" = numeric(),
                          "Replication" = integer())

DF_MultiV_Regions <- tibble("F-method" = character(),
                           "R-method" = character(),
                           "Forecast Horizon" = integer(),
                           "Energy score" = numeric(),
                           "Variogram score" = numeric(),
                           "Log score" = numeric(),
                           "Replication" = integer())

DF_UniV <- tibble("Series" = character(),
                  "F-method" = character(),
                  "R-method" = character(),
                  "Forecast Horizon" = integer(),
                  "Actual" = double(),
                  "CRPS" = numeric(),
                  "Replication" = integer())

Start <- Sys.time()

for (j in 1:25) {#p #??59
  
  
  AllTS_a <- AllTS[j : (163+j),]
  
  Testing_h1 <- matrix(0,r,n) 
  Testing_h2 <- matrix(0,r,n)
  Testing_h3 <- matrix(0,r,n)
  Testing_h4 <- matrix(0,r,n)
  Testing_h5 <- matrix(0,r,n)
  Testing_h6 <- matrix(0,r,n)
  Testing_h7 <- matrix(0,r,n)
  Testing_h8 <- matrix(0,r,n)
  Testing_h9 <- matrix(0,r,n)
  Testing_h10 <- matrix(0,r,n)
  Testing_h11 <- matrix(0,r,n)
  Testing_h12 <- matrix(0,r,n)
  
  Base_mean_h1 <- matrix(0, r, n)
  Base_mean_h2 <- matrix(0, r, n)
  Base_mean_h3 <- matrix(0, r, n)
  Base_mean_h4 <- matrix(0, r, n)
  Base_mean_h5 <- matrix(0, r, n)
  Base_mean_h6 <- matrix(0, r, n)
  Base_mean_h7 <- matrix(0, r, n)
  Base_mean_h8 <- matrix(0, r, n)
  Base_mean_h9 <- matrix(0, r, n)
  Base_mean_h10 <- matrix(0, r, n)
  Base_mean_h11 <- matrix(0, r, n)
  Base_mean_h12 <- matrix(0, r, n)
  
  Base_Sigma_Cov <- list()
  
  X_trainG_unrecon_h1 <- list()
  X_trainG_unrecon_h2 <- list()
  X_trainG_unrecon_h3 <- list()
  X_trainG_unrecon_h4 <- list()
  X_trainG_unrecon_h5 <- list()
  X_trainG_unrecon_h6 <- list()
  X_trainG_unrecon_h7 <- list()
  X_trainG_unrecon_h8 <- list()
  X_trainG_unrecon_h9 <- list()
  X_trainG_unrecon_h10 <- list()
  X_trainG_unrecon_h11 <- list()
  X_trainG_unrecon_h12 <- list()
  
  
  Start_train <-  Sys.time()
  for (q in 1:r)
  {
    Training <- AllTS_a[q:(L+q-1), ]
    Testing_h1[q,] <- AllTS_a[(L+q), ] %>% as.numeric()
    Testing_h2[q,] <- AllTS_a[(L+q+1), ] %>% as.numeric()
    Testing_h3[q,] <- AllTS_a[(L+q+2), ] %>% as.numeric()
    Testing_h4[q,] <- AllTS_a[(L+q+3), ] %>% as.numeric()
    Testing_h5[q,] <- AllTS_a[(L+q+4), ] %>% as.numeric()
    Testing_h6[q,] <- AllTS_a[(L+q+5), ] %>% as.numeric()
    Testing_h7[q,] <- AllTS_a[(L+q+6), ] %>% as.numeric()
    Testing_h8[q,] <- AllTS_a[(L+q+7), ] %>% as.numeric()
    Testing_h9[q,] <- AllTS_a[(L+q+8), ] %>% as.numeric()
    Testing_h10[q,] <- AllTS_a[(L+q+9), ] %>% as.numeric()
    Testing_h11[q,] <- AllTS_a[(L+q+10), ] %>% as.numeric()
    Testing_h12[q,] <- AllTS_a[(L+q+11), ] %>% as.numeric()
    
    ##--Model fitting, forecasting and obtaining residuals--##
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
    fit_training <- list()
    Base_forecasts_training <- matrix(NA, nrow = H, ncol = n)
    
    
    for(i in 1:n) {
      
      TS <- ts(Training[,i])
      fit_training[[i]] <- auto.arima(TS)
      Base_forecasts_training[,i] <- forecast(fit_training[[i]], h = H)$mean
      Residuals_all_training[,i] <- TS - fitted(fit_training[[i]])
      
    }
    
    targ <- diag(diag(var(Residuals_all_training)), n, n)
    shrink <- shrink.estim(Residuals_all_training,targ)
    Base_Sigma_Cov[[q]] <- shrink[[1]]
    
    Base_mean_h1[q,] <- Base_forecasts_training[1,]
    Base_mean_h2[q,] <- Base_forecasts_training[2,]
    Base_mean_h3[q,] <- Base_forecasts_training[3,]
    Base_mean_h4[q,] <- Base_forecasts_training[4,]
    Base_mean_h5[q,] <- Base_forecasts_training[5,]
    Base_mean_h6[q,] <- Base_forecasts_training[6,]
    Base_mean_h7[q,] <- Base_forecasts_training[7,]
    Base_mean_h8[q,] <- Base_forecasts_training[8,]
    Base_mean_h9[q,] <- Base_forecasts_training[9,]
    Base_mean_h10[q,] <- Base_forecasts_training[10,]
    Base_mean_h11[q,] <- Base_forecasts_training[11,]
    Base_mean_h12[q,] <- Base_forecasts_training[12,]
    
    X_trainG_unrecon_h1[[q]] <- mvrnorm(n = B, mu = Base_mean_h1[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h2[[q]] <- mvrnorm(n = B, mu = Base_mean_h2[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h3[[q]] <- mvrnorm(n = B, mu = Base_mean_h3[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h4[[q]] <- mvrnorm(n = B, mu = Base_mean_h4[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h5[[q]] <- mvrnorm(n = B, mu = Base_mean_h5[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h6[[q]] <- mvrnorm(n = B, mu = Base_mean_h6[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h7[[q]] <- mvrnorm(n = B, mu = Base_mean_h7[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h8[[q]] <- mvrnorm(n = B, mu = Base_mean_h8[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h9[[q]] <- mvrnorm(n = B, mu = Base_mean_h9[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h10[[q]] <- mvrnorm(n = B, mu = Base_mean_h10[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h11[[q]] <- mvrnorm(n = B, mu = Base_mean_h11[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    X_trainG_unrecon_h12[[q]] <- mvrnorm(n = B, mu = Base_mean_h12[q,], 
                                        Sigma = Base_Sigma_Cov[[q]])
    
  }
  
  End_train <-  Sys.time()
  
  
  FP_training_list <- list(X_trainG_unrecon_h1, X_trainG_unrecon_h2,
                           X_trainG_unrecon_h3, X_trainG_unrecon_h4, 
                           X_trainG_unrecon_h5, X_trainG_unrecon_h6, 
                           X_trainG_unrecon_h7, X_trainG_unrecon_h8,
                           X_trainG_unrecon_h9, X_trainG_unrecon_h10, 
                           X_trainG_unrecon_h11,X_trainG_unrecon_h12)
  
  Testing <- list(Testing_h1, Testing_h2, Testing_h3, 
                  Testing_h4, Testing_h5, Testing_h6, 
                  Testing_h7, Testing_h8, Testing_h9,
                  Testing_h10, Testing_h11, Testing_h12)
  
  
  
  ###Method 1: Estimating W_h (W_1 to W_12)
  
  #Calculating initial Inv.weight matrix for the optimization problem
  
  Int_par_W <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  Opt_G <- list() #Stores Optimal G from method 1 for H forecast horizons

  Start_opt <- Sys.time()
  for (h in 1:H) {
    
    Opt_Vec_W <- optim(Int_par_W, Variogram_score_method1, method = "BFGS",
                       Future_paths = FP_training_list[[h]], Test = Testing[[h]], 
                       n1 = n, B1 = B, 
                       r1 = r, S1=S)$par
    Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
    Opt_G[[h]] <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
    
  }
  End_opt <- Sys.time()
  
  Start_rest <- Sys.time()
  
  ###---Evaluation---###
  

  Training_eval <- AllTS_a[(r+1):(L+r),]
  Testing_eval <- AllTS_a[(L+r+1):(L+r+H),]
  
  #To store base forecasts
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Testing_eval)), ncol = n)
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  
  Start_fc <- Sys.time()
  for(i in 1:n) {
    
    TS <- ts(Training_eval[,i], frequency = 12)
    
    #Forecsting with ARIMA
    fit_ARIMA <- auto.arima(TS) #, stepwise=FALSE,approx=FALSE
    Forecast_ARIMA <- forecast(fit_ARIMA, h = min(H, nrow(Testing_eval[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA))
    
  }  
  End_fc <- Sys.time() 
  
  
  ###--Calculating different G matrices required for reconciliation of ARIMA base forecasts--###
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  
  #MinT shrink G
  targ <- diag(diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  shrink <- shrink.estim(na.omit(ForeError_all_ARIMA),targ)
  Shr.cov_full_ARIMA <- shrink[[1]]
  Inv_Shr.cov_ARIMA <- solve(Shr.cov_full_ARIMA)
  
  MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
  
  #WLS G
  Inv_WLS_ARIMA <- diag(1/diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  
  WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
  
  
  
  ###Mean forecast reconciliation (from ARIMA base)###
  
  #Reconciled point forecasts for the full hierarchy 
  Recon_PointF_full_BU_ARIMA <- t(S %*% BU_G %*% t(Base_ARIMA))
  Recon_PointF_full_OLS_ARIMA <- t(S %*% OLS_G %*% t(Base_ARIMA))
  Recon_PointF_full_WLS_ARIMA <- t(S %*% WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_full_MinT.Shr_ARIMA <- t(S %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))

  Recon_PointF_full_OptG_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_full_OptG_ARIMA[h,] <- S %*% Opt_G[[h]] %*% Base_ARIMA[h,]
  }
  
  
  #Reconciled point forecasts for the states of the hierarchy 
  Recon_PointF_states_BU_ARIMA <- t(S_state %*% BU_G %*% t(Base_ARIMA))
  Recon_PointF_states_OLS_ARIMA <- t(S_state %*% OLS_G %*% t(Base_ARIMA))
  Recon_PointF_states_WLS_ARIMA <- t(S_state %*% WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_states_MinT.Shr_ARIMA <- t(S_state %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
  
  Recon_PointF_states_OptG_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = l2)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_states_OptG_ARIMA[h,] <- S_state %*% Opt_G[[h]] %*% Base_ARIMA[h,]
  }
  
  #Reconciled point forecasts for the zones of the hierarchy 
  Recon_PointF_zones_BU_ARIMA <- t(S_zone %*% BU_G %*% t(Base_ARIMA))
  Recon_PointF_zones_OLS_ARIMA <- t(S_zone %*% OLS_G %*% t(Base_ARIMA))
  Recon_PointF_zones_WLS_ARIMA <- t(S_zone %*% WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_zones_MinT.Shr_ARIMA <- t(S_zone %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
  
  Recon_PointF_zones_OptG_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = l3)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_zones_OptG_ARIMA[h,] <- S_zone %*% Opt_G[[h]] %*% Base_ARIMA[h,]
  }
  
  Recon_PointF_region_BU_ARIMA <- t(BU_G %*% t(Base_ARIMA))
  Recon_PointF_region_OLS_ARIMA <- t(OLS_G %*% t(Base_ARIMA))
  Recon_PointF_region_WLS_ARIMA <- t(WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_region_MinT.Shr_ARIMA <- t(MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
  
  Recon_PointF_region_OptG_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = m)
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_PointF_region_OptG_ARIMA[h,] <-  Opt_G[[h]] %*% Base_ARIMA[h,]
  }
  
  ###Variance forecast reconciliation (from ARIMA base)###
  
  
  #Reconciled variance forecasts for the full hierarchy (Followed from ARIMA)#
  Recon_Var.Cov_full_BU_ARIMA <- S %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS_ARIMA <- S %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS_ARIMA <- S %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% WLS_G_ARIMA)
  Recon_Var.Cov_full_MinT.Shr_ARIMA <- S %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% MinT.Shr_G_ARIMA)
  
  Recon_Var.Cov_full_OptG_ARIMA <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_full_OptG_ARIMA[[h]] <- S %*% Opt_G[[h]] %*% Shr.cov_full_ARIMA %*% t(S %*% Opt_G[[h]])
  }
  
  #Reconciled variance forecasts for the states hierarchy (Followed from ARIMA)#
  Recon_Var.Cov_states_BU_ARIMA <- S_state %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S_state %*% BU_G)
  Recon_Var.Cov_states_OLS_ARIMA <- S_state %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S_state %*% OLS_G)
  Recon_Var.Cov_states_WLS_ARIMA <- S_state %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_state %*% WLS_G_ARIMA)
  Recon_Var.Cov_states_MinT.Shr_ARIMA <- S_state %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_state %*% MinT.Shr_G_ARIMA)
  
  Recon_Var.Cov_states_OptG_ARIMA <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_states_OptG_ARIMA[[h]] <- S_state %*% Opt_G[[h]] %*% Shr.cov_full_ARIMA %*% t(S_state %*% Opt_G[[h]])
  }
  
  #Reconciled variance forecasts for the zones hierarchy (Followed from ARIMA)#
  Recon_Var.Cov_zones_BU_ARIMA <- S_zone %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S_zone %*% BU_G)
  Recon_Var.Cov_zones_OLS_ARIMA <- S_zone %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S_zone %*% OLS_G)
  Recon_Var.Cov_zones_WLS_ARIMA <- S_zone %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_zone %*% WLS_G_ARIMA)
  Recon_Var.Cov_zones_MinT.Shr_ARIMA <- S_zone %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_zone %*% MinT.Shr_G_ARIMA)
  
  Recon_Var.Cov_zones_OptG_ARIMA <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_zones_OptG_ARIMA[[h]] <- S_zone %*% Opt_G[[h]] %*% Shr.cov_full_ARIMA %*% t(S_zone %*% Opt_G[[h]])
  }
  
  #Reconciled bottom level variance forecasts (Followed from ARIMA)#
  Recon_Var.Cov_region_BU_ARIMA <- BU_G %*% Shr.cov_full_ARIMA %*% t(BU_G)
  Recon_Var.Cov_region_OLS_ARIMA <- OLS_G %*% Shr.cov_full_ARIMA %*% t(OLS_G)
  Recon_Var.Cov_region_WLS_ARIMA <- WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(WLS_G_ARIMA)
  Recon_Var.Cov_region_MinT.Shr_ARIMA <- MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(MinT.Shr_G_ARIMA)
  
  Recon_Var.Cov_region_OptG_ARIMA <- list()
  for (h in 1:min(H, nrow(Testing_eval))) {
    Recon_Var.Cov_region_OptG_ARIMA[[h]] <- Opt_G[[h]] %*% Shr.cov_full_ARIMA %*% t(Opt_G[[h]])
  }
  
  #List to store random samples from reconciled Gauss distribution of the full hierarchy
  X_full_BU_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_full_OLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_full_WLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_full_MinT.Shr_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_full_OptG_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_full_unrecon_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  
  #List to store random samples from reconciled Gauss distribution of the state levels
  X_states_BU_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_states_OLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_states_WLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_states_MinT.Shr_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_states_OptG_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_states_unrecon_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #List to store random samples from reconciled Gauss distribution of the Zone levels
  X_zones_BU_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_zones_OLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_zones_WLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_zones_MinT.Shr_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_zones_OptG_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_zones_unrecon_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #List to store random samples from reconciled Gauss distribution of the Region levels
  X_regions_BU_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_regions_OLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_regions_WLS_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_regions_MinT.Shr_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_regions_OptG_ARIMA <- list(min(H, nrow(Testing_eval)))
  X_regions_unrecon_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #To store univariate scores
  CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_OptG_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  
  
  for (h in 1: min(H, nrow(Testing_eval))) {
    
    #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
    #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
    # function to generate random samples)
    
    
    X_full_BU_ARIMA[[h]] <- mvrnorm(n=B, mu = Recon_PointF_full_BU_ARIMA[h,], 
                                             Sigma = Recon_Var.Cov_full_BU_ARIMA)
    
    X_full_OLS_ARIMA[[h]] <- mvrnorm(n=B, mu = Recon_PointF_full_OLS_ARIMA[h,], 
                                              Sigma = Recon_Var.Cov_full_OLS_ARIMA)
    
    X_full_WLS_ARIMA[[h]] <- mvrnorm(n=B, mu = Recon_PointF_full_WLS_ARIMA[h,], 
                                              Sigma = Recon_Var.Cov_full_WLS_ARIMA)
    
    X_full_MinT.Shr_ARIMA[[h]] <- mvrnorm(n=B, mu = Recon_PointF_full_MinT.Shr_ARIMA[h,], 
                                                   Sigma = Recon_Var.Cov_full_MinT.Shr_ARIMA)
    
    X_full_OptG_ARIMA[[h]] <- mvrnorm(n=B, mu = Recon_PointF_full_OptG_ARIMA[h,], 
                                                   Sigma = Recon_Var.Cov_full_OptG_ARIMA[[h]])
    
    X_full_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h,], Sigma = Shr.cov_full_ARIMA)
    
    
    
    
    #Obtaining random samples from the possible forecast Gaussian densities 
    #of state level of the hierarchy
    
    X_states_BU_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_BU_ARIMA[h,], 
                                      Sigma = Recon_Var.Cov_states_BU_ARIMA)
    
    X_states_OLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_OLS_ARIMA[h,], 
                                       Sigma = Recon_Var.Cov_states_OLS_ARIMA)
    
    X_states_WLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_WLS_ARIMA[h,], 
                                       Sigma = Recon_Var.Cov_states_WLS_ARIMA)
    
    X_states_MinT.Shr_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_MinT.Shr_ARIMA[h,], 
                                            Sigma = Recon_Var.Cov_states_MinT.Shr_ARIMA)
    
    X_states_OptG_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_OptG_ARIMA[h,], 
                                            Sigma = Recon_Var.Cov_states_OptG_ARIMA[[h]])
    
    X_states_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h,(l1+1):(l1+l2)], 
                                           Sigma = Shr.cov_full_ARIMA[(l1+1):(l1+l2), (l1+1):(l1+l2)])
    
    #Obtaining random samples from the possible forecast Gaussian densities 
    #of zone level of the hierarchy
    
    X_zones_BU_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_BU_ARIMA[h,], 
                                     Sigma = Recon_Var.Cov_zones_BU_ARIMA)
    
    X_zones_OLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_OLS_ARIMA[h,], 
                                      Sigma = Recon_Var.Cov_zones_OLS_ARIMA)
    
    X_zones_WLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_WLS_ARIMA[h,], 
                                      Sigma = Recon_Var.Cov_zones_WLS_ARIMA)
    
    X_zones_MinT.Shr_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_MinT.Shr_ARIMA[h,], 
                                           Sigma = Recon_Var.Cov_zones_MinT.Shr_ARIMA)
    
    X_zones_OptG_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_OptG_ARIMA[h,], 
                                           Sigma = Recon_Var.Cov_zones_OptG_ARIMA[[h]])
    
    X_zones_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h,(l1+l2+1):(l1+l2+l3)], 
                                          Sigma = Shr.cov_full_ARIMA[(l1+l2+1):(l1+l2+l3), (l1+l2+1):(l1+l2+l3)])
    
    #Obtaining random samples from the possible forecast Gaussian densities 
    #of zone level of the hierarchy
    
    X_regions_BU_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_BU_ARIMA[h,], 
                                       Sigma = Recon_Var.Cov_region_BU_ARIMA)
    
    X_regions_OLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_OLS_ARIMA[h,], 
                                        Sigma = Recon_Var.Cov_region_OLS_ARIMA)
    
    X_regions_WLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_WLS_ARIMA[h,], 
                                        Sigma = Recon_Var.Cov_region_WLS_ARIMA)
    
    X_regions_MinT.Shr_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_MinT.Shr_ARIMA[h,], 
                                             Sigma = Recon_Var.Cov_region_MinT.Shr_ARIMA)
    
    X_regions_OptG_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_OptG_ARIMA[h,], 
                                             Sigma = Recon_Var.Cov_region_OptG_ARIMA[[h]])
    
    X_regions_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h, (n-m+1):n], 
                                            Sigma = Shr.cov_full_ARIMA[(n-m+1):n, (n-m+1):n])
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_unrecon_ARIMA[[h]][,i],
                                             method = "edf")
      CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_BU_ARIMA[[h]][,i],
                                        method = "edf")
      CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_OLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_WLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_MinT.Shr_ARIMA[[h]][,i],
                                              method = "edf")
      CRPS_OptG_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = X_full_OptG_ARIMA[[h]][,i],
                                              method = "edf")
      
    }
    
    
    DF_MultiV_Total <- DF_MultiV_Total %>% add_row("F-method" = "ARIMA",
                                                   "Replication" = j)
    
    DF_MultiV_States <- DF_MultiV_States %>% add_row("F-method" = "ARIMA",
                                                     "Replication" = j)
    
    DF_MultiV_Zones <- DF_MultiV_Zones %>% add_row("F-method" = "ARIMA",
                                                   "Replication" = j)
    
    DF_MultiV_Regions <- DF_MultiV_Regions %>% add_row("F-method" = "ARIMA",
                                                       "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("F-method" = "ARIMA",
                                   "Replication" = rep(j, n*min(H, nrow(Testing_eval))))
    
  }
  
  
  Testing_eval.list_full <- split(Testing_eval[1:min(H, nrow(Testing_eval)),], 1:min(H, nrow(Testing_eval)))
  Testing_eval.list_states <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(l1+1):(l1+l2)], 1:min(H, nrow(Testing_eval)))
  Testing_eval.list_zones <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(l1+l2+1):(l1+l2+l3)], 1:min(H, nrow(Testing_eval)))
  Testing_eval.list_regions <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(n-m+1):n], 1:min(H, nrow(Testing_eval)))
  
  #Calculating Energy score for full predicive densities
  ES_full_BU_ARIMA <- mapply(Energy_score, Data = X_full_BU_ARIMA, Real = Testing_eval.list_full)
  ES_full_OLS_ARIMA <- mapply(Energy_score, X_full_OLS_ARIMA, Real = Testing_eval.list_full)
  ES_full_WLS_ARIMA <- mapply(Energy_score, X_full_WLS_ARIMA, Real = Testing_eval.list_full)
  ES_full_MinT.Shr_ARIMA <- mapply(Energy_score, X_full_MinT.Shr_ARIMA, Real = Testing_eval.list_full)
  ES_full_OptG_ARIMA <- mapply(Energy_score, X_full_OptG_ARIMA, Real = Testing_eval.list_full)
  ES_full_Unrecon_ARIMA <- mapply(Energy_score, X_full_unrecon_ARIMA, Real = Testing_eval.list_full)
  
  #Calculating Variogram score for full predicive densities
  VS_full_BU_ARIMA <- mapply(Variogram_score, Data = X_full_BU_ARIMA, Real = Testing_eval.list_full)
  VS_full_OLS_ARIMA <- mapply(Variogram_score, X_full_OLS_ARIMA, Real = Testing_eval.list_full)
  VS_full_WLS_ARIMA <- mapply(Variogram_score, X_full_WLS_ARIMA, Real = Testing_eval.list_full)
  VS_full_MinT.Shr_ARIMA <- mapply(Variogram_score, X_full_MinT.Shr_ARIMA, Real = Testing_eval.list_full)
  VS_full_OptG_ARIMA <- mapply(Variogram_score, X_full_OptG_ARIMA, Real = Testing_eval.list_full)
  VS_full_Unrecon_ARIMA <- mapply(Variogram_score, X_full_unrecon_ARIMA, Real = Testing_eval.list_full)
  
  #Calculating Energy score for states predicive densities
  ES_states_BU_ARIMA <- mapply(Energy_score, Data = X_states_BU_ARIMA, Real = Testing_eval.list_states)
  ES_states_OLS_ARIMA <- mapply(Energy_score, X_states_OLS_ARIMA, Real = Testing_eval.list_states)
  ES_states_WLS_ARIMA <- mapply(Energy_score, X_states_WLS_ARIMA, Real = Testing_eval.list_states)
  ES_states_MinT.Shr_ARIMA <- mapply(Energy_score, X_states_MinT.Shr_ARIMA, Real = Testing_eval.list_states)
  ES_states_OptG_ARIMA <- mapply(Energy_score, X_states_OptG_ARIMA, Real = Testing_eval.list_states)
  ES_states_Unrecon_ARIMA <- mapply(Energy_score, X_states_unrecon_ARIMA, Real = Testing_eval.list_states)
  
  #Calculating Variogram score for states predicive densities
  VS_states_BU_ARIMA <- mapply(Variogram_score, Data = X_states_BU_ARIMA, Real = Testing_eval.list_states)
  VS_states_OLS_ARIMA <- mapply(Variogram_score, X_states_OLS_ARIMA, Real = Testing_eval.list_states)
  VS_states_WLS_ARIMA <- mapply(Variogram_score, X_states_WLS_ARIMA, Real = Testing_eval.list_states)
  VS_states_MinT.Shr_ARIMA <- mapply(Variogram_score, X_states_MinT.Shr_ARIMA, Real = Testing_eval.list_states)
  VS_states_OptG_ARIMA <- mapply(Variogram_score, X_states_OptG_ARIMA, Real = Testing_eval.list_states)
  VS_states_Unrecon_ARIMA <- mapply(Variogram_score, X_states_unrecon_ARIMA, Real = Testing_eval.list_states)
  
  #Splitting Reconciled means and variances
  Recon_PointF_states_BU_ARIMA_list <- split(Recon_PointF_states_BU_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_states_OLS_ARIMA_list <- split(Recon_PointF_states_OLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_states_WLS_ARIMA_list <- split(Recon_PointF_states_WLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_states_MinT.Shr_ARIMA_list <- split(Recon_PointF_states_MinT.Shr_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_states_OptG_ARIMA_list <- split(Recon_PointF_states_OptG_ARIMA, 1:min(H, nrow(Testing_eval)))
  
  Recon_Var.Cov_states_BU_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                               function(x) Recon_Var.Cov_states_BU_ARIMA)
  Recon_Var.Cov_states_OLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                function(x) Recon_Var.Cov_states_OLS_ARIMA)
  Recon_Var.Cov_states_WLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                function(x) Recon_Var.Cov_states_WLS_ARIMA)
  Recon_Var.Cov_states_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                     function(x) Recon_Var.Cov_states_MinT.Shr_ARIMA)
  
  #Calculating Log score for states predicive densities
  LS_states_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_BU_ARIMA_list, 
                               Mean = Recon_PointF_states_BU_ARIMA_list, real = Testing_eval.list_states)
  LS_states_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_OLS_ARIMA_list, 
                                Mean = Recon_PointF_states_OLS_ARIMA_list, real = Testing_eval.list_states)
  LS_states_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_WLS_ARIMA_list, 
                                Mean = Recon_PointF_states_WLS_ARIMA_list, real = Testing_eval.list_states)
  LS_states_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_MinT.Shr_ARIMA_list, 
                                     Mean = Recon_PointF_states_MinT.Shr_ARIMA_list, real = Testing_eval.list_states)
  LS_states_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_OptG, 
                                     Mean = Recon_PointF_states_OptG_ARIMA_list, real = Testing_eval.list_states)
  
  #Calculating Energy score for zones predicive densities
  ES_zones_BU_ARIMA <- mapply(Energy_score, Data = X_zones_BU_ARIMA, Real = Testing_eval.list_zones)
  ES_zones_OLS_ARIMA <- mapply(Energy_score, X_zones_OLS_ARIMA, Real = Testing_eval.list_zones)
  ES_zones_WLS_ARIMA <- mapply(Energy_score, X_zones_WLS_ARIMA, Real = Testing_eval.list_zones)
  ES_zones_MinT.Shr_ARIMA <- mapply(Energy_score, X_zones_MinT.Shr_ARIMA, Real = Testing_eval.list_zones)
  ES_zones_OptG_ARIMA <- mapply(Energy_score, X_zones_OptG_ARIMA, Real = Testing_eval.list_zones)
  ES_zones_Unrecon_ARIMA <- mapply(Energy_score, X_zones_unrecon_ARIMA, Real = Testing_eval.list_zones)
  
  #Calculating Variogram score for zones predicive densities
  VS_zones_BU_ARIMA <- mapply(Variogram_score, Data = X_zones_BU_ARIMA, Real = Testing_eval.list_zones)
  VS_zones_OLS_ARIMA <- mapply(Variogram_score, X_zones_OLS_ARIMA, Real = Testing_eval.list_zones)
  VS_zones_WLS_ARIMA <- mapply(Variogram_score, X_zones_WLS_ARIMA, Real = Testing_eval.list_zones)
  VS_zones_MinT.Shr_ARIMA <- mapply(Variogram_score, X_zones_MinT.Shr_ARIMA, Real = Testing_eval.list_zones)
  VS_zones_OptG_ARIMA <- mapply(Variogram_score, X_zones_OptG_ARIMA, Real = Testing_eval.list_zones)
  VS_zones_Unrecon_ARIMA <- mapply(Variogram_score, X_zones_unrecon_ARIMA, Real = Testing_eval.list_zones)
  
  #Splitting Reconciled means and variances
  Recon_PointF_zones_BU_ARIMA_list <- split(Recon_PointF_zones_BU_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_zones_OLS_ARIMA_list <- split(Recon_PointF_zones_OLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_zones_WLS_ARIMA_list <- split(Recon_PointF_zones_WLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_zones_MinT.Shr_ARIMA_list <- split(Recon_PointF_zones_MinT.Shr_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_zones_OptG_ARIMA_list <- split(Recon_PointF_zones_OptG_ARIMA, 1:min(H, nrow(Testing_eval)))
  
  Recon_Var.Cov_zones_BU_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                              function(x) Recon_Var.Cov_zones_BU_ARIMA)
  Recon_Var.Cov_zones_OLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                               function(x) Recon_Var.Cov_zones_OLS_ARIMA)
  Recon_Var.Cov_zones_WLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                               function(x) Recon_Var.Cov_zones_WLS_ARIMA)
  Recon_Var.Cov_zones_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                    function(x) Recon_Var.Cov_zones_MinT.Shr_ARIMA)
  
  #Calculating Log score for zones predicive densities
  LS_zones_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_BU_ARIMA_list, 
                              Mean = Recon_PointF_zones_BU_ARIMA_list, real = Testing_eval.list_zones)
  LS_zones_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_OLS_ARIMA_list, 
                               Mean = Recon_PointF_zones_OLS_ARIMA_list, real = Testing_eval.list_zones)
  LS_zones_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_WLS_ARIMA_list, 
                               Mean = Recon_PointF_zones_WLS_ARIMA_list, real = Testing_eval.list_zones)
  LS_zones_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_MinT.Shr_ARIMA_list, 
                                    Mean = Recon_PointF_zones_MinT.Shr_ARIMA_list, real = Testing_eval.list_zones)
  LS_zones_OptG_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_OptG_ARIMA, 
                                    Mean = Recon_PointF_zones_OptG_ARIMA_list, real = Testing_eval.list_zones)
  
  #Calculating Energy score for regions predicive densities
  ES_regions_BU_ARIMA <- mapply(Energy_score, Data = X_regions_BU_ARIMA, Real = Testing_eval.list_regions)
  ES_regions_OLS_ARIMA <- mapply(Energy_score, X_regions_OLS_ARIMA, Real = Testing_eval.list_regions)
  ES_regions_WLS_ARIMA <- mapply(Energy_score, X_regions_WLS_ARIMA, Real = Testing_eval.list_regions)
  ES_regions_MinT.Shr_ARIMA <- mapply(Energy_score, X_regions_MinT.Shr_ARIMA, Real = Testing_eval.list_regions)
  ES_regions_OptG_ARIMA <- mapply(Energy_score, X_regions_OptG_ARIMA, Real = Testing_eval.list_regions)
  ES_regions_Unrecon_ARIMA <- mapply(Energy_score, X_regions_unrecon_ARIMA, Real = Testing_eval.list_regions)
  
  #Calculating Variogram score for regions predicive densities
  VS_regions_BU_ARIMA <- mapply(Variogram_score, Data = X_regions_BU_ARIMA, Real = Testing_eval.list_regions)
  VS_regions_OLS_ARIMA <- mapply(Variogram_score, X_regions_OLS_ARIMA, Real = Testing_eval.list_regions)
  VS_regions_WLS_ARIMA <- mapply(Variogram_score, X_regions_WLS_ARIMA, Real = Testing_eval.list_regions)
  VS_regions_MinT.Shr_ARIMA <- mapply(Variogram_score, X_regions_MinT.Shr_ARIMA, Real = Testing_eval.list_regions)
  VS_regions_OptG_ARIMA <- mapply(Variogram_score, X_regions_OptG_ARIMA, Real = Testing_eval.list_regions)
  VS_regions_Unrecon_ARIMA <- mapply(Variogram_score, X_regions_unrecon_ARIMA, Real = Testing_eval.list_regions)
  
  #Splitting Reconciled means and variances
  Recon_PointF_regions_BU_ARIMA_list <- split(Recon_PointF_region_BU_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_regions_OLS_ARIMA_list <- split(Recon_PointF_region_OLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_regions_WLS_ARIMA_list <- split(Recon_PointF_region_WLS_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_regions_MinT.Shr_ARIMA_list <- split(Recon_PointF_region_MinT.Shr_ARIMA, 1:min(H, nrow(Testing_eval)))
  Recon_PointF_regions_OptG_ARIMA_list <- split(Recon_PointF_region_OptG_ARIMA, 1:min(H, nrow(Testing_eval)))
  
  Recon_Var.Cov_regions_BU_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                function(x) Recon_Var.Cov_region_BU_ARIMA)
  Recon_Var.Cov_regions_OLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                 function(x) Recon_Var.Cov_region_OLS_ARIMA)
  Recon_Var.Cov_regions_WLS_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                 function(x) Recon_Var.Cov_region_WLS_ARIMA)
  Recon_Var.Cov_regions_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Testing_eval)), 
                                                      function(x) Recon_Var.Cov_region_MinT.Shr_ARIMA)
  
  #Calculating Log score for regions predicive densities
  LS_regions_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_BU_ARIMA_list, 
                                Mean = Recon_PointF_regions_BU_ARIMA_list, real = Testing_eval.list_regions)
  LS_regions_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_OLS_ARIMA_list, 
                                 Mean = Recon_PointF_regions_OLS_ARIMA_list, real = Testing_eval.list_regions)
  LS_regions_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_WLS_ARIMA_list, 
                                 Mean = Recon_PointF_regions_WLS_ARIMA_list, real = Testing_eval.list_regions)
  LS_regions_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_MinT.Shr_ARIMA_list, 
                                      Mean = Recon_PointF_regions_MinT.Shr_ARIMA_list, real = Testing_eval.list_regions)
  LS_regions_OptG_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_OptG_ARIMA, 
                                      Mean = Recon_PointF_regions_OptG_ARIMA_list, real = Testing_eval.list_regions)
  
  
  
  #Adding to ES and VS full to data frame
  DF_MultiV_Total %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_Unrecon_ARIMA, 
        "Variogram score" = VS_full_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV_Total)] -> DF_Base
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_full_BU_ARIMA, 
        "Variogram score" = VS_full_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Total)] -> DF_BU
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_OLS_ARIMA, 
        "Variogram score" = VS_full_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Total)] -> DF_OLS
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_WLS_ARIMA, 
        "Variogram score" = VS_full_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Total)] -> DF_WLS
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_MinT.Shr_ARIMA, 
        "Variogram score" = VS_full_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Total)] -> DF_MinT.Shr
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "OptimalG", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_OptG_ARIMA, 
        "Variogram score" = VS_full_OptG_ARIMA) -> DF_OptG
  DF_OptG[names(DF_MultiV_Total)] -> DF_OptG
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_OptG)
  
  
  #Adding to ES, VS and LS of states to data frame
  
  DF_MultiV_States %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_Unrecon_ARIMA, 
        "Variogram score" = VS_states_Unrecon_ARIMA, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_States)] -> DF_Base
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_states_BU_ARIMA, 
        "Variogram score" = VS_states_BU_ARIMA, 
        "Log score" = LS_states_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_States)] -> DF_BU
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_OLS_ARIMA, 
        "Variogram score" = VS_states_OLS_ARIMA, 
        "Log score" = LS_states_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_States)] -> DF_OLS
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_WLS_ARIMA, 
        "Variogram score" = VS_states_WLS_ARIMA, 
        "Log score" = LS_states_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_States)] -> DF_WLS
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_MinT.Shr_ARIMA, 
        "Variogram score" = VS_states_MinT.Shr_ARIMA, 
        "Log score" = LS_states_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "OptimalG", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_OptG_ARIMA, 
        "Variogram score" = VS_states_OptG_ARIMA, 
        "Log score" = LS_states_OptG_ARIMA) -> DF_OptG
  DF_OptG[names(DF_MultiV_States)] -> DF_OptG
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_OptG)
  
  
  #Adding to ES, VS and LS of zones to data frame
  
  DF_MultiV_Zones %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_Unrecon_ARIMA, 
        "Variogram score" = VS_zones_Unrecon_ARIMA, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_Zones)] -> DF_Base
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_zones_BU_ARIMA, 
        "Variogram score" = VS_zones_BU_ARIMA, 
        "Log score" = LS_zones_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Zones)] -> DF_BU
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_OLS_ARIMA, 
        "Variogram score" = VS_zones_OLS_ARIMA, 
        "Log score" = LS_zones_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_WLS_ARIMA, 
        "Variogram score" = VS_zones_WLS_ARIMA, 
        "Log score" = LS_zones_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_MinT.Shr_ARIMA, 
        "Variogram score" = VS_zones_MinT.Shr_ARIMA, 
        "Log score" = LS_zones_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "OptimalG", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_OptG_ARIMA, 
        "Variogram score" = VS_zones_OptG_ARIMA, 
        "Log score" = LS_zones_OptG_ARIMA) -> DF_OptG
  DF_OptG[names(DF_MultiV_Zones)] -> DF_OptG
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OptG)
  
  #Adding to ES, VS and LS of regions to data frame
  
  DF_MultiV_Regions %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_Unrecon_ARIMA, 
        "Variogram score" = VS_regions_Unrecon_ARIMA, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_Regions)] -> DF_Base
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_regions_BU_ARIMA, 
        "Variogram score" = VS_regions_BU_ARIMA, 
        "Log score" = LS_regions_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Regions)] -> DF_BU
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_OLS_ARIMA, 
        "Variogram score" = VS_regions_OLS_ARIMA, 
        "Log score" = LS_regions_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_WLS_ARIMA, 
        "Variogram score" = VS_regions_WLS_ARIMA, 
        "Log score" = LS_regions_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_MinT.Shr_ARIMA, 
        "Variogram score" = VS_regions_MinT.Shr_ARIMA, 
        "Log score" = LS_regions_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Regions)] -> DF_MinT.Shr
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "OptimalG", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_OptG_ARIMA, 
        "Variogram score" = VS_regions_OptG_ARIMA, 
        "Log score" = LS_regions_OptG_ARIMA) -> DF_OptG
  DF_OptG[names(DF_MultiV_Regions)] -> DF_OptG
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OptG)
  
  #Addinng CRPS to the DF
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))),  
        "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_Unrecon_ARIMA))) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), 
        "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_BU_ARIMA))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_OLS_ARIMA))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_WLS_ARIMA))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n),
        "CRPS" = c(t(CRPS_MinT.Shr_ARIMA))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), 
        "R-method" = "OptimalG", "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n),
        "CRPS" = c(t(CRPS_OptG_ARIMA))) -> DF_OptG
  DF_OptG[names(DF_UniV)] -> DF_OptG
  DF_UniV <- rbind(DF_UniV, DF_OptG)
  
  End_rest <- Sys.time()

}

End <- Sys.time()



DF_MultiV_Total[complete.cases(DF_MultiV_Total[ , "R-method"]),] -> DF_MultiV_Total
DF_MultiV_States[complete.cases(DF_MultiV_States[ , "R-method"]),] -> DF_MultiV_States
DF_MultiV_Zones[complete.cases(DF_MultiV_Zones[ , "R-method"]),] -> DF_MultiV_Zones
DF_MultiV_Regions[complete.cases(DF_MultiV_Regions[ , "R-method"]),] -> DF_MultiV_Regions

write.csv(DF_MultiV_Total, "Results/DF_MultiV_Total_1-25.csv")
write.csv(DF_MultiV_States, "Results/DF_MultiV_States_1-25.csv")
write.csv(DF_MultiV_Zones, "Results/DF_MultiV_Zones_1-25.csv")
write.csv(DF_MultiV_Regions, "Results/DF_MultiV_Regions_1-25.csv")
write.csv(DF_UniV, "Results/DF_UniV_1-25.csv")

save.image("Results/Forecasting_OvernightTrips_GaussMethod_1-25.RData")
