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

set.seed(1989)

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

L <- 100 #Size of the training window
p <- nrow(AllTS) - L #number of replications
B <- 2500 # Number of random numbers generated from the predictive distributions
H <- 12 #Forecast horizones

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

# Lists to save means and covariances of Gaussian Fdists

Unrecon_mean_ETS <- list()
Unrecon_mean_ARIMA <- list()

Recon_BU_mean_ETS <- list()
Recon_BU_mean_ARIMA <- list()

Recon_OLS_mean_ETS <- list()
Recon_OLS_mean_ARIMA <- list()

Recon_WLS_mean_ETS <- list()
Recon_WLS_mean_ARIMA <- list()

Recon_MinT_mean_ETS <- list()
Recon_MinT_mean_ARIMA <- list()

Unrecon_Cov_ETS <- list()
Unrecon_Cov_ARIMA <- list()

Recon_BU_Cov_ETS <- list()
Recon_BU_Cov_ARIMA <- list()

Recon_OLS_Cov_ETS <- list()
Recon_OLS_Cov_ARIMA <- list()

Recon_WLS_Cov_ETS <- list()
Recon_WLS_Cov_ARIMA <- list()

Recon_MinT_Cov_ETS <- list()
Recon_MinT_Cov_ARIMA <- list()

Start <- Sys.time()

for (j in 1:p) {#p #??59
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):nrow(AllTS),]
  
  #To store base forecasts
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_ETS <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  ForeError_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  
  Start_fc <- Sys.time()
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 12)
    
    ##Forecsting with ETS##
    fit_ETS <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS, h = min(H, nrow(Test)))
    Base_ETS[,i] <- Forecast_ETS$mean
    ForeError_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS))
    
    
    #Forecsting with ARIMA
    fit_ARIMA <- auto.arima(TS) #, stepwise=FALSE,approx=FALSE
    Forecast_ARIMA <- forecast(fit_ARIMA, h = min(H, nrow(Test[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA))
    
  }  
  End_fc <- Sys.time() 
  
  
  ###--Calculating different G matrices required for reconciliation of ETS base forecasts--###
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  #MinT shrink G
  targ <- diag(diag(var(na.omit(ForeError_all_ETS))), n, n)
  shrink <- shrink.estim(na.omit(ForeError_all_ETS),targ)
  Shr.cov_full_ETS <- shrink[[1]]
  Inv_Shr.cov_ETS <- solve(Shr.cov_full_ETS)
  
  MinT.Shr_G_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
  
  #WLS G
  Inv_WLS_ETS <- diag(1/diag(var(na.omit(ForeError_all_ETS))), n, n)
  
  WLS_G_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
  
  
  
  ###Mean forecast reconciliation (from ETS base)###
  
  #Reconciled point forecasts for the full hierarchy 
  Recon_PointF_full_BU_ETS <- t(S %*% BU_G %*% t(Base_ETS))
  Recon_PointF_full_OLS_ETS <- t(S %*% OLS_G %*% t(Base_ETS))
  Recon_PointF_full_WLS_ETS <- t(S %*% WLS_G_ETS %*% t(Base_ETS))
  Recon_PointF_full_MinT.Shr_ETS <- t(S %*% MinT.Shr_G_ETS %*% t(Base_ETS))
  
 
  
  ###Variance forecast reconciliation (from ETS base)###
  
  
  #Reconciled variance forecasts for the full hierarchy (Follows from ETS)#
  Recon_Var.Cov_full_BU_ETS <- S %*% BU_G %*% Shr.cov_full_ETS %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS_ETS <- S %*% OLS_G %*% Shr.cov_full_ETS %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS_ETS <- S %*% WLS_G_ETS %*% Shr.cov_full_ETS %*% t(S %*% WLS_G_ETS)
  Recon_Var.Cov_full_MinT.Shr_ETS <- S %*% MinT.Shr_G_ETS %*% Shr.cov_full_ETS %*% t(S %*% MinT.Shr_G_ETS)
  
 
  
  
  ##Calculating different G matrices required for reconciliation of ARIMA base forecasts##
  
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
  

  
  ###Variance forecast reconciliation (from ARIMA base)###
  
  
  #Reconciled variance forecasts for the full hierarchy (Followed from ARIMA)#
  Recon_Var.Cov_full_BU_ARIMA <- S %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS_ARIMA <- S %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS_ARIMA <- S %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% WLS_G_ARIMA)
  Recon_Var.Cov_full_MinT.Shr_ARIMA <- S %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% MinT.Shr_G_ARIMA)
  
  Unrecon_mean_ETS[[j]] <- Base_ETS
  Unrecon_mean_ARIMA[[j]] <- Base_ARIMA
  
  Recon_BU_mean_ETS[[j]] <- Recon_PointF_full_BU_ETS
  Recon_BU_mean_ARIMA[[j]] <- Recon_PointF_full_BU_ARIMA
  
  Recon_OLS_mean_ETS[[j]] <- Recon_PointF_full_OLS_ETS
  Recon_OLS_mean_ARIMA[[j]] <- Recon_PointF_full_OLS_ARIMA
  
  Recon_WLS_mean_ETS[[j]] <- Recon_PointF_full_WLS_ETS
  Recon_WLS_mean_ARIMA[[j]] <- Recon_PointF_full_WLS_ARIMA
  
  Recon_MinT_mean_ETS[[j]] <- Recon_PointF_full_MinT.Shr_ETS
  Recon_MinT_mean_ARIMA[[j]] <- Recon_PointF_full_MinT.Shr_ARIMA
  
  Unrecon_Cov_ETS[[j]] <- Shr.cov_full_ARIMA
  Unrecon_Cov_ARIMA[[j]] <- Shr.cov_full_ETS
  
  Recon_BU_Cov_ETS[[j]] <- Recon_Var.Cov_full_BU_ETS
  Recon_BU_Cov_ARIMA[[j]] <- Recon_Var.Cov_full_BU_ARIMA
  
  Recon_OLS_Cov_ETS[[j]] <- Recon_Var.Cov_full_OLS_ETS
  Recon_OLS_Cov_ARIMA[[j]] <- Recon_Var.Cov_full_OLS_ARIMA
  
  Recon_WLS_Cov_ETS[[j]] <- Recon_Var.Cov_full_WLS_ETS
  Recon_WLS_Cov_ARIMA[[j]] <- Recon_Var.Cov_full_WLS_ARIMA
  
  Recon_MinT_Cov_ETS[[j]] <- Recon_Var.Cov_full_MinT.Shr_ETS
  Recon_MinT_Cov_ARIMA[[j]] <- Recon_Var.Cov_full_MinT.Shr_ARIMA
  
  

}

End <- Sys.time()




save.image("Results/Forecasting_OvernightTrips_GaussMethod_Fdistributions.RData")
