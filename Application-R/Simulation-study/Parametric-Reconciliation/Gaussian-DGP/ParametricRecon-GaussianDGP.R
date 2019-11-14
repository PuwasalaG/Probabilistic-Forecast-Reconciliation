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

source("Score-functions.R")

set.seed(1989)

start <- Sys.time()

C <- 1000   #C - Length of the outer rolling window
N <- 2002  #N - Length of the original data dagenerated
L <- 500    #L - Length of the rolling window 
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

for (j in 1:C) {
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):nrow(AllTS),]
  
  #To store base forecasts
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  
  Start_fc <- Sys.time()
  for(i in 1:n) {
    
    TS <- ts(Train[,i])
    
    #Forecsting with ARIMA
    fit_ARIMA <- auto.arima(TS) #, stepwise=FALSE,approx=FALSE
    Forecast_ARIMA <- forecast(fit_ARIMA, h = min(H, nrow(Test[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA))
    
  }  
  End_fc <- Sys.time() 
  
  
  ############################################    
  ##Calculating different G matrices##
  ############################################  
  
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
  
  #MinT sample G
  Sam.cov_full_ARIMA <- var(na.omit(ForeError_all_ARIMA))
  Inv_Sam.cov_ARIMA <- solve(Sam.cov_full_ARIMA)
  
  MinT.Sam_G_ARIMA <- solve(t(S) %*% Inv_Sam.cov_ARIMA %*% S) %*% t(S) %*% Inv_Sam.cov_ARIMA
  
  #WLS G
  Inv_WLS_ARIMA <- diag(1/diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  
  WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
  
  
  #######################################  
  ###Mean forecast reconciliation###
  #######################################  
  
  #Reconciled point forecasts for the full hierarchy 
  Recon_PointF_full_BU_ARIMA <- t(S %*% BU_G %*% t(Base_ARIMA))
  Recon_PointF_full_OLS_ARIMA <- t(S %*% OLS_G %*% t(Base_ARIMA))
  Recon_PointF_full_WLS_ARIMA <- t(S %*% WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_full_MinT.Shr_ARIMA <- t(S %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_full_MinT.Sam_ARIMA <- t(S %*% MinT.Sam_G_ARIMA %*% t(Base_ARIMA))
  
  
  #Reconciled bottom level point forecasts#
  Recon_PointF_Bot_BU_ARIMA <- t(BU_G %*% t(Base_ARIMA))
  Recon_PointF_Bot_OLS_ARIMA <- t(OLS_G %*% t(Base_ARIMA))
  Recon_PointF_Bot_WLS_ARIMA <- t(WLS_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_Bot_MinT.Shr_ARIMA <- t(MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_Bot_MinT.Sam_ARIMA <- t(MinT.Sam_G_ARIMA %*% t(Base_ARIMA))
  
  
  ############################################    
  ###Variance forecast reconciliation###
  ############################################  
  
  #Reconciled variance forecasts for the full hierarchy#
  Recon_Var.Cov_full_BU_ARIMA <- S %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S %*% BU_G)
  Recon_Var.Cov_full_OLS_ARIMA <- S %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S %*% OLS_G)
  Recon_Var.Cov_full_WLS_ARIMA <- S %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% WLS_G_ARIMA)
  Recon_Var.Cov_full_MinT.Shr_ARIMA <- S %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% MinT.Shr_G_ARIMA)
  Recon_Var.Cov_full_MinT.Sam_ARIMA <- S %*% MinT.Sam_G_ARIMA %*% Sam.cov_full_ARIMA %*% t(S %*% MinT.Sam_G_ARIMA)
  
  #Reconciled bottom level variance forecasts#
  Recon_Var.Cov_Bot_BU_ARIMA <- BU_G %*% Shr.cov_full_ARIMA %*% t(BU_G)
  Recon_Var.Cov_Bot_OLS_ARIMA <- OLS_G %*% Shr.cov_full_ARIMA %*% t(OLS_G)
  Recon_Var.Cov_Bot_WLS_ARIMA <- WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(WLS_G_ARIMA)
  Recon_Var.Cov_Bot_MinT.Shr_ARIMA <- MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(MinT.Shr_G_ARIMA)
  Recon_Var.Cov_Bot_MinT.Sam_ARIMA <- MinT.Sam_G_ARIMA %*% Sam.cov_full_ARIMA %*% t(MinT.Sam_G_ARIMA)
  
  #List to store random samples from reconciled Gauss distribution of the full hierarchy
  X_full_BU_ARIMA <- list(min(H, nrow(Test)))
  X_full_OLS_ARIMA <- list(min(H, nrow(Test)))
  X_full_WLS_ARIMA <- list(min(H, nrow(Test)))
  X_full_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
  X_full_MinT.Sam_ARIMA <- list(min(H, nrow(Test)))
  X_full_unrecon_ARIMA <- list(min(H, nrow(Test)))
  
  
  #List to store random samples from reconciled Gauss distribution of the bottom levels
  X_Bot_BU_ARIMA <- list(min(H, nrow(Test)))
  X_Bot_OLS_ARIMA <- list(min(H, nrow(Test)))
  X_Bot_WLS_ARIMA <- list(min(H, nrow(Test)))
  X_Bot_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
  X_Bot_MinT.Sam_ARIMA <- list(min(H, nrow(Test)))
  X_Bot_unrecon_ARIMA <- list(min(H, nrow(Test)))
  
  #To store univariate scores
  CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Sam_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  LS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_MinT.Sam_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  
  ###################################
  #Random sample generation#
  ################################### 
  
  #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
  #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
  # function to generate random samples)
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    X_full_BU_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_BU_ARIMA[h,], 
                                             Sigma = Recon_Var.Cov_full_BU_ARIMA, k = B, n = n)
    
    X_full_OLS_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_OLS_ARIMA[h,], 
                                              Sigma = Recon_Var.Cov_full_OLS_ARIMA, k = B, n = n)
    
    X_full_WLS_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_WLS_ARIMA[h,], 
                                              Sigma = Recon_Var.Cov_full_WLS_ARIMA, k = B, n = n)
    
    X_full_MinT.Shr_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Shr_ARIMA[h,], 
                                                   Sigma = Recon_Var.Cov_full_MinT.Shr_ARIMA, k = B, n = n)
    
    X_full_MinT.Sam_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Sam_ARIMA[h,], 
                                                   Sigma = Recon_Var.Cov_full_MinT.Sam_ARIMA, k = B, n = n)
    
    X_full_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h,], Sigma = Sam.cov_full_ARIMA)
    
    
    
    
    #Obtaining random samples from the possible forecast Gaussian densities 
    #of bottom level of the hierarchy
    
    X_Bot_BU_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_BU_ARIMA[h,], 
                                   Sigma = Recon_Var.Cov_Bot_BU_ARIMA)
    
    X_Bot_OLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_OLS_ARIMA[h,], 
                                    Sigma = Recon_Var.Cov_Bot_OLS_ARIMA)
    
    X_Bot_WLS_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_WLS_ARIMA[h,], 
                                    Sigma = Recon_Var.Cov_Bot_WLS_ARIMA)
    
    X_Bot_MinT.Shr_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_MinT.Shr_ARIMA[h,], 
                                         Sigma = Recon_Var.Cov_Bot_MinT.Shr_ARIMA)
    
    X_Bot_MinT.Sam_ARIMA[[h]] <- mvrnorm(n = B, mu = Recon_PointF_Bot_MinT.Sam_ARIMA[h,], 
                                         Sigma = Recon_Var.Cov_Bot_MinT.Sam_ARIMA)
    
    X_Bot_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h, (n-m+1):n], 
                                        Sigma = Sam.cov_full_ARIMA[(n-m+1):n, (n-m+1):n])
    
    #Calculating CRPS for univariate predictive densities

    for (i in 1:n) {

      CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon_ARIMA[[h]][,i],
                                             method = "edf")
      CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU_ARIMA[[h]][,i],
                                        method = "edf")
      CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr_ARIMA[[h]][,i],
                                              method = "edf")
      CRPS_MinT.Sam_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Sam_ARIMA[[h]][,i],
                                              method = "edf")
      
      
      #Calculating LS for univariate predictive densities
      
      LS_Unrecon_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Base_ARIMA[h,i], 
                                      sd = sqrt(Sam.cov_full_ARIMA[i,i]),log = TRUE)
      LS_BU_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_BU_ARIMA[h,i], 
                                 sd = sqrt(Recon_Var.Cov_full_BU_ARIMA[i,i]),log = TRUE)
      LS_OLS_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_OLS_ARIMA[h,i], 
                                  sd = sqrt(Recon_Var.Cov_full_OLS_ARIMA[i,i]),log = TRUE)
      LS_WLS_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_WLS_ARIMA[h,i], 
                                  sd = sqrt(Recon_Var.Cov_full_WLS_ARIMA[i,i]),log = TRUE)
      LS_MinT.Shr_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_MinT.Shr_ARIMA[h,i], 
                                       sd = sqrt(Recon_Var.Cov_full_MinT.Shr_ARIMA[i,i]),log = TRUE)
      LS_MinT.Sam_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_full_MinT.Sam_ARIMA[h,i], 
                                       sd = sqrt(Recon_Var.Cov_full_MinT.Sam_ARIMA[i,i]),log = TRUE)
      

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
  ES_full_BU_ARIMA <- mapply(Energy_score, Data = X_full_BU_ARIMA, Real = Test.list_full)
  ES_full_OLS_ARIMA <- mapply(Energy_score, X_full_OLS_ARIMA, Real = Test.list_full)
  ES_full_WLS_ARIMA <- mapply(Energy_score, X_full_WLS_ARIMA, Real = Test.list_full)
  ES_full_MinT.Shr_ARIMA <- mapply(Energy_score, X_full_MinT.Shr_ARIMA, Real = Test.list_full)
  ES_full_MinT.Sam_ARIMA <- mapply(Energy_score, X_full_MinT.Sam_ARIMA, Real = Test.list_full)
  ES_full_Unrecon_ARIMA <- mapply(Energy_score, X_full_unrecon_ARIMA, Real = Test.list_full)
  
  #Calculating Variogram score for full predicive densities
  VS_full_BU_ARIMA <- mapply(Variogram_score, Data = X_full_BU_ARIMA, Real = Test.list_full)
  VS_full_OLS_ARIMA <- mapply(Variogram_score, X_full_OLS_ARIMA, Real = Test.list_full)
  VS_full_WLS_ARIMA <- mapply(Variogram_score, X_full_WLS_ARIMA, Real = Test.list_full)
  VS_full_MinT.Shr_ARIMA <- mapply(Variogram_score, X_full_MinT.Shr_ARIMA, Real = Test.list_full)
  VS_full_MinT.Sam_ARIMA <- mapply(Variogram_score, X_full_MinT.Sam_ARIMA, Real = Test.list_full)
  VS_full_Unrecon_ARIMA <- mapply(Variogram_score, X_full_unrecon_ARIMA, Real = Test.list_full)
  
  
  #Calculating Energy score for Bot predicive densities
  ES_Bot_BU_ARIMA <- mapply(Energy_score, Data = X_Bot_BU_ARIMA, Real = Test.list_Bot)
  ES_Bot_OLS_ARIMA <- mapply(Energy_score, X_Bot_OLS_ARIMA, Real = Test.list_Bot)
  ES_Bot_WLS_ARIMA <- mapply(Energy_score, X_Bot_WLS_ARIMA, Real = Test.list_Bot)
  ES_Bot_MinT.Shr_ARIMA <- mapply(Energy_score, X_Bot_MinT.Shr_ARIMA, Real = Test.list_Bot)
  ES_Bot_MinT.Sam_ARIMA <- mapply(Energy_score, X_Bot_MinT.Sam_ARIMA, Real = Test.list_Bot)
  ES_Bot_Unrecon_ARIMA <- mapply(Energy_score, X_Bot_unrecon_ARIMA, Real = Test.list_Bot)
  
  #Calculating Variogram score for Bot predicive densities
  VS_Bot_BU_ARIMA <- mapply(Variogram_score, Data = X_Bot_BU_ARIMA, Real = Test.list_Bot)
  VS_Bot_OLS_ARIMA <- mapply(Variogram_score, X_Bot_OLS_ARIMA, Real = Test.list_Bot)
  VS_Bot_WLS_ARIMA <- mapply(Variogram_score, X_Bot_WLS_ARIMA, Real = Test.list_Bot)
  VS_Bot_MinT.Shr_ARIMA <- mapply(Variogram_score, X_Bot_MinT.Shr_ARIMA, Real = Test.list_Bot)
  VS_Bot_MinT.Sam_ARIMA <- mapply(Variogram_score, X_Bot_MinT.Sam_ARIMA, Real = Test.list_Bot)
  VS_Bot_Unrecon_ARIMA <- mapply(Variogram_score, X_Bot_unrecon_ARIMA, Real = Test.list_Bot)
  
  #Splitting Reconciled means and variances
  Recon_PointF_Bot_BU_ARIMA_list <- split(Recon_PointF_Bot_BU_ARIMA, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_OLS_ARIMA_list <- split(Recon_PointF_Bot_OLS_ARIMA, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_WLS_ARIMA_list <- split(Recon_PointF_Bot_WLS_ARIMA, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_MinT.Shr_ARIMA_list <- split(Recon_PointF_Bot_MinT.Shr_ARIMA, 1:min(H, nrow(Test)))
  Recon_PointF_Bot_MinT.Sam_ARIMA_list <- split(Recon_PointF_Bot_MinT.Sam_ARIMA, 1:min(H, nrow(Test)))
  
  Recon_Var.Cov_Bot_BU_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                            function(x) Recon_Var.Cov_Bot_BU_ARIMA)
  Recon_Var.Cov_Bot_OLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                             function(x) Recon_Var.Cov_Bot_OLS_ARIMA)
  Recon_Var.Cov_Bot_WLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                             function(x) Recon_Var.Cov_Bot_WLS_ARIMA)
  Recon_Var.Cov_Bot_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_Bot_MinT.Shr_ARIMA)
  Recon_Var.Cov_Bot_MinT.Sam_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_Bot_MinT.Sam_ARIMA)
  
  #Calculating Log score for Bot predicive densities
  LS_Bot_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_BU_ARIMA_list, 
                            Mean = Recon_PointF_Bot_BU_ARIMA_list, real = Test.list_Bot)
  LS_Bot_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_OLS_ARIMA_list, 
                             Mean = Recon_PointF_Bot_OLS_ARIMA_list, real = Test.list_Bot)
  LS_Bot_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_WLS_ARIMA_list, 
                             Mean = Recon_PointF_Bot_WLS_ARIMA_list, real = Test.list_Bot)
  LS_Bot_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_MinT.Shr_ARIMA_list, 
                                  Mean = Recon_PointF_Bot_MinT.Shr_ARIMA_list, real = Test.list_Bot)
  LS_Bot_MinT.Sam_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_Bot_MinT.Sam_ARIMA_list, 
                                  Mean = Recon_PointF_Bot_MinT.Sam_ARIMA_list, real = Test.list_Bot)
  
  
  
  #Adding to ES and VS full to data frame
  DF_MultiV_Full %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_Unrecon_ARIMA, 
        "Variogram score" = VS_full_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV_Full)] -> DF_Base
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
        "Energy score" = ES_full_BU_ARIMA, 
        "Variogram score" = VS_full_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Full)] -> DF_BU
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_OLS_ARIMA, 
        "Variogram score" = VS_full_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Full)] -> DF_OLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_WLS_ARIMA, 
        "Variogram score" = VS_full_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Full)] -> DF_WLS
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_MinT.Shr_ARIMA, 
        "Variogram score" = VS_full_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Full)] -> DF_MinT.Shr
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_full_MinT.Sam_ARIMA, 
        "Variogram score" = VS_full_MinT.Sam_ARIMA) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Full)] -> DF_MinT.Sam
  DF_MultiV_Full <- rbind(DF_MultiV_Full, DF_MinT.Sam)
  
  #Adding to ES, VS and LS of Bot to data frame
  
  DF_MultiV_Bot %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_Unrecon_ARIMA, 
        "Variogram score" = VS_Bot_Unrecon_ARIMA, 
        "Log score" = NA) -> DF_Base
  DF_Base[names(DF_MultiV_Bot)] -> DF_Base
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
        "Energy score" = ES_Bot_BU_ARIMA, 
        "Variogram score" = VS_Bot_BU_ARIMA, 
        "Log score" = LS_Bot_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Bot)] -> DF_BU
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_OLS_ARIMA, 
        "Variogram score" = VS_Bot_OLS_ARIMA, 
        "Log score" = LS_Bot_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Bot)] -> DF_OLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_WLS_ARIMA, 
        "Variogram score" = VS_Bot_WLS_ARIMA, 
        "Log score" = LS_Bot_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Bot)] -> DF_WLS
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_MinT.Shr_ARIMA, 
        "Variogram score" = VS_Bot_MinT.Shr_ARIMA, 
        "Log score" = LS_Bot_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Bot)] -> DF_MinT.Shr
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "Energy score" = ES_Bot_MinT.Sam_ARIMA, 
        "Variogram score" = VS_Bot_MinT.Sam_ARIMA, 
        "Log score" = LS_Bot_MinT.Sam_ARIMA) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_MultiV_Bot)] -> DF_MinT.Sam
  DF_MultiV_Bot <- rbind(DF_MultiV_Bot, DF_MinT.Sam)
  
  
  
  
  #Adding to CRPS and LS of univariate series to data frame
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication", "Series") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_Unrecon_ARIMA), 
        "LS" = as.numeric(LS_Unrecon_ARIMA)) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_BU_ARIMA), 
        "LS" = as.numeric(LS_BU_ARIMA)) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_OLS_ARIMA), 
        "LS" = as.numeric(LS_OLS_ARIMA)) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_WLS_ARIMA), 
        "LS" = as.numeric(LS_WLS_ARIMA)) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_MinT.Shr_ARIMA), 
        "LS" = as.numeric(LS_MinT.Shr_ARIMA)) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "MinT.Sam", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
        "CRPS" = as.numeric(CRPS_MinT.Sam_ARIMA), 
        "LS" = as.numeric(LS_MinT.Sam_ARIMA)) -> DF_MinT.Sam
  DF_MinT.Sam[names(DF_UniV)] -> DF_MinT.Sam
  DF_UniV <- rbind(DF_UniV, DF_MinT.Sam)
  
}

End <- Sys.time()



DF_MultiV_Full[complete.cases(DF_MultiV_Full[ , "R-method"]),] -> DF_MultiV_Full
DF_MultiV_Bot[complete.cases(DF_MultiV_Bot[ , "R-method"]),] -> DF_MultiV_Bot
DF_UniV[complete.cases(DF_UniV[ , "R-method"]),] -> DF_UniV


write.csv(DF_MultiV_Full, "Results/DF_MultiV_Full_GaussianDGP.csv")
write.csv(DF_MultiV_Bot, "Results/DF_MultiV_Bot_GaussianDGP.csv")
write.csv(DF_UniV, "Results/DF_UniV_GaussianDGP.csv")

save.image("Results/ParametricRecon-GaussianDGP.RData")
