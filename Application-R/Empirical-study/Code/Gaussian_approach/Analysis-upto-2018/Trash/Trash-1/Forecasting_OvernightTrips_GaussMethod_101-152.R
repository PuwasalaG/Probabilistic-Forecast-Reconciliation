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
H <- 6 #Forecast horizones

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

for (j in 101:152) {#p #??59
  
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
    fit_ARIMA <- auto.arima(TS)
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
    targ <- lowerD(ForeError_all_ETS)
    shrink <- shrink.estim(ForeError_all_ETS,targ)
    Shr.cov_full_ETS <- shrink[[1]]
    Inv_Shr.cov_ETS <- solve(Shr.cov_full_ETS)
    
    MinT.Shr_G_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
    
    #WLS G
    Cov_WLS_ETS <- diag(diag(Shr.cov_full_ETS), n, n)
    Inv_WLS_ETS <- solve(Cov_WLS_ETS)
    
    WLS_G_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
    
    
    
    ###Mean forecast reconciliation (from ETS base)###
    
    #Reconciled point forecasts for the full hierarchy 
    Recon_PointF_full_BU_ETS <- t(S %*% BU_G %*% t(Base_ETS))
    Recon_PointF_full_OLS_ETS <- t(S %*% OLS_G %*% t(Base_ETS))
    Recon_PointF_full_WLS_ETS <- t(S %*% WLS_G_ETS %*% t(Base_ETS))
    Recon_PointF_full_MinT.Shr_ETS <- t(S %*% MinT.Shr_G_ETS %*% t(Base_ETS))
    
    #Reconciled point forecasts for the states of the hierarchy 
    Recon_PointF_states_BU_ETS <- t(S_state %*% BU_G %*% t(Base_ETS))
    Recon_PointF_states_OLS_ETS <- t(S_state %*% OLS_G %*% t(Base_ETS))
    Recon_PointF_states_WLS_ETS <- t(S_state %*% WLS_G_ETS %*% t(Base_ETS))
    Recon_PointF_states_MinT.Shr_ETS <- t(S_state %*% MinT.Shr_G_ETS %*% t(Base_ETS))
    
    #Reconciled point forecasts for the zones of the hierarchy 
    Recon_PointF_zones_BU_ETS <- t(S_zone %*% BU_G %*% t(Base_ETS))
    Recon_PointF_zones_OLS_ETS <- t(S_zone %*% OLS_G %*% t(Base_ETS))
    Recon_PointF_zones_WLS_ETS <- t(S_zone %*% WLS_G_ETS %*% t(Base_ETS))
    Recon_PointF_zones_MinT.Shr_ETS <- t(S_zone %*% MinT.Shr_G_ETS %*% t(Base_ETS))
    
    Recon_PointF_region_BU_ETS <- t(BU_G %*% t(Base_ETS))
    Recon_PointF_region_OLS_ETS <- t(OLS_G %*% t(Base_ETS))
    Recon_PointF_region_WLS_ETS <- t(WLS_G_ETS %*% t(Base_ETS))
    Recon_PointF_region_MinT.Shr_ETS <- t(MinT.Shr_G_ETS %*% t(Base_ETS))
    
    
    ###Variance forecast reconciliation (from ETS base)###
    
    
    #Reconciled variance forecasts for the full hierarchy (Follows from ETS)#
    Recon_Var.Cov_full_BU_ETS <- S %*% BU_G %*% Shr.cov_full_ETS %*% t(S %*% BU_G)
    Recon_Var.Cov_full_OLS_ETS <- S %*% OLS_G %*% Shr.cov_full_ETS %*% t(S %*% OLS_G)
    Recon_Var.Cov_full_WLS_ETS <- S %*% WLS_G_ETS %*% Shr.cov_full_ETS %*% t(S %*% WLS_G_ETS)
    Recon_Var.Cov_full_MinT.Shr_ETS <- S %*% MinT.Shr_G_ETS %*% Shr.cov_full_ETS %*% t(S %*% MinT.Shr_G_ETS)
    
    #Reconciled variance forecasts for the states hierarchy (Follows from ETS)#
    Recon_Var.Cov_states_BU_ETS <- S_state %*% BU_G %*% Shr.cov_full_ETS %*% t(S_state %*% BU_G)
    Recon_Var.Cov_states_OLS_ETS <- S_state %*% OLS_G %*% Shr.cov_full_ETS %*% t(S_state %*% OLS_G)
    Recon_Var.Cov_states_WLS_ETS <- S_state %*% WLS_G_ETS %*% Shr.cov_full_ETS %*% t(S_state %*% WLS_G_ETS)
    Recon_Var.Cov_states_MinT.Shr_ETS <- S_state %*% MinT.Shr_G_ETS %*% Shr.cov_full_ETS %*% t(S_state %*% MinT.Shr_G_ETS)
    
    #Reconciled variance forecasts for the zones hierarchy (Follows from ETS)#
    Recon_Var.Cov_zones_BU_ETS <- S_zone %*% BU_G %*% Shr.cov_full_ETS %*% t(S_zone %*% BU_G)
    Recon_Var.Cov_zones_OLS_ETS <- S_zone %*% OLS_G %*% Shr.cov_full_ETS %*% t(S_zone %*% OLS_G)
    Recon_Var.Cov_zones_WLS_ETS <- S_zone %*% WLS_G_ETS %*% Shr.cov_full_ETS %*% t(S_zone %*% WLS_G_ETS)
    Recon_Var.Cov_zones_MinT.Shr_ETS <- S_zone %*% MinT.Shr_G_ETS %*% Shr.cov_full_ETS %*% t(S_zone %*% MinT.Shr_G_ETS)
    
    #Reconciled bottom level variance forecasts (Follows from ETS)#
    Recon_Var.Cov_region_BU_ETS <- BU_G %*% Shr.cov_full_ETS %*% t(BU_G)
    Recon_Var.Cov_region_OLS_ETS <- OLS_G %*% Shr.cov_full_ETS %*% t(OLS_G)
    Recon_Var.Cov_region_WLS_ETS <- WLS_G_ETS %*% Shr.cov_full_ETS %*% t(WLS_G_ETS)
    Recon_Var.Cov_region_MinT.Shr_ETS <- MinT.Shr_G_ETS %*% Shr.cov_full_ETS %*% t(MinT.Shr_G_ETS)

    #List to store random samples from reconciled Gauss distribution of the full hierarchy
    X_full_BU_ETS <- list(min(H, nrow(Test)))
    X_full_OLS_ETS <- list(min(H, nrow(Test)))
    X_full_WLS_ETS <- list(min(H, nrow(Test)))
    X_full_MinT.Shr_ETS <- list(min(H, nrow(Test)))
    X_full_unrecon_ETS <- list(min(H, nrow(Test)))
    
    
    #List to store random samples from reconciled Gauss distribution of the state levels
    X_states_BU_ETS <- list(min(H, nrow(Test)))
    X_states_OLS_ETS <- list(min(H, nrow(Test)))
    X_states_WLS_ETS <- list(min(H, nrow(Test)))
    X_states_MinT.Shr_ETS <- list(min(H, nrow(Test)))
    X_states_unrecon_ETS <- list(min(H, nrow(Test)))
    
    #List to store random samples from reconciled Gauss distribution of the Zone levels
    X_zones_BU_ETS <- list(min(H, nrow(Test)))
    X_zones_OLS_ETS <- list(min(H, nrow(Test)))
    X_zones_WLS_ETS <- list(min(H, nrow(Test)))
    X_zones_MinT.Shr_ETS <- list(min(H, nrow(Test)))
    X_zones_unrecon_ETS <- list(min(H, nrow(Test)))
    
    #List to store random samples from reconciled Gauss distribution of the Region levels
    X_regions_BU_ETS <- list(min(H, nrow(Test)))
    X_regions_OLS_ETS <- list(min(H, nrow(Test)))
    X_regions_WLS_ETS <- list(min(H, nrow(Test)))
    X_regions_MinT.Shr_ETS <- list(min(H, nrow(Test)))
    X_regions_unrecon_ETS <- list(min(H, nrow(Test)))
    
    #To store univariate scores
    CRPS_BU_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_OLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_WLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_MinT.Shr_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_Unrecon_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    
    
    for (h in 1: min(H, nrow(Test))) {
      
      #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
      #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
      # function to generate random samples)
      
      X_full_BU_ETS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_BU_ETS[h,], 
                                        Sigma = Recon_Var.Cov_full_BU_ETS, k = B, n = n)
    
      X_full_OLS_ETS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_OLS_ETS[h,], 
                                         Sigma = Recon_Var.Cov_full_OLS_ETS, k = B, n = n)
      
      X_full_WLS_ETS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_WLS_ETS[h,], 
                                         Sigma = Recon_Var.Cov_full_WLS_ETS, k = B, n = n)
      
      X_full_MinT.Shr_ETS[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Shr_ETS[h,], 
                                              Sigma = Recon_Var.Cov_full_MinT.Shr_ETS, k = B, n = n)
      
      X_full_unrecon_ETS[[h]] <- mvrnorm(n = B, mu = Base_ETS[h,], Sigma = Shr.cov_full_ETS)
      
      
      
      
      #Obtaining random samples from the possible forecast Gaussian densities 
      #of state level of the hierarchy
      
      X_states_BU_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_BU_ETS[h,], 
                                        Sigma = Recon_Var.Cov_states_BU_ETS)
      
      X_states_OLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_OLS_ETS[h,], 
                                         Sigma = Recon_Var.Cov_states_OLS_ETS)
      
      X_states_WLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_WLS_ETS[h,], 
                                         Sigma = Recon_Var.Cov_states_WLS_ETS)
      
      X_states_MinT.Shr_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_states_MinT.Shr_ETS[h,], 
                                              Sigma = Recon_Var.Cov_states_MinT.Shr_ETS)
      
      X_states_unrecon_ETS[[h]] <- mvrnorm(n = B, mu = Base_ETS[h,(l1+1):(l1+l2)], 
                                           Sigma = Shr.cov_full_ETS[(l1+1):(l1+l2), (l1+1):(l1+l2)])
      
      #Obtaining random samples from the possible forecast Gaussian densities 
      #of zone level of the hierarchy
      
      X_zones_BU_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_BU_ETS[h,], 
                                          Sigma = Recon_Var.Cov_zones_BU_ETS)
      
      X_zones_OLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_OLS_ETS[h,], 
                                           Sigma = Recon_Var.Cov_zones_OLS_ETS)
      
      X_zones_WLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_WLS_ETS[h,], 
                                           Sigma = Recon_Var.Cov_zones_WLS_ETS)
      
      X_zones_MinT.Shr_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_zones_MinT.Shr_ETS[h,], 
                                                Sigma = Recon_Var.Cov_zones_MinT.Shr_ETS)
      
      X_zones_unrecon_ETS[[h]] <- mvrnorm(n = B, mu = Base_ETS[h,(l1+l2+1):(l1+l2+l3)], 
                                     Sigma = Shr.cov_full_ETS[(l1+l2+1):(l1+l2+l3), (l1+l2+1):(l1+l2+l3)])
      
      #Obtaining random samples from the possible forecast Gaussian densities 
      #of zone level of the hierarchy
      
      X_regions_BU_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_BU_ETS[h,], 
                                Sigma = Recon_Var.Cov_region_BU_ETS)
      
      X_regions_OLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_OLS_ETS[h,], 
                                 Sigma = Recon_Var.Cov_region_OLS_ETS)
      
      X_regions_WLS_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_WLS_ETS[h,], 
                                 Sigma = Recon_Var.Cov_region_WLS_ETS)
      
      X_regions_MinT.Shr_ETS[[h]] <- mvrnorm(n = B, mu = Recon_PointF_region_MinT.Shr_ETS[h,], 
                                      Sigma = Recon_Var.Cov_region_MinT.Shr_ETS)
      
      X_regions_unrecon_ETS[[h]] <- mvrnorm(n = B, mu = Base_ETS[h, (n-m+1):n], 
                                     Sigma = Shr.cov_full_ETS[(n-m+1):n, (n-m+1):n])
      
      #Calculating CRPS for univariate predictive densities
      
      for (i in 1:n) {
        
        CRPS_Unrecon_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon_ETS[[h]][,i],
                                             method = "edf")
        CRPS_BU_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU_ETS[[h]][,i],
                                        method = "edf")
        CRPS_OLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS_ETS[[h]][,i],
                                         method = "edf")
        CRPS_WLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS_ETS[[h]][,i],
                                         method = "edf")
        CRPS_MinT.Shr_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr_ETS[[h]][,i],
                                              method = "edf")
        
      }
      
      
      DF_MultiV_Total <- DF_MultiV_Total %>% add_row("F-method" = "ETS",
                                         "Replication" = j)
      
      DF_MultiV_States <- DF_MultiV_States %>% add_row("F-method" = "ETS",
                                         "Replication" = j)
      
      DF_MultiV_Zones <- DF_MultiV_Zones %>% add_row("F-method" = "ETS",
                                         "Replication" = j)
      
      DF_MultiV_Regions <- DF_MultiV_Regions %>% add_row("F-method" = "ETS",
                                         "Replication" = j)
      
      DF_UniV <- DF_UniV %>% add_row("F-method" = "ETS",
                                     "Replication" = rep(j, n*min(H, nrow(Test))))
      
    }
    
    
    Test.list_full <- split(Test[1:min(H, nrow(Test)),], 1:min(H, nrow(Test)))
    Test.list_states <- split(Test[1:min(H, nrow(Test)),(l1+1):(l1+l2)], 1:min(H, nrow(Test)))
    Test.list_zones <- split(Test[1:min(H, nrow(Test)),(l1+l2+1):(l1+l2+l3)], 1:min(H, nrow(Test)))
    Test.list_regions <- split(Test[1:min(H, nrow(Test)),(n-m+1):n], 1:min(H, nrow(Test)))
    
    #Calculating Energy score for full predicive densities
    ES_full_BU_ETS <- mapply(Energy_score, Data = X_full_BU_ETS, Real = Test.list_full)
    ES_full_OLS_ETS <- mapply(Energy_score, X_full_OLS_ETS, Real = Test.list_full)
    ES_full_WLS_ETS <- mapply(Energy_score, X_full_WLS_ETS, Real = Test.list_full)
    ES_full_MinT.Shr_ETS <- mapply(Energy_score, X_full_MinT.Shr_ETS, Real = Test.list_full)
    ES_full_Unrecon_ETS <- mapply(Energy_score, X_full_unrecon_ETS, Real = Test.list_full)
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ETS <- mapply(Variogram_score, Data = X_full_BU_ETS, Real = Test.list_full)
    VS_full_OLS_ETS <- mapply(Variogram_score, X_full_OLS_ETS, Real = Test.list_full)
    VS_full_WLS_ETS <- mapply(Variogram_score, X_full_WLS_ETS, Real = Test.list_full)
    VS_full_MinT.Shr_ETS <- mapply(Variogram_score, X_full_MinT.Shr_ETS, Real = Test.list_full)
    VS_full_Unrecon_ETS <- mapply(Variogram_score, X_full_unrecon_ETS, Real = Test.list_full)
    
    #Calculating Energy score for states predicive densities
    ES_states_BU_ETS <- mapply(Energy_score, Data = X_states_BU_ETS, Real = Test.list_states)
    ES_states_OLS_ETS <- mapply(Energy_score, X_states_OLS_ETS, Real = Test.list_states)
    ES_states_WLS_ETS <- mapply(Energy_score, X_states_WLS_ETS, Real = Test.list_states)
    ES_states_MinT.Shr_ETS <- mapply(Energy_score, X_states_MinT.Shr_ETS, Real = Test.list_states)
    ES_states_Unrecon_ETS <- mapply(Energy_score, X_states_unrecon_ETS, Real = Test.list_states)
    
    #Calculating Variogram score for states predicive densities
    VS_states_BU_ETS <- mapply(Variogram_score, Data = X_states_BU_ETS, Real = Test.list_states)
    VS_states_OLS_ETS <- mapply(Variogram_score, X_states_OLS_ETS, Real = Test.list_states)
    VS_states_WLS_ETS <- mapply(Variogram_score, X_states_WLS_ETS, Real = Test.list_states)
    VS_states_MinT.Shr_ETS <- mapply(Variogram_score, X_states_MinT.Shr_ETS, Real = Test.list_states)
    VS_states_Unrecon_ETS <- mapply(Variogram_score, X_states_unrecon_ETS, Real = Test.list_states)
    
    #Splitting Reconciled means and variances
    Recon_PointF_states_BU_ETS_list <- split(Recon_PointF_states_BU_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_states_OLS_ETS_list <- split(Recon_PointF_states_OLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_states_WLS_ETS_list <- split(Recon_PointF_states_WLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_states_MinT.Shr_ETS_list <- split(Recon_PointF_states_MinT.Shr_ETS, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_states_BU_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                               function(x) Recon_Var.Cov_states_BU_ETS)
    Recon_Var.Cov_states_OLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                function(x) Recon_Var.Cov_states_OLS_ETS)
    Recon_Var.Cov_states_WLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                function(x) Recon_Var.Cov_states_WLS_ETS)
    Recon_Var.Cov_states_MinT.Shr_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                function(x) Recon_Var.Cov_states_MinT.Shr_ETS)
    
    #Calculating Log score for states predicive densities
    LS_states_BU_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_states_BU_ETS_list, 
                               Mean = Recon_PointF_states_BU_ETS_list, real = Test.list_states)
    LS_states_OLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_states_OLS_ETS_list, 
                                Mean = Recon_PointF_states_OLS_ETS_list, real = Test.list_states)
    LS_states_WLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_states_WLS_ETS_list, 
                                Mean = Recon_PointF_states_WLS_ETS_list, real = Test.list_states)
    LS_states_MinT.Shr_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_states_MinT.Shr_ETS_list, 
                                     Mean = Recon_PointF_states_MinT.Shr_ETS_list, real = Test.list_states)

    #Calculating Energy score for zones predicive densities
    ES_zones_BU_ETS <- mapply(Energy_score, Data = X_zones_BU_ETS, Real = Test.list_zones)
    ES_zones_OLS_ETS <- mapply(Energy_score, X_zones_OLS_ETS, Real = Test.list_zones)
    ES_zones_WLS_ETS <- mapply(Energy_score, X_zones_WLS_ETS, Real = Test.list_zones)
    ES_zones_MinT.Shr_ETS <- mapply(Energy_score, X_zones_MinT.Shr_ETS, Real = Test.list_zones)
    ES_zones_Unrecon_ETS <- mapply(Energy_score, X_zones_unrecon_ETS, Real = Test.list_zones)
    
    #Calculating Variogram score for zones predicive densities
    VS_zones_BU_ETS <- mapply(Variogram_score, Data = X_zones_BU_ETS, Real = Test.list_zones)
    VS_zones_OLS_ETS <- mapply(Variogram_score, X_zones_OLS_ETS, Real = Test.list_zones)
    VS_zones_WLS_ETS <- mapply(Variogram_score, X_zones_WLS_ETS, Real = Test.list_zones)
    VS_zones_MinT.Shr_ETS <- mapply(Variogram_score, X_zones_MinT.Shr_ETS, Real = Test.list_zones)
    VS_zones_Unrecon_ETS <- mapply(Variogram_score, X_zones_unrecon_ETS, Real = Test.list_zones)
    
    #Splitting Reconciled means and variances
    Recon_PointF_zones_BU_ETS_list <- split(Recon_PointF_zones_BU_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_zones_OLS_ETS_list <- split(Recon_PointF_zones_OLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_zones_WLS_ETS_list <- split(Recon_PointF_zones_WLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_zones_MinT.Shr_ETS_list <- split(Recon_PointF_zones_MinT.Shr_ETS, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_zones_BU_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                              function(x) Recon_Var.Cov_zones_BU_ETS)
    Recon_Var.Cov_zones_OLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                               function(x) Recon_Var.Cov_zones_OLS_ETS)
    Recon_Var.Cov_zones_WLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                               function(x) Recon_Var.Cov_zones_WLS_ETS)
    Recon_Var.Cov_zones_MinT.Shr_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                    function(x) Recon_Var.Cov_zones_MinT.Shr_ETS)
    
    #Calculating Log score for zones predicive densities
    LS_zones_BU_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_BU_ETS_list, 
                              Mean = Recon_PointF_zones_BU_ETS_list, real = Test.list_zones)
    LS_zones_OLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_OLS_ETS_list, 
                               Mean = Recon_PointF_zones_OLS_ETS_list, real = Test.list_zones)
    LS_zones_WLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_WLS_ETS_list, 
                               Mean = Recon_PointF_zones_WLS_ETS_list, real = Test.list_zones)
    LS_zones_MinT.Shr_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_MinT.Shr_ETS_list, 
                                    Mean = Recon_PointF_zones_MinT.Shr_ETS_list, real = Test.list_zones)
    
    #Calculating Energy score for regions predicive densities
    ES_regions_BU_ETS <- mapply(Energy_score, Data = X_regions_BU_ETS, Real = Test.list_regions)
    ES_regions_OLS_ETS <- mapply(Energy_score, X_regions_OLS_ETS, Real = Test.list_regions)
    ES_regions_WLS_ETS <- mapply(Energy_score, X_regions_WLS_ETS, Real = Test.list_regions)
    ES_regions_MinT.Shr_ETS <- mapply(Energy_score, X_regions_MinT.Shr_ETS, Real = Test.list_regions)
    ES_regions_Unrecon_ETS <- mapply(Energy_score, X_regions_unrecon_ETS, Real = Test.list_regions)
    
    #Calculating Variogram score for regions predicive densities
    VS_regions_BU_ETS <- mapply(Variogram_score, Data = X_regions_BU_ETS, Real = Test.list_regions)
    VS_regions_OLS_ETS <- mapply(Variogram_score, X_regions_OLS_ETS, Real = Test.list_regions)
    VS_regions_WLS_ETS <- mapply(Variogram_score, X_regions_WLS_ETS, Real = Test.list_regions)
    VS_regions_MinT.Shr_ETS <- mapply(Variogram_score, X_regions_MinT.Shr_ETS, Real = Test.list_regions)
    VS_regions_Unrecon_ETS <- mapply(Variogram_score, X_regions_unrecon_ETS, Real = Test.list_regions)
    
    #Splitting Reconciled means and variances
    Recon_PointF_regions_BU_ETS_list <- split(Recon_PointF_region_BU_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_regions_OLS_ETS_list <- split(Recon_PointF_region_OLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_regions_WLS_ETS_list <- split(Recon_PointF_region_WLS_ETS, 1:min(H, nrow(Test)))
    Recon_PointF_regions_MinT.Shr_ETS_list <- split(Recon_PointF_region_MinT.Shr_ETS, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_regions_BU_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                function(x) Recon_Var.Cov_region_BU_ETS)
    Recon_Var.Cov_regions_OLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                 function(x) Recon_Var.Cov_region_OLS_ETS)
    Recon_Var.Cov_regions_WLS_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                 function(x) Recon_Var.Cov_region_WLS_ETS)
    Recon_Var.Cov_regions_MinT.Shr_ETS_list <- lapply(1:min(H, nrow(Test)), 
                                                      function(x) Recon_Var.Cov_region_MinT.Shr_ETS)
    
    #Calculating Log score for regions predicive densities
    LS_regions_BU_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_BU_ETS_list, 
                                Mean = Recon_PointF_regions_BU_ETS_list, real = Test.list_regions)
    LS_regions_OLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_OLS_ETS_list, 
                                 Mean = Recon_PointF_regions_OLS_ETS_list, real = Test.list_regions)
    LS_regions_WLS_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_WLS_ETS_list, 
                                 Mean = Recon_PointF_regions_WLS_ETS_list, real = Test.list_regions)
    LS_regions_MinT.Shr_ETS <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_MinT.Shr_ETS_list, 
                                      Mean = Recon_PointF_regions_MinT.Shr_ETS_list, real = Test.list_regions)
    
    

    #Adding to ES and VS full to data frame
    DF_MultiV_Total %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_Unrecon_ETS, 
          "Variogram score" = VS_full_Unrecon_ETS) -> DF_Base
    DF_Base[names(DF_MultiV_Total)] -> DF_Base
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_full_BU_ETS, 
          "Variogram score" = VS_full_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_Total)] -> DF_BU
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_OLS_ETS, 
          "Variogram score" = VS_full_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_Total)] -> DF_OLS
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_WLS_ETS, 
          "Variogram score" = VS_full_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_Total)] -> DF_WLS
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_MinT.Shr_ETS, 
          "Variogram score" = VS_full_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Total)] -> DF_MinT.Shr
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_MinT.Shr)
    
    
    #Adding to ES, VS and LS of states to data frame
    
    DF_MultiV_States %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_Unrecon_ETS, 
          "Variogram score" = VS_states_Unrecon_ETS, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_States)] -> DF_Base
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_states_BU_ETS, 
          "Variogram score" = VS_states_BU_ETS, 
          "Log score" = LS_states_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_States)] -> DF_BU
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_OLS_ETS, 
          "Variogram score" = VS_states_OLS_ETS, 
          "Log score" = LS_states_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_States)] -> DF_OLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_WLS_ETS, 
          "Variogram score" = VS_states_WLS_ETS, 
          "Log score" = LS_states_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_States)] -> DF_WLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_MinT.Shr_ETS, 
          "Variogram score" = VS_states_MinT.Shr_ETS, 
          "Log score" = LS_states_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
    
    
    #Adding to ES, VS and LS of zones to data frame
    
    DF_MultiV_Zones %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_Unrecon_ETS, 
          "Variogram score" = VS_zones_Unrecon_ETS, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_Zones)] -> DF_Base
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_zones_BU_ETS, 
          "Variogram score" = VS_zones_BU_ETS, 
          "Log score" = LS_zones_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_Zones)] -> DF_BU
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_OLS_ETS, 
          "Variogram score" = VS_zones_OLS_ETS, 
          "Log score" = LS_zones_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_WLS_ETS, 
          "Variogram score" = VS_zones_WLS_ETS, 
          "Log score" = LS_zones_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_MinT.Shr_ETS, 
          "Variogram score" = VS_zones_MinT.Shr_ETS, 
          "Log score" = LS_zones_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
    
    
    
    #Adding to ES, VS and LS of regions to data frame
    
    DF_MultiV_Regions %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_Unrecon_ETS, 
          "Variogram score" = VS_regions_Unrecon_ETS, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_Regions)] -> DF_Base
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_regions_BU_ETS, 
          "Variogram score" = VS_regions_BU_ETS, 
          "Log score" = LS_regions_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_Regions)] -> DF_BU
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_OLS_ETS, 
          "Variogram score" = VS_regions_OLS_ETS, 
          "Log score" = LS_regions_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_WLS_ETS, 
          "Variogram score" = VS_regions_WLS_ETS, 
          "Log score" = LS_zones_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_MinT.Shr_ETS, 
          "Variogram score" = VS_regions_MinT.Shr_ETS, 
          "Log score" = LS_zones_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Regions)] -> DF_MinT.Shr
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_MinT.Shr)
    
    
    #Addinng CRPS to the DF
    
    DF_UniV %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  
          "R-method" = "Base", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_Unrecon_ETS))) -> DF_Base
    DF_Base[names(DF_UniV)] -> DF_Base
    DF_UniV <- rbind(DF_UniV, DF_Base)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), 
          "R-method" = "Bottom up", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_BU_ETS))) -> DF_BU
    DF_BU[names(DF_UniV)] -> DF_BU
    DF_UniV <- rbind(DF_UniV, DF_BU)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_OLS_ETS))) -> DF_OLS
    DF_OLS[names(DF_UniV)] -> DF_OLS
    DF_UniV <- rbind(DF_UniV, DF_OLS)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_WLS_ETS))) -> DF_WLS
    DF_WLS[names(DF_UniV)] -> DF_WLS
    DF_UniV <- rbind(DF_UniV, DF_WLS)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n),
          "CRPS" = c(t(CRPS_MinT.Shr_ETS))) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
    DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
    
    
    
    
    ##Calculating different G matrices required for reconciliation of ARIMA base forecasts##
    
    #Bottom up 
    
    Null.ma <- matrix(0,m,(n-m))
    BU_G <- cbind(Null.ma, diag(1,m,m))
    
    #OLS G
    OLS_G <- solve(t(S) %*% S) %*% t(S)
    
    #MinT shrink G
    targ <- lowerD(ForeError_all_ARIMA)
    shrink <- shrink.estim(ForeError_all_ARIMA,targ)
    Shr.cov_full_ARIMA <- shrink[[1]]
    Inv_Shr.cov_ARIMA <- solve(Shr.cov_full_ARIMA)
    
    MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
    
    #WLS G
    Cov_WLS_ARIMA <- diag(diag(Shr.cov_full_ARIMA), n, n)
    Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
    
    WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
    
    
    
    ###Mean forecast reconciliation (from ARIMA base)###
    
    #Reconciled point forecasts for the full hierarchy 
    Recon_PointF_full_BU_ARIMA <- t(S %*% BU_G %*% t(Base_ARIMA))
    Recon_PointF_full_OLS_ARIMA <- t(S %*% OLS_G %*% t(Base_ARIMA))
    Recon_PointF_full_WLS_ARIMA <- t(S %*% WLS_G_ARIMA %*% t(Base_ARIMA))
    Recon_PointF_full_MinT.Shr_ARIMA <- t(S %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
    
    #Reconciled point forecasts for the states of the hierarchy 
    Recon_PointF_states_BU_ARIMA <- t(S_state %*% BU_G %*% t(Base_ARIMA))
    Recon_PointF_states_OLS_ARIMA <- t(S_state %*% OLS_G %*% t(Base_ARIMA))
    Recon_PointF_states_WLS_ARIMA <- t(S_state %*% WLS_G_ARIMA %*% t(Base_ARIMA))
    Recon_PointF_states_MinT.Shr_ARIMA <- t(S_state %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
    
    #Reconciled point forecasts for the zones of the hierarchy 
    Recon_PointF_zones_BU_ARIMA <- t(S_zone %*% BU_G %*% t(Base_ARIMA))
    Recon_PointF_zones_OLS_ARIMA <- t(S_zone %*% OLS_G %*% t(Base_ARIMA))
    Recon_PointF_zones_WLS_ARIMA <- t(S_zone %*% WLS_G_ARIMA %*% t(Base_ARIMA))
    Recon_PointF_zones_MinT.Shr_ARIMA <- t(S_zone %*% MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
    
    Recon_PointF_region_BU_ARIMA <- t(BU_G %*% t(Base_ARIMA))
    Recon_PointF_region_OLS_ARIMA <- t(OLS_G %*% t(Base_ARIMA))
    Recon_PointF_region_WLS_ARIMA <- t(WLS_G_ARIMA %*% t(Base_ARIMA))
    Recon_PointF_region_MinT.Shr_ARIMA <- t(MinT.Shr_G_ARIMA %*% t(Base_ARIMA))
    
    
    ###Variance forecast reconciliation (from ARIMA base)###
    
    
    #Reconciled variance forecasts for the full hierarchy (Followed from ARIMA)#
    Recon_Var.Cov_full_BU_ARIMA <- S %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S %*% BU_G)
    Recon_Var.Cov_full_OLS_ARIMA <- S %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S %*% OLS_G)
    Recon_Var.Cov_full_WLS_ARIMA <- S %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% WLS_G_ARIMA)
    Recon_Var.Cov_full_MinT.Shr_ARIMA <- S %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S %*% MinT.Shr_G_ARIMA)
    
    #Reconciled variance forecasts for the states hierarchy (Followed from ARIMA)#
    Recon_Var.Cov_states_BU_ARIMA <- S_state %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S_state %*% BU_G)
    Recon_Var.Cov_states_OLS_ARIMA <- S_state %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S_state %*% OLS_G)
    Recon_Var.Cov_states_WLS_ARIMA <- S_state %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_state %*% WLS_G_ARIMA)
    Recon_Var.Cov_states_MinT.Shr_ARIMA <- S_state %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_state %*% MinT.Shr_G_ARIMA)
    
    #Reconciled variance forecasts for the zones hierarchy (Followed from ARIMA)#
    Recon_Var.Cov_zones_BU_ARIMA <- S_zone %*% BU_G %*% Shr.cov_full_ARIMA %*% t(S_zone %*% BU_G)
    Recon_Var.Cov_zones_OLS_ARIMA <- S_zone %*% OLS_G %*% Shr.cov_full_ARIMA %*% t(S_zone %*% OLS_G)
    Recon_Var.Cov_zones_WLS_ARIMA <- S_zone %*% WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_zone %*% WLS_G_ARIMA)
    Recon_Var.Cov_zones_MinT.Shr_ARIMA <- S_zone %*% MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(S_zone %*% MinT.Shr_G_ARIMA)
    
    #Reconciled bottom level variance forecasts (Followed from ARIMA)#
    Recon_Var.Cov_region_BU_ARIMA <- BU_G %*% Shr.cov_full_ARIMA %*% t(BU_G)
    Recon_Var.Cov_region_OLS_ARIMA <- OLS_G %*% Shr.cov_full_ARIMA %*% t(OLS_G)
    Recon_Var.Cov_region_WLS_ARIMA <- WLS_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(WLS_G_ARIMA)
    Recon_Var.Cov_region_MinT.Shr_ARIMA <- MinT.Shr_G_ARIMA %*% Shr.cov_full_ARIMA %*% t(MinT.Shr_G_ARIMA)
    
    #List to store random samples from reconciled Gauss distribution of the full hierarchy
    X_full_BU_ARIMA <- list(min(H, nrow(Test)))
    X_full_OLS_ARIMA <- list(min(H, nrow(Test)))
    X_full_WLS_ARIMA <- list(min(H, nrow(Test)))
    X_full_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
    X_full_unrecon_ARIMA <- list(min(H, nrow(Test)))
    
    
    #List to store random samples from reconciled Gauss distribution of the state levels
    X_states_BU_ARIMA <- list(min(H, nrow(Test)))
    X_states_OLS_ARIMA <- list(min(H, nrow(Test)))
    X_states_WLS_ARIMA <- list(min(H, nrow(Test)))
    X_states_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
    X_states_unrecon_ARIMA <- list(min(H, nrow(Test)))
    
    #List to store random samples from reconciled Gauss distribution of the Zone levels
    X_zones_BU_ARIMA <- list(min(H, nrow(Test)))
    X_zones_OLS_ARIMA <- list(min(H, nrow(Test)))
    X_zones_WLS_ARIMA <- list(min(H, nrow(Test)))
    X_zones_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
    X_zones_unrecon_ARIMA <- list(min(H, nrow(Test)))
    
    #List to store random samples from reconciled Gauss distribution of the Region levels
    X_regions_BU_ARIMA <- list(min(H, nrow(Test)))
    X_regions_OLS_ARIMA <- list(min(H, nrow(Test)))
    X_regions_WLS_ARIMA <- list(min(H, nrow(Test)))
    X_regions_MinT.Shr_ARIMA <- list(min(H, nrow(Test)))
    X_regions_unrecon_ARIMA <- list(min(H, nrow(Test)))
    
    #To store univariate scores
    CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    
    
    for (h in 1: min(H, nrow(Test))) {
      
      #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
      #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
      # function to generate random samples)
      
      X_full_BU_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_BU_ARIMA[h,], 
                                               Sigma = Recon_Var.Cov_full_BU_ARIMA, k = B, n = n)
      
      X_full_OLS_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_OLS_ARIMA[h,], 
                                                Sigma = Recon_Var.Cov_full_OLS_ARIMA, k = B, n = n)
      
      X_full_WLS_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_WLS_ARIMA[h,], 
                                                Sigma = Recon_Var.Cov_full_WLS_ARIMA, k = B, n = n)
      
      X_full_MinT.Shr_ARIMA[[h]] <- rnorm_degenerate(mu = Recon_PointF_full_MinT.Shr_ARIMA[h,], 
                                                     Sigma = Recon_Var.Cov_full_MinT.Shr_ARIMA, k = B, n = n)
      
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
      
      X_regions_unrecon_ARIMA[[h]] <- mvrnorm(n = B, mu = Base_ARIMA[h, (n-m+1):n], 
                                              Sigma = Shr.cov_full_ARIMA[(n-m+1):n, (n-m+1):n])
      
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
                                     "Replication" = rep(j, n*min(H, nrow(Test))))
      
    }
    
    
    Test.list_full <- split(Test[1:min(H, nrow(Test)),], 1:min(H, nrow(Test)))
    Test.list_states <- split(Test[1:min(H, nrow(Test)),(l1+1):(l1+l2)], 1:min(H, nrow(Test)))
    Test.list_zones <- split(Test[1:min(H, nrow(Test)),(l1+l2+1):(l1+l2+l3)], 1:min(H, nrow(Test)))
    Test.list_regions <- split(Test[1:min(H, nrow(Test)),(n-m+1):n], 1:min(H, nrow(Test)))
    
    #Calculating Energy score for full predicive densities
    ES_full_BU_ARIMA <- mapply(Energy_score, Data = X_full_BU_ARIMA, Real = Test.list_full)
    ES_full_OLS_ARIMA <- mapply(Energy_score, X_full_OLS_ARIMA, Real = Test.list_full)
    ES_full_WLS_ARIMA <- mapply(Energy_score, X_full_WLS_ARIMA, Real = Test.list_full)
    ES_full_MinT.Shr_ARIMA <- mapply(Energy_score, X_full_MinT.Shr_ARIMA, Real = Test.list_full)
    ES_full_Unrecon_ARIMA <- mapply(Energy_score, X_full_unrecon_ARIMA, Real = Test.list_full)
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ARIMA <- mapply(Variogram_score, Data = X_full_BU_ARIMA, Real = Test.list_full)
    VS_full_OLS_ARIMA <- mapply(Variogram_score, X_full_OLS_ARIMA, Real = Test.list_full)
    VS_full_WLS_ARIMA <- mapply(Variogram_score, X_full_WLS_ARIMA, Real = Test.list_full)
    VS_full_MinT.Shr_ARIMA <- mapply(Variogram_score, X_full_MinT.Shr_ARIMA, Real = Test.list_full)
    VS_full_Unrecon_ARIMA <- mapply(Variogram_score, X_full_unrecon_ARIMA, Real = Test.list_full)
    
    #Calculating Energy score for states predicive densities
    ES_states_BU_ARIMA <- mapply(Energy_score, Data = X_states_BU_ARIMA, Real = Test.list_states)
    ES_states_OLS_ARIMA <- mapply(Energy_score, X_states_OLS_ARIMA, Real = Test.list_states)
    ES_states_WLS_ARIMA <- mapply(Energy_score, X_states_WLS_ARIMA, Real = Test.list_states)
    ES_states_MinT.Shr_ARIMA <- mapply(Energy_score, X_states_MinT.Shr_ARIMA, Real = Test.list_states)
    ES_states_Unrecon_ARIMA <- mapply(Energy_score, X_states_unrecon_ARIMA, Real = Test.list_states)
    
    #Calculating Variogram score for states predicive densities
    VS_states_BU_ARIMA <- mapply(Variogram_score, Data = X_states_BU_ARIMA, Real = Test.list_states)
    VS_states_OLS_ARIMA <- mapply(Variogram_score, X_states_OLS_ARIMA, Real = Test.list_states)
    VS_states_WLS_ARIMA <- mapply(Variogram_score, X_states_WLS_ARIMA, Real = Test.list_states)
    VS_states_MinT.Shr_ARIMA <- mapply(Variogram_score, X_states_MinT.Shr_ARIMA, Real = Test.list_states)
    VS_states_Unrecon_ARIMA <- mapply(Variogram_score, X_states_unrecon_ARIMA, Real = Test.list_states)
    
    #Splitting Reconciled means and variances
    Recon_PointF_states_BU_ARIMA_list <- split(Recon_PointF_states_BU_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_states_OLS_ARIMA_list <- split(Recon_PointF_states_OLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_states_WLS_ARIMA_list <- split(Recon_PointF_states_WLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_states_MinT.Shr_ARIMA_list <- split(Recon_PointF_states_MinT.Shr_ARIMA, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_states_BU_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                 function(x) Recon_Var.Cov_states_BU_ARIMA)
    Recon_Var.Cov_states_OLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_states_OLS_ARIMA)
    Recon_Var.Cov_states_WLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_states_WLS_ARIMA)
    Recon_Var.Cov_states_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                       function(x) Recon_Var.Cov_states_MinT.Shr_ARIMA)
    
    #Calculating Log score for states predicive densities
    LS_states_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_BU_ARIMA_list, 
                                 Mean = Recon_PointF_states_BU_ARIMA_list, real = Test.list_states)
    LS_states_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_OLS_ARIMA_list, 
                                  Mean = Recon_PointF_states_OLS_ARIMA_list, real = Test.list_states)
    LS_states_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_WLS_ARIMA_list, 
                                  Mean = Recon_PointF_states_WLS_ARIMA_list, real = Test.list_states)
    LS_states_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_states_MinT.Shr_ARIMA_list, 
                                       Mean = Recon_PointF_states_MinT.Shr_ARIMA_list, real = Test.list_states)
    
    #Calculating Energy score for zones predicive densities
    ES_zones_BU_ARIMA <- mapply(Energy_score, Data = X_zones_BU_ARIMA, Real = Test.list_zones)
    ES_zones_OLS_ARIMA <- mapply(Energy_score, X_zones_OLS_ARIMA, Real = Test.list_zones)
    ES_zones_WLS_ARIMA <- mapply(Energy_score, X_zones_WLS_ARIMA, Real = Test.list_zones)
    ES_zones_MinT.Shr_ARIMA <- mapply(Energy_score, X_zones_MinT.Shr_ARIMA, Real = Test.list_zones)
    ES_zones_Unrecon_ARIMA <- mapply(Energy_score, X_zones_unrecon_ARIMA, Real = Test.list_zones)
    
    #Calculating Variogram score for zones predicive densities
    VS_zones_BU_ARIMA <- mapply(Variogram_score, Data = X_zones_BU_ARIMA, Real = Test.list_zones)
    VS_zones_OLS_ARIMA <- mapply(Variogram_score, X_zones_OLS_ARIMA, Real = Test.list_zones)
    VS_zones_WLS_ARIMA <- mapply(Variogram_score, X_zones_WLS_ARIMA, Real = Test.list_zones)
    VS_zones_MinT.Shr_ARIMA <- mapply(Variogram_score, X_zones_MinT.Shr_ARIMA, Real = Test.list_zones)
    VS_zones_Unrecon_ARIMA <- mapply(Variogram_score, X_zones_unrecon_ARIMA, Real = Test.list_zones)
    
    #Splitting Reconciled means and variances
    Recon_PointF_zones_BU_ARIMA_list <- split(Recon_PointF_zones_BU_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_zones_OLS_ARIMA_list <- split(Recon_PointF_zones_OLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_zones_WLS_ARIMA_list <- split(Recon_PointF_zones_WLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_zones_MinT.Shr_ARIMA_list <- split(Recon_PointF_zones_MinT.Shr_ARIMA, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_zones_BU_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                function(x) Recon_Var.Cov_zones_BU_ARIMA)
    Recon_Var.Cov_zones_OLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                 function(x) Recon_Var.Cov_zones_OLS_ARIMA)
    Recon_Var.Cov_zones_WLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                 function(x) Recon_Var.Cov_zones_WLS_ARIMA)
    Recon_Var.Cov_zones_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                      function(x) Recon_Var.Cov_zones_MinT.Shr_ARIMA)
    
    #Calculating Log score for zones predicive densities
    LS_zones_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_BU_ARIMA_list, 
                                Mean = Recon_PointF_zones_BU_ARIMA_list, real = Test.list_zones)
    LS_zones_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_OLS_ARIMA_list, 
                                 Mean = Recon_PointF_zones_OLS_ARIMA_list, real = Test.list_zones)
    LS_zones_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_WLS_ARIMA_list, 
                                 Mean = Recon_PointF_zones_WLS_ARIMA_list, real = Test.list_zones)
    LS_zones_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_zones_MinT.Shr_ARIMA_list, 
                                      Mean = Recon_PointF_zones_MinT.Shr_ARIMA_list, real = Test.list_zones)
    
    #Calculating Energy score for regions predicive densities
    ES_regions_BU_ARIMA <- mapply(Energy_score, Data = X_regions_BU_ARIMA, Real = Test.list_regions)
    ES_regions_OLS_ARIMA <- mapply(Energy_score, X_regions_OLS_ARIMA, Real = Test.list_regions)
    ES_regions_WLS_ARIMA <- mapply(Energy_score, X_regions_WLS_ARIMA, Real = Test.list_regions)
    ES_regions_MinT.Shr_ARIMA <- mapply(Energy_score, X_regions_MinT.Shr_ARIMA, Real = Test.list_regions)
    ES_regions_Unrecon_ARIMA <- mapply(Energy_score, X_regions_unrecon_ARIMA, Real = Test.list_regions)
    
    #Calculating Variogram score for regions predicive densities
    VS_regions_BU_ARIMA <- mapply(Variogram_score, Data = X_regions_BU_ARIMA, Real = Test.list_regions)
    VS_regions_OLS_ARIMA <- mapply(Variogram_score, X_regions_OLS_ARIMA, Real = Test.list_regions)
    VS_regions_WLS_ARIMA <- mapply(Variogram_score, X_regions_WLS_ARIMA, Real = Test.list_regions)
    VS_regions_MinT.Shr_ARIMA <- mapply(Variogram_score, X_regions_MinT.Shr_ARIMA, Real = Test.list_regions)
    VS_regions_Unrecon_ARIMA <- mapply(Variogram_score, X_regions_unrecon_ARIMA, Real = Test.list_regions)
    
    #Splitting Reconciled means and variances
    Recon_PointF_regions_BU_ARIMA_list <- split(Recon_PointF_region_BU_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_regions_OLS_ARIMA_list <- split(Recon_PointF_region_OLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_regions_WLS_ARIMA_list <- split(Recon_PointF_region_WLS_ARIMA, 1:min(H, nrow(Test)))
    Recon_PointF_regions_MinT.Shr_ARIMA_list <- split(Recon_PointF_region_MinT.Shr_ARIMA, 1:min(H, nrow(Test)))
    
    Recon_Var.Cov_regions_BU_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                  function(x) Recon_Var.Cov_region_BU_ARIMA)
    Recon_Var.Cov_regions_OLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                   function(x) Recon_Var.Cov_region_OLS_ARIMA)
    Recon_Var.Cov_regions_WLS_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                   function(x) Recon_Var.Cov_region_WLS_ARIMA)
    Recon_Var.Cov_regions_MinT.Shr_ARIMA_list <- lapply(1:min(H, nrow(Test)), 
                                                        function(x) Recon_Var.Cov_region_MinT.Shr_ARIMA)
    
    #Calculating Log score for regions predicive densities
    LS_regions_BU_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_BU_ARIMA_list, 
                                  Mean = Recon_PointF_regions_BU_ARIMA_list, real = Test.list_regions)
    LS_regions_OLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_OLS_ARIMA_list, 
                                   Mean = Recon_PointF_regions_OLS_ARIMA_list, real = Test.list_regions)
    LS_regions_WLS_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_WLS_ARIMA_list, 
                                   Mean = Recon_PointF_regions_WLS_ARIMA_list, real = Test.list_regions)
    LS_regions_MinT.Shr_ARIMA <- mapply(Log_score, Sigma = Recon_Var.Cov_regions_MinT.Shr_ARIMA_list, 
                                        Mean = Recon_PointF_regions_MinT.Shr_ARIMA_list, real = Test.list_regions)
    
    
    
    #Adding to ES and VS full to data frame
    DF_MultiV_Total %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_Unrecon_ARIMA, 
          "Variogram score" = VS_full_Unrecon_ARIMA) -> DF_Base
    DF_Base[names(DF_MultiV_Total)] -> DF_Base
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_full_BU_ARIMA, 
          "Variogram score" = VS_full_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_Total)] -> DF_BU
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_OLS_ARIMA, 
          "Variogram score" = VS_full_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_Total)] -> DF_OLS
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_WLS_ARIMA, 
          "Variogram score" = VS_full_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_Total)] -> DF_WLS
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_full_MinT.Shr_ARIMA, 
          "Variogram score" = VS_full_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Total)] -> DF_MinT.Shr
    DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_MinT.Shr)
    
    
    #Adding to ES, VS and LS of states to data frame
    
    DF_MultiV_States %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_Unrecon_ARIMA, 
          "Variogram score" = VS_states_Unrecon_ARIMA, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_States)] -> DF_Base
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_states_BU_ARIMA, 
          "Variogram score" = VS_states_BU_ARIMA, 
          "Log score" = LS_states_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_States)] -> DF_BU
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_OLS_ARIMA, 
          "Variogram score" = VS_states_OLS_ARIMA, 
          "Log score" = LS_states_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_States)] -> DF_OLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_WLS_ARIMA, 
          "Variogram score" = VS_states_WLS_ARIMA, 
          "Log score" = LS_states_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_States)] -> DF_WLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_MinT.Shr_ARIMA, 
          "Variogram score" = VS_states_MinT.Shr_ARIMA, 
          "Log score" = LS_states_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
    
    
    #Adding to ES, VS and LS of zones to data frame
    
    DF_MultiV_Zones %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_Unrecon_ARIMA, 
          "Variogram score" = VS_zones_Unrecon_ARIMA, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_Zones)] -> DF_Base
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_zones_BU_ARIMA, 
          "Variogram score" = VS_zones_BU_ARIMA, 
          "Log score" = LS_zones_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_Zones)] -> DF_BU
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_OLS_ARIMA, 
          "Variogram score" = VS_zones_OLS_ARIMA, 
          "Log score" = LS_zones_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_WLS_ARIMA, 
          "Variogram score" = VS_zones_WLS_ARIMA, 
          "Log score" = LS_zones_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_MinT.Shr_ARIMA, 
          "Variogram score" = VS_zones_MinT.Shr_ARIMA, 
          "Log score" = LS_zones_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
    
    
    
    #Adding to ES, VS and LS of regions to data frame
    
    DF_MultiV_Regions %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_Unrecon_ARIMA, 
          "Variogram score" = VS_regions_Unrecon_ARIMA, 
          "Log score" = NA) -> DF_Base
    DF_Base[names(DF_MultiV_Regions)] -> DF_Base
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_regions_BU_ARIMA, 
          "Variogram score" = VS_regions_BU_ARIMA, 
          "Log score" = LS_regions_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_Regions)] -> DF_BU
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_OLS_ARIMA, 
          "Variogram score" = VS_regions_OLS_ARIMA, 
          "Log score" = LS_regions_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_WLS_ARIMA, 
          "Variogram score" = VS_regions_WLS_ARIMA, 
          "Log score" = LS_zones_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_MinT.Shr_ARIMA, 
          "Variogram score" = VS_regions_MinT.Shr_ARIMA, 
          "Log score" = LS_zones_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Regions)] -> DF_MinT.Shr
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_MinT.Shr)
    
    
    #Addinng CRPS to the DF
    
    DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  
          "R-method" = "Base", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_Unrecon_ARIMA))) -> DF_Base
    DF_Base[names(DF_UniV)] -> DF_Base
    DF_UniV <- rbind(DF_UniV, DF_Base)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), 
          "R-method" = "Bottom up", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_BU_ARIMA))) -> DF_BU
    DF_BU[names(DF_UniV)] -> DF_BU
    DF_UniV <- rbind(DF_UniV, DF_BU)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_OLS_ARIMA))) -> DF_OLS
    DF_OLS[names(DF_UniV)] -> DF_OLS
    DF_UniV <- rbind(DF_UniV, DF_OLS)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_WLS_ARIMA))) -> DF_WLS
    DF_WLS[names(DF_UniV)] -> DF_WLS
    DF_UniV <- rbind(DF_UniV, DF_WLS)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n),
          "CRPS" = c(t(CRPS_MinT.Shr_ARIMA))) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
    DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)

}

End <- Sys.time()



DF_MultiV_Total[complete.cases(DF_MultiV_Total[ , "R-method"]),] -> DF_MultiV_Total
DF_MultiV_States[complete.cases(DF_MultiV_States[ , "R-method"]),] -> DF_MultiV_States
DF_MultiV_Zones[complete.cases(DF_MultiV_Zones[ , "R-method"]),] -> DF_MultiV_Zones
DF_MultiV_Regions[complete.cases(DF_MultiV_Regions[ , "R-method"]),] -> DF_MultiV_Regions

write.csv(DF_MultiV_Total, "DF_MultiV_Total_101-152.csv")
write.csv(DF_MultiV_States, "DF_MultiV_States_101-152.csv")
write.csv(DF_MultiV_Zones, "DF_MultiV_Zones_101-152.csv")
write.csv(DF_MultiV_Regions, "DF_MultiV_Regions_101-152.csv")
write.csv(DF_UniV, "DF_UniV_101-152.csv")

save.image("Forecasting_OvernightTrips_GaussMethod_101-152.RData")
