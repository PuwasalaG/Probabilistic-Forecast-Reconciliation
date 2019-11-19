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

#A function to obtain future paths
FP_func <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the
  #k^th future path for H forecast horizons for all n series. In the returning matrix, 
  #rows represents forecast horizons columns represents the series
  
  fit <- fit
  Residuals_all <- Resid
  Index <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(Residuals_all[Index_seq[k,],]))
  
  return(mapply(simulate, fit, future = TRUE, nsim = H, innov = Innov))
  
}


#Following function will return the k^th future path for H forecast horizons for all n 
#series. In the returning matrix, rows represents forecast horizons
#columns represents the series
# FP_func <- function(k, fit, Resid, Index, Index_seq, H, n) { 
#   
#   fit_eval <- fit
#   ResidModel_all <- Resid
#   Index_eval <- Index
#   Index_seq <- Index_seq
#   H <- H
#   n <- n
#   
#   Innov <- as.list(as.data.frame(ResidModel_all[Index_seq[k,],]))
#   
#   return(mapply(simulate, fit_eval, future = TRUE, nsim = H, innov = Innov))
#   
# }


#All the forecasts and related informations are stored in the DF dataframe

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
                           "Replication" = integer())

DF_MultiV_Zones <- tibble("F-method" = character(),
                          "R-method" = character(),
                          "Forecast Horizon" = integer(),
                          "Energy score" = numeric(),
                          "Variogram score" = numeric(),
                          "Replication" = integer())

DF_MultiV_Regions <- tibble("F-method" = character(),
                            "R-method" = character(),
                            "Forecast Horizon" = integer(),
                            "Energy score" = numeric(),
                            "Variogram score" = numeric(),
                            "Replication" = integer())


DF_UniV <- tibble("Series" = character(),
                  "F-method" = character(),
                  "R-method" = character(),
                  "Forecast Horizon" = integer(),
                  "Actual" = double(),
                  "CRPS" = numeric(),
                  "Replication" = integer())

Start <- Sys.time()

for (j in 71:80) { #1:89 (To get forecasts for all H=1 to H=12)
  
  
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
  
  
  Unreconciled_future_paths_h1 <- list()
  Unreconciled_future_paths_h2 <- list()
  Unreconciled_future_paths_h3 <- list()
  Unreconciled_future_paths_h4 <- list()
  Unreconciled_future_paths_h5 <- list()
  Unreconciled_future_paths_h6 <- list()
  Unreconciled_future_paths_h7 <- list()
  Unreconciled_future_paths_h8 <- list()
  Unreconciled_future_paths_h9 <- list()
  Unreconciled_future_paths_h10 <- list()
  Unreconciled_future_paths_h11 <- list()
  Unreconciled_future_paths_h12 <- list()
  
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
    
    
    #Model fitting, forecasting and obtaining residuals
    
    Residuals_all_training <- matrix(NA, nrow = nrow(Training), ncol = n)
    fit_training <- list()
    
    
    for(i in 1:n) {
      TS <- ts(Training[,i], frequency = 12)
      fit_training[[i]] <- auto.arima(TS)
      Residuals_all_training[,i] <- TS - fitted(fit_training[[i]])
    }
    
    
    Index <- base::sample(c(1:(nrow(Residuals_all_training)-(H-1))), size = B, 
                          replace = TRUE)
    Index_seq <- matrix(0, B, H)
    
    for (k in 1:B) {
      
      Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
      
    }
    
    
    
    Start_FP <- Sys.time()
    future_paths <-  furrr::future_map(c(1:B), FP_func, fit = fit_training,
                                       Resid = Residuals_all_training, Index = Index,
                                       Index_seq = Index_seq, H=H, n=n)
    End_FP <- Sys.time()
    
    Unreconciled_future_paths_h1[[q]] <- laply(future_paths, function(y) y[1,])
    Unreconciled_future_paths_h2[[q]] <- laply(future_paths, function(y) y[2,])
    Unreconciled_future_paths_h3[[q]] <- laply(future_paths, function(y) y[3,])
    Unreconciled_future_paths_h4[[q]] <- laply(future_paths, function(y) y[4,])
    Unreconciled_future_paths_h5[[q]] <- laply(future_paths, function(y) y[5,])
    Unreconciled_future_paths_h6[[q]] <- laply(future_paths, function(y) y[6,])
    Unreconciled_future_paths_h7[[q]] <- laply(future_paths, function(y) y[7,])
    Unreconciled_future_paths_h8[[q]] <- laply(future_paths, function(y) y[8,])
    Unreconciled_future_paths_h9[[q]] <- laply(future_paths, function(y) y[9,])
    Unreconciled_future_paths_h10[[q]] <- laply(future_paths, function(y) y[10,])
    Unreconciled_future_paths_h11[[q]] <- laply(future_paths, function(y) y[11,])
    Unreconciled_future_paths_h12[[q]] <- laply(future_paths, function(y) y[12,])
    
    
    
  }
  
  End_train <-  Sys.time()
  
  FP_training_list <- list(Unreconciled_future_paths_h1, Unreconciled_future_paths_h2, 
                           Unreconciled_future_paths_h3, Unreconciled_future_paths_h4, 
                           Unreconciled_future_paths_h5, Unreconciled_future_paths_h6, 
                           Unreconciled_future_paths_h7, Unreconciled_future_paths_h8, 
                           Unreconciled_future_paths_h9, Unreconciled_future_paths_h10,
                           Unreconciled_future_paths_h11, Unreconciled_future_paths_h12)
  
  Testing <- list(Testing_h1, Testing_h2, Testing_h3, Testing_h4, Testing_h5, Testing_h6,
                  Testing_h7, Testing_h8, Testing_h9, Testing_h10, Testing_h11, 
                  Testing_h12)
  
  
  ###Method 1: Estimating W_h (W_1 to W_12)
  
  
  #Calculating initial Inv.weight matrix for the optimization problem
  
  Int_par_W <- gdata::upperTriangle(diag(1,n,n), diag = TRUE, byrow = TRUE)
  
  Method1_Opt_G <- list() #Stores Optimal G from method 1 for H forecast horizons
  
  Start_opt <- Sys.time()
  for (h in 1:H) {
    
    Opt_Vec_W <- optim(Int_par_W, Energy_score_method1, gr = Grad_method1, method = "BFGS",
                       Future_paths = FP_training_list[[h]], Test = Testing[[h]], n1 = n, B1 = B, 
                       r1 = r, S1=S)$par
    Inv_Opt_W <- miscTools::symMatrix(Opt_Vec_W, nrow = n, byrow = FALSE)
    Method1_Opt_G[[h]] <- solve(t(S) %*% Inv_Opt_W %*% S) %*% t(S) %*% Inv_Opt_W
    
  }
  End_opt <- Sys.time()
  
  
  # Using optimal G and other G matrices to obtain reconciled future paths   
  
  
  Training_eval <- AllTS_a[(r+1):(L+r),]
  Testing_eval <- AllTS_a[(L+r+1):(L+r+H),]
  
  
  #List to store model fit for each series - use later to simualte future paths
  fit_ARIMA <- list(n)
  
  #Matrix to store model residuals
  ModelResid_all_ARIMA <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Training_eval), ncol = n)
  
  Start_fc <- Sys.time()
  for(i in 1:n) {
    
    TS <- ts(Training_eval[,i], frequency = 12)
    
    #Forecsting with ARIMA
    fit_ARIMA[[i]] <- auto.arima(TS) # stepwise=FALSE,approx=FALSE
    Forecast_ARIMA <- forecast(fit_ARIMA[[i]], h = min(H, nrow(Testing_eval[,i])))
    ModelResid_all_ARIMA[,i] <- residuals(fit_ARIMA[[i]])
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA[[i]]))
    
  }  
  End_fc <- Sys.time() 
  
  ## Getting incoherent sample paths ##
  
  #List of lenght H to store future paths. Each element of this list corresponds to a matrix
  #that holds bootstrap future paths at forecast horizon H.
  Unrecon_future_paths_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #To store unreconcile future paths of states
  Unrecon_future_paths_States_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #To store unreconcile future paths of states
  Unrecon_future_paths_Zones_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #To store unreconcile future paths of states
  Unrecon_future_paths_Regions_ARIMA <- list(min(H, nrow(Testing_eval)))
  
  #Index to get the block of bootsrap samples
  Index <- base::sample(c(1:(nrow(ModelResid_all_ARIMA)-(H-1))), size = B , 
                        replace = TRUE)
  Index_seq <- matrix(0, B, H)
  
  for (k in 1:B) {
    
    Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
    
  }
  
  #Simulating future paths
  Start_FP <- Sys.time()
  
  fp_ARIMA <-  lapply(c(1:B), FP_func, fit = fit_ARIMA, 
                      Resid = ModelResid_all_ARIMA, Index = Index, 
                      Index_seq = Index_seq, H=H, n=n)
  
  End_FP <- Sys.time()
  
  for (h in 1:min(H, nrow(Testing_eval))) {
    
    Unrecon_future_paths_ARIMA[[h]] <- plyr::laply(fp_ARIMA, function(y) y[h,])
    
    Unrecon_future_paths_States_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(l1+1):(l1+l2)]
    
    Unrecon_future_paths_Zones_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    
    Unrecon_future_paths_Regions_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(n-m+1):n]
    
    
  }
  
  
  
  
  
  
  ##Calculating different G matrices required for reconciliation of ARIMA base forecasts##
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  #MinT shrink G
  targ <- diag(diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  shrink <- shrink.estim(na.omit(ForeError_all_ARIMA),targ)
  Shr.cov_ARIMA <- shrink[[1]]
  Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
  
  MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
  
  #WLS G
  Inv_WLS_ARIMA <- diag(1/diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  
  
  WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
  
  
  
  ###Reconciliation of base forecasts from ARIMA###
  
  #List to store reconciled future paths from the full hierarchy
  Reconciled_future_paths_BU_ARIMA <- list()
  Reconciled_future_paths_OLS_ARIMA <- list()
  Reconciled_future_paths_WLS_ARIMA <- list()
  Reconciled_future_paths_MinT.Shr_ARIMA <- list()
  Reconciled_future_paths_Optimal_ARIMA <- list()
  
  #List to store reconciled future paths from states
  Reconciled_future_paths_States_BU_ARIMA <- list()
  Reconciled_future_paths_States_OLS_ARIMA <- list()
  Reconciled_future_paths_States_WLS_ARIMA <- list()
  Reconciled_future_paths_States_MinT.Shr_ARIMA <- list()
  Reconciled_future_paths_States_Optimal_ARIMA <- list()
  
  #List to store reconciled future paths from zones
  Reconciled_future_paths_Zones_BU_ARIMA <- list()
  Reconciled_future_paths_Zones_OLS_ARIMA <- list()
  Reconciled_future_paths_Zones_WLS_ARIMA <- list()
  Reconciled_future_paths_Zones_MinT.Shr_ARIMA <- list()
  Reconciled_future_paths_Zones_Optimal_ARIMA <- list()
  
  #List to store reconciled future paths from regions
  Reconciled_future_paths_Regions_BU_ARIMA <- list()
  Reconciled_future_paths_Regions_OLS_ARIMA <- list()
  Reconciled_future_paths_Regions_WLS_ARIMA <- list()
  Reconciled_future_paths_Regions_MinT.Shr_ARIMA <- list()
  Reconciled_future_paths_Regions_Optimal_ARIMA <- list()
  
  #To store univariate scores
  CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_Optimal_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Testing_eval)), ncol = n)
  
  Start.rest <- Sys.time()
  
  for (h in 1: min(H, nrow(Testing_eval))) {
    
    Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_MinT.Shr_ARIMA[[h]] <- t(S %*% MinT.Shr_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_Optimal_ARIMA[[h]] <- t(S %*% Method1_Opt_G[[h]] %*% t(Unrecon_future_paths_ARIMA[[h]]))
    
    #Reconciled densities for states
    Reconciled_future_paths_States_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(l1+1):(l1+l2)]
    Reconciled_future_paths_States_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(l1+1):(l1+l2)]
    Reconciled_future_paths_States_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(l1+1):(l1+l2)]
    Reconciled_future_paths_States_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(l1+1):(l1+l2)]
    Reconciled_future_paths_States_Optimal_ARIMA[[h]] <- Reconciled_future_paths_Optimal_ARIMA[[h]][,(l1+1):(l1+l2)]
    
    #Reconciled densities for zones
    Reconciled_future_paths_Zones_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    Reconciled_future_paths_Zones_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    Reconciled_future_paths_Zones_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    Reconciled_future_paths_Zones_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    Reconciled_future_paths_Zones_Optimal_ARIMA[[h]] <- Reconciled_future_paths_Optimal_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
    
    #Reconciled densities for regions
    Reconciled_future_paths_Regions_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(n-m+1):n]
    Reconciled_future_paths_Regions_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(n-m+1):n]
    Reconciled_future_paths_Regions_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(n-m+1):n]
    Reconciled_future_paths_Regions_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(n-m+1):n]
    Reconciled_future_paths_Regions_Optimal_ARIMA[[h]] <- Reconciled_future_paths_Optimal_ARIMA[[h]][,(n-m+1):n]
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Unrecon_future_paths_ARIMA[[h]][,i],
                                             method = "edf")
      CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Reconciled_future_paths_BU_ARIMA[[h]][,i],
                                        method = "edf")
      CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Reconciled_future_paths_OLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Reconciled_future_paths_WLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,i],
                                              method = "edf")
      CRPS_Optimal_ARIMA[h,i] <- crps_sample(as.numeric(Testing_eval[h,i]), dat = Reconciled_future_paths_Optimal_ARIMA[[h]][,i],
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
  
  
  
  Test.list_full <- split(Testing_eval[1:min(H, nrow(Testing_eval)),], 1:min(H, nrow(Testing_eval)))
  Test.list_states <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(l1+1):(l1+l2)], 1:min(H, nrow(Testing_eval)))
  Test.list_zones <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(l1+l2+1):(l1+l2+l3)], 1:min(H, nrow(Testing_eval)))
  Test.list_regions <- split(Testing_eval[1:min(H, nrow(Testing_eval)),(n-m+1):n], 1:min(H, nrow(Testing_eval)))
  
  #Calculating Energy score for full predicive densities
  ES_full_BU_ARIMA <- mapply(Energy_score, Data = Reconciled_future_paths_BU_ARIMA, Real = Test.list_full)
  ES_full_OLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_OLS_ARIMA, Real = Test.list_full)
  ES_full_WLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_WLS_ARIMA, Real = Test.list_full)
  ES_full_MinT.Shr_ARIMA <- mapply(Energy_score, Reconciled_future_paths_MinT.Shr_ARIMA, Real = Test.list_full)
  ES_full_Optimal_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Optimal_ARIMA, Real = Test.list_full)
  ES_full_Unrecon_ARIMA <- mapply(Energy_score, Unrecon_future_paths_ARIMA, Real = Test.list_full)
  
  #Calculating Variogram score for full predicive densities
  VS_full_BU_ARIMA <- mapply(Variogram_score, Data = Reconciled_future_paths_BU_ARIMA, Real = Test.list_full)
  VS_full_OLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_OLS_ARIMA, Real = Test.list_full)
  VS_full_WLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_WLS_ARIMA, Real = Test.list_full)
  VS_full_MinT.Shr_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_MinT.Shr_ARIMA, Real = Test.list_full)
  VS_full_Optimal_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Optimal_ARIMA, Real = Test.list_full)
  VS_full_Unrecon_ARIMA <- mapply(Variogram_score, Unrecon_future_paths_ARIMA, Real = Test.list_full)
  
  #Calculating Energy score for predicive densities of states
  ES_states_BU_ARIMA <- mapply(Energy_score, Data = Reconciled_future_paths_States_BU_ARIMA, 
                               Real = Test.list_states)
  ES_states_OLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_States_OLS_ARIMA, 
                                Real = Test.list_states)
  ES_states_WLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_States_WLS_ARIMA, 
                                Real = Test.list_states)
  ES_states_MinT.Shr_ARIMA <- mapply(Energy_score, Reconciled_future_paths_States_MinT.Shr_ARIMA, 
                                     Real = Test.list_states)
  ES_states_Optimal_ARIMA <- mapply(Energy_score, Reconciled_future_paths_States_Optimal_ARIMA, 
                                    Real = Test.list_states)
  ES_states_Unrecon_ARIMA <- mapply(Energy_score, Unrecon_future_paths_States_ARIMA, 
                                    Real = Test.list_states)
  
  #Calculating Variogram score for predicive densities of states
  VS_states_BU_ARIMA <- mapply(Variogram_score, Data = Reconciled_future_paths_States_BU_ARIMA, 
                               Real = Test.list_states)
  VS_states_OLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_States_OLS_ARIMA, 
                                Real = Test.list_states)
  VS_states_WLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_States_WLS_ARIMA, 
                                Real = Test.list_states)
  VS_states_MinT.Shr_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_States_MinT.Shr_ARIMA, 
                                     Real = Test.list_states)
  VS_states_Optimal_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_States_Optimal_ARIMA, 
                                    Real = Test.list_states)
  VS_states_Unrecon_ARIMA <- mapply(Variogram_score, Unrecon_future_paths_States_ARIMA, 
                                    Real = Test.list_states)
  
  #Calculating Energy score for predicive densities of zones
  ES_zones_BU_ARIMA <- mapply(Energy_score, Data = Reconciled_future_paths_Zones_BU_ARIMA, 
                              Real = Test.list_zones)
  ES_zones_OLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Zones_OLS_ARIMA, 
                               Real = Test.list_zones)
  ES_zones_WLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Zones_WLS_ARIMA, 
                               Real = Test.list_zones)
  ES_zones_MinT.Shr_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Zones_MinT.Shr_ARIMA, 
                                    Real = Test.list_zones)
  ES_zones_Optimal_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Zones_Optimal_ARIMA, 
                                   Real = Test.list_zones)
  ES_zones_Unrecon_ARIMA <- mapply(Energy_score, Unrecon_future_paths_Zones_ARIMA, 
                                   Real = Test.list_zones)
  
  #Calculating Variogram score for predicive densities of zones
  VS_zones_BU_ARIMA <- mapply(Variogram_score, Data = Reconciled_future_paths_Zones_BU_ARIMA, 
                              Real = Test.list_zones)
  VS_zones_OLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Zones_OLS_ARIMA, 
                               Real = Test.list_zones)
  VS_zones_WLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Zones_WLS_ARIMA, 
                               Real = Test.list_zones)
  VS_zones_MinT.Shr_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Zones_MinT.Shr_ARIMA, 
                                    Real = Test.list_zones)
  VS_zones_Optimal_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Zones_Optimal_ARIMA, 
                                   Real = Test.list_zones)
  VS_zones_Unrecon_ARIMA <- mapply(Variogram_score, Unrecon_future_paths_Zones_ARIMA, 
                                   Real = Test.list_zones)
  
  
  #Calculating Energy score for predicive densities of regions
  ES_regions_BU_ARIMA <- mapply(Energy_score, Data = Reconciled_future_paths_Regions_BU_ARIMA, 
                                Real = Test.list_regions)
  ES_regions_OLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Regions_OLS_ARIMA, 
                                 Real = Test.list_regions)
  ES_regions_WLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Regions_WLS_ARIMA, 
                                 Real = Test.list_regions)
  ES_regions_MinT.Shr_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Regions_MinT.Shr_ARIMA, 
                                      Real = Test.list_regions)
  ES_regions_Optimal_ARIMA <- mapply(Energy_score, Reconciled_future_paths_Regions_Optimal_ARIMA, 
                                     Real = Test.list_regions)
  ES_regions_Unrecon_ARIMA <- mapply(Energy_score, Unrecon_future_paths_Regions_ARIMA, 
                                     Real = Test.list_regions)
  
  #Calculating Variogram score for predicive densities of zones
  VS_regions_BU_ARIMA <- mapply(Variogram_score, Data = Reconciled_future_paths_Regions_BU_ARIMA, 
                                Real = Test.list_regions)
  VS_regions_OLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Regions_OLS_ARIMA, 
                                 Real = Test.list_regions)
  VS_regions_WLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Regions_WLS_ARIMA, 
                                 Real = Test.list_regions)
  VS_regions_MinT.Shr_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Regions_MinT.Shr_ARIMA, 
                                      Real = Test.list_regions)
  VS_regions_Optimal_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_Regions_Optimal_ARIMA, 
                                     Real = Test.list_regions)
  VS_regions_Unrecon_ARIMA <- mapply(Variogram_score, Unrecon_future_paths_Regions_ARIMA, 
                                     Real = Test.list_regions)
  
  
  #Adding to ES_full to data frame
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
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_full_Optimal_ARIMA, 
        "Variogram score" = VS_full_Optimal_ARIMA) -> DF_Optim
  DF_Optim[names(DF_MultiV_Total)] -> DF_Optim
  DF_MultiV_Total <- rbind(DF_MultiV_Total, DF_Optim)
  
  #Adding to ES_states to data frame
  
  DF_MultiV_States %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_Unrecon_ARIMA, 
        "Variogram score" = VS_states_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV_States)] -> DF_Base
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_states_BU_ARIMA, 
        "Variogram score" = VS_states_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_States)] -> DF_BU
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_OLS_ARIMA, 
        "Variogram score" = VS_states_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_States)] -> DF_OLS
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_WLS_ARIMA, 
        "Variogram score" = VS_states_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_States)] -> DF_WLS
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_MinT.Shr_ARIMA, 
        "Variogram score" = VS_states_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_states_Optimal_ARIMA, 
        "Variogram score" = VS_states_Optimal_ARIMA) -> DF_Optim
  DF_Optim[names(DF_MultiV_States)] -> DF_Optim
  DF_MultiV_States <- rbind(DF_MultiV_States, DF_Optim)
  
  #Adding to ES_zones to data frame
  
  DF_MultiV_Zones %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_Unrecon_ARIMA, 
        "Variogram score" = VS_zones_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV_Zones)] -> DF_Base
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_zones_BU_ARIMA, 
        "Variogram score" = VS_zones_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Zones)] -> DF_BU
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_OLS_ARIMA, 
        "Variogram score" = VS_zones_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_WLS_ARIMA, 
        "Variogram score" = VS_zones_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_MinT.Shr_ARIMA, 
        "Variogram score" = VS_zones_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_zones_Optimal_ARIMA, 
        "Variogram score" = VS_zones_Optimal_ARIMA) -> DF_Optim
  DF_Optim[names(DF_MultiV_Zones)] -> DF_Optim
  DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Optim)
  
  
  #Adding to ES_regions to data frame
  
  DF_MultiV_Regions %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_Unrecon_ARIMA, 
        "Variogram score" = VS_regions_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV_Regions)] -> DF_Base
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))),
        "Energy score" = ES_regions_BU_ARIMA, 
        "Variogram score" = VS_regions_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV_Regions)] -> DF_BU
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_OLS_ARIMA, 
        "Variogram score" = VS_regions_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_WLS_ARIMA, 
        "Variogram score" = VS_regions_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_MinT.Shr_ARIMA, 
        "Variogram score" = VS_regions_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV_Regions)] -> DF_MinT.Shr
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_MinT.Shr)
  
  cbind(Fltr, "R-method" = "Optimal", "Forecast Horizon" = c(1: min(H, nrow(Testing_eval))), 
        "Energy score" = ES_regions_Optimal_ARIMA, 
        "Variogram score" = VS_regions_Optimal_ARIMA) -> DF_Optim
  DF_Optim[names(DF_MultiV_Regions)] -> DF_Optim
  DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Optim)
  
  #Addinng CRPS to the DF
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("F-method", "Replication") -> Fltr
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_Unrecon_ARIMA))) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), "R-method" = "Bottom up", 
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
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), "CRPS" = c(t(CRPS_MinT.Shr_ARIMA))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Testing_eval))), 
        "Actual" = c(t(as.matrix(Testing_eval[1:min(H, nrow(Testing_eval)),]))), 
        "R-method" = "Optimal", 
        "Forecast Horizon" = rep(1:min(H, nrow(Testing_eval)), each = n), 
        "CRPS" = c(t(CRPS_Optimal_ARIMA))) -> DF_Optim
  DF_Optim[names(DF_UniV)] -> DF_Optim
  DF_UniV <- rbind(DF_UniV, DF_Optim)
  
  End.rest <- Sys.time()
  
  
}

End <- Sys.time()



DF_MultiV_Total[complete.cases(DF_MultiV_Total[ , "R-method"]),] -> DF_MultiV_Total
DF_MultiV_States[complete.cases(DF_MultiV_States[ , "R-method"]),] -> DF_MultiV_States
DF_MultiV_Zones[complete.cases(DF_MultiV_Zones[ , "R-method"]),] -> DF_MultiV_Zones
DF_MultiV_Regions[complete.cases(DF_MultiV_Regions[ , "R-method"]),] -> DF_MultiV_Regions

write.csv(DF_MultiV_Total, "Results/DF_MultiV_Total_71-80.csv")
write.csv(DF_MultiV_States, "Results/DF_MultiV_States_71-80.csv")
write.csv(DF_MultiV_Zones, "Results/DF_MultiV_Zones_71-80.csv")
write.csv(DF_MultiV_Regions, "Results/DF_MultiV_Regions_71-80.csv")
write.csv(DF_UniV, "Results/DF_UniV_71-80.csv")

save.image("Results/Forecasting_OvernightTrips_NonParaMethod_71-80.RData")
