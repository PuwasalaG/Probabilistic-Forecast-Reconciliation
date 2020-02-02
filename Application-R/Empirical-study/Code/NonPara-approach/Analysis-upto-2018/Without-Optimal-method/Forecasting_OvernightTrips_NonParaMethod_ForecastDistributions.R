# This script generate reconciled forecast distributions and saved the values in 
# lists. These can be used to visualise the predictive distributions before and
# after the reconciliation

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

#Following function will return the k^th future path for H forecast horizons for all n 
#series. In the returning matrix, rows represents forecast horizons
#columns represents the series
FP_func <- function(k, fit, Resid, Index, Index_seq, H, n) { 
  
  fit_eval <- fit
  ResidModel_all <- Resid
  Index_eval <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(ResidModel_all[Index_seq[k,],]))
  
  return(mapply(simulate, fit_eval, future = TRUE, nsim = H, innov = Innov))
  
}


# Lists to save future paths
Unrecon_FP_ETS_h1 <- list()
Unrecon_FP_ETS_h2 <- list()
Unrecon_FP_ETS_h3 <- list()
Unrecon_FP_ETS_h4 <- list()
Unrecon_FP_ETS_h5 <- list()
Unrecon_FP_ETS_h6 <- list()

Unrecon_FP_ARIMA_h1 <- list()
Unrecon_FP_ARIMA_h2 <- list()
Unrecon_FP_ARIMA_h3 <- list()
Unrecon_FP_ARIMA_h4 <- list()
Unrecon_FP_ARIMA_h5 <- list()
Unrecon_FP_ARIMA_h6 <- list()

Recon_BU_FP_ETS_h1 <- list()
Recon_BU_FP_ETS_h2 <- list()
Recon_BU_FP_ETS_h3 <- list()
Recon_BU_FP_ETS_h4 <- list()
Recon_BU_FP_ETS_h5 <- list()
Recon_BU_FP_ETS_h6 <- list()

Recon_BU_FP_ARIMA_h1 <- list()
Recon_BU_FP_ARIMA_h2 <- list()
Recon_BU_FP_ARIMA_h3 <- list()
Recon_BU_FP_ARIMA_h4 <- list()
Recon_BU_FP_ARIMA_h5 <- list()
Recon_BU_FP_ARIMA_h6 <- list()

Recon_OLS_FP_ETS_h1 <- list()
Recon_OLS_FP_ETS_h2 <- list()
Recon_OLS_FP_ETS_h3 <- list()
Recon_OLS_FP_ETS_h4 <- list()
Recon_OLS_FP_ETS_h5 <- list()
Recon_OLS_FP_ETS_h6 <- list()

Recon_OLS_FP_ARIMA_h1 <- list()
Recon_OLS_FP_ARIMA_h2 <- list()
Recon_OLS_FP_ARIMA_h3 <- list()
Recon_OLS_FP_ARIMA_h4 <- list()
Recon_OLS_FP_ARIMA_h5 <- list()
Recon_OLS_FP_ARIMA_h6 <- list()

Recon_WLS_FP_ETS_h1 <- list()
Recon_WLS_FP_ETS_h2 <- list()
Recon_WLS_FP_ETS_h3 <- list()
Recon_WLS_FP_ETS_h4 <- list()
Recon_WLS_FP_ETS_h5 <- list()
Recon_WLS_FP_ETS_h6 <- list()

Recon_WLS_FP_ARIMA_h1 <- list()
Recon_WLS_FP_ARIMA_h2 <- list()
Recon_WLS_FP_ARIMA_h3 <- list()
Recon_WLS_FP_ARIMA_h4 <- list()
Recon_WLS_FP_ARIMA_h5 <- list()
Recon_WLS_FP_ARIMA_h6 <- list()

Recon_MinT_FP_ETS_h1 <- list()
Recon_MinT_FP_ETS_h2 <- list()
Recon_MinT_FP_ETS_h3 <- list()
Recon_MinT_FP_ETS_h4 <- list()
Recon_MinT_FP_ETS_h5 <- list()
Recon_MinT_FP_ETS_h6 <- list()

Recon_MinT_FP_ARIMA_h1 <- list()
Recon_MinT_FP_ARIMA_h2 <- list()
Recon_MinT_FP_ARIMA_h3 <- list()
Recon_MinT_FP_ARIMA_h4 <- list()
Recon_MinT_FP_ARIMA_h5 <- list()
Recon_MinT_FP_ARIMA_h6 <- list()


Start <- Sys.time()

for (j in 1:p) {#p
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):nrow(AllTS),]
  
  #Note that for ETS models with multiplicative errors, the model residuals are different 
  #from the insample forecast errors. To get simualted bootstrap future paths we use model residuals
  #and to calculate MinT we use forecast errors.
  
  #List to store model fit for each series - use later to simualte future paths
  fit_ARIMA <- list(n)
  fit_ETS <- list(n)
  fit_Benchmark <- list(n)
  
  #Matrix to store model residuals
  ModelResid_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  ModelResid_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  ModelResid_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  ForeError_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  ForeError_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  Start_fc <- Sys.time()
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 12)
    
    ##Forecsting with ETS##
    fit_ETS[[i]] <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS[[i]], h = min(H, nrow(Test)))
    ModelResid_all_ETS[,i] <- residuals(fit_ETS[[i]])
    ForeError_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS[[i]]))
    
    
    #Forecsting with ARIMA
    fit_ARIMA[[i]] <- auto.arima(TS) # stepwise=FALSE,approx=FALSE
    Forecast_ARIMA <- forecast(fit_ARIMA[[i]], h = min(H, nrow(Test[,i])))
    ModelResid_all_ARIMA[,i] <- residuals(fit_ARIMA[[i]])
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA[[i]]))
    
  }  
  End_fc <- Sys.time() 
  
  ## Getting incoherent sample paths ##
  
  #List of lenght H to store future paths. Each element of this list corresponds to a matrix
  #that holds bootstrap future paths at forecast horizon H.
  Unrecon_future_paths_ETS <- list(min(H, nrow(Test)))
  Unrecon_future_paths_ARIMA <- list(min(H, nrow(Test)))
  
  #Index to get the block of bootsrap samples
  Index <- base::sample(c(1:(nrow(ModelResid_all_ETS)-(H-1))), size = B , replace = TRUE)
  Index_seq <- matrix(0, B, H)
  
  for (k in 1:B) {
    
    Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
    
  }
  
  #Simulating future paths
  Start_FP <- Sys.time()
  
  fp_ETS <-  lapply(c(1:B), FP_func, fit = fit_ETS, 
                    Resid = ModelResid_all_ETS, Index = Index, 
                    Index_seq = Index_seq, H=H, n=n)
  
  fp_ARIMA <-  lapply(c(1:B), FP_func, fit = fit_ARIMA, 
                      Resid = ModelResid_all_ARIMA, Index = Index, 
                      Index_seq = Index_seq, H=H, n=n)
  
  End_FP <- Sys.time()
  
  for (h in 1:min(H, nrow(Test))) {
    
    Unrecon_future_paths_ETS[[h]] <- plyr::laply(fp_ETS, function(y) y[h,])
    Unrecon_future_paths_ARIMA[[h]] <- plyr::laply(fp_ARIMA, function(y) y[h,])
    
  }
  
  
  
  
  ##Calculating different G matrices required for reconciliation of ETS base forecasts##
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  #MinT shrink G
  targ <- diag(diag(var(na.omit(ForeError_all_ETS))), n, n)
  shrink <- shrink.estim(na.omit(ForeError_all_ETS),targ)
  Shr.cov_ETS <- shrink[[1]]
  Inv_Shr.cov_ETS <- solve(Shr.cov_ETS)
  
  MinT.Shr_G_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
  
  #WLS G
  Cov_WLS_ETS <- diag(diag(Shr.cov_ETS), n, n)
  Inv_WLS_ETS <- diag(1/diag(var(na.omit(ForeError_all_ARIMA))), n, n)
  
  WLS_G_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
  
  
  
  ###Reconciliation of base forecasts from ETS###
  
  #List to store reconciled future paths from the full hierarchy
  Reconciled_future_paths_BU_ETS <- list()
  Reconciled_future_paths_OLS_ETS <- list()
  Reconciled_future_paths_WLS_ETS <- list()
  Reconciled_future_paths_MinT.Shr_ETS <- list()
  

  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ETS[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_OLS_ETS[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_WLS_ETS[[h]] <- t(S %*% WLS_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_MinT.Shr_ETS[[h]] <- t(S %*% MinT.Shr_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    

  }
  
  

  ##Calculating different G matrices required for reconciliation of ARIMA base forecasts##
  
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
  

  
  
  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_MinT.Shr_ARIMA[[h]] <- t(S %*% MinT.Shr_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    

  }
  

# Saving forecast distributions into the lists
  
  Unrecon_FP_ETS_h1[[j]] <- Unrecon_future_paths_ETS[[1]]
  Unrecon_FP_ETS_h2[[j]] <- Unrecon_future_paths_ETS[[2]]
  Unrecon_FP_ETS_h3[[j]] <- Unrecon_future_paths_ETS[[3]]
  Unrecon_FP_ETS_h4[[j]] <- Unrecon_future_paths_ETS[[4]]
  Unrecon_FP_ETS_h5[[j]] <- Unrecon_future_paths_ETS[[5]]
  Unrecon_FP_ETS_h6[[j]] <- Unrecon_future_paths_ETS[[6]]
  
  Unrecon_FP_ARIMA_h1[[j]] <- Unrecon_future_paths_ARIMA[[1]]
  Unrecon_FP_ARIMA_h2[[j]] <- Unrecon_future_paths_ARIMA[[2]]
  Unrecon_FP_ARIMA_h3[[j]] <- Unrecon_future_paths_ARIMA[[3]]
  Unrecon_FP_ARIMA_h4[[j]] <- Unrecon_future_paths_ARIMA[[4]]
  Unrecon_FP_ARIMA_h5[[j]] <- Unrecon_future_paths_ARIMA[[5]]
  Unrecon_FP_ARIMA_h6[[j]] <- Unrecon_future_paths_ARIMA[[6]]
  
  Recon_BU_FP_ETS_h1[[j]] <- Reconciled_future_paths_BU_ETS[[1]]
  Recon_BU_FP_ETS_h2[[j]] <- Reconciled_future_paths_BU_ETS[[2]]
  Recon_BU_FP_ETS_h3[[j]] <- Reconciled_future_paths_BU_ETS[[3]]
  Recon_BU_FP_ETS_h4[[j]] <- Reconciled_future_paths_BU_ETS[[4]]
  Recon_BU_FP_ETS_h5[[j]] <- Reconciled_future_paths_BU_ETS[[5]]
  Recon_BU_FP_ETS_h6[[j]] <- Reconciled_future_paths_BU_ETS[[6]]
  
  Recon_BU_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_BU_ARIMA[[1]]
  Recon_BU_FP_ARIMA_h2[[j]] <- Reconciled_future_paths_BU_ARIMA[[2]]
  Recon_BU_FP_ARIMA_h3[[j]] <- Reconciled_future_paths_BU_ARIMA[[3]]
  Recon_BU_FP_ARIMA_h4[[j]] <- Reconciled_future_paths_BU_ARIMA[[4]]
  Recon_BU_FP_ARIMA_h5[[j]] <- Reconciled_future_paths_BU_ARIMA[[5]]
  Recon_BU_FP_ARIMA_h6[[j]] <- Reconciled_future_paths_BU_ARIMA[[6]]
  
  Recon_OLS_FP_ETS_h1[[j]] <- Reconciled_future_paths_OLS_ETS[[1]]
  Recon_OLS_FP_ETS_h2[[j]] <- Reconciled_future_paths_OLS_ETS[[2]]
  Recon_OLS_FP_ETS_h3[[j]] <- Reconciled_future_paths_OLS_ETS[[3]]
  Recon_OLS_FP_ETS_h4[[j]] <- Reconciled_future_paths_OLS_ETS[[4]]
  Recon_OLS_FP_ETS_h5[[j]] <- Reconciled_future_paths_OLS_ETS[[5]]
  Recon_OLS_FP_ETS_h6[[j]] <- Reconciled_future_paths_OLS_ETS[[6]]
  
  Recon_OLS_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_OLS_ARIMA[[1]]
  Recon_OLS_FP_ARIMA_h2[[j]] <- Reconciled_future_paths_OLS_ARIMA[[2]]
  Recon_OLS_FP_ARIMA_h3[[j]] <- Reconciled_future_paths_OLS_ARIMA[[3]]
  Recon_OLS_FP_ARIMA_h4[[j]] <- Reconciled_future_paths_OLS_ARIMA[[4]]
  Recon_OLS_FP_ARIMA_h5[[j]] <- Reconciled_future_paths_OLS_ARIMA[[5]]
  Recon_OLS_FP_ARIMA_h6[[j]] <- Reconciled_future_paths_OLS_ARIMA[[6]]
  
  Recon_WLS_FP_ETS_h1[[j]] <- Reconciled_future_paths_WLS_ETS[[1]]
  Recon_WLS_FP_ETS_h2[[j]] <- Reconciled_future_paths_WLS_ETS[[2]]
  Recon_WLS_FP_ETS_h3[[j]] <- Reconciled_future_paths_WLS_ETS[[3]]
  Recon_WLS_FP_ETS_h4[[j]] <- Reconciled_future_paths_WLS_ETS[[4]]
  Recon_WLS_FP_ETS_h5[[j]] <- Reconciled_future_paths_WLS_ETS[[5]]
  Recon_WLS_FP_ETS_h6[[j]] <- Reconciled_future_paths_WLS_ETS[[6]]
  
  Recon_WLS_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_WLS_ARIMA[[1]]
  Recon_WLS_FP_ARIMA_h2[[j]] <- Reconciled_future_paths_WLS_ARIMA[[2]]
  Recon_WLS_FP_ARIMA_h3[[j]] <- Reconciled_future_paths_WLS_ARIMA[[3]]
  Recon_WLS_FP_ARIMA_h4[[j]] <- Reconciled_future_paths_WLS_ARIMA[[4]]
  Recon_WLS_FP_ARIMA_h5[[j]] <- Reconciled_future_paths_WLS_ARIMA[[5]]
  Recon_WLS_FP_ARIMA_h6[[j]] <- Reconciled_future_paths_WLS_ARIMA[[6]]
  
  Recon_MinT_FP_ETS_h1[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[1]]
  Recon_MinT_FP_ETS_h2[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[2]]
  Recon_MinT_FP_ETS_h3[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[3]]
  Recon_MinT_FP_ETS_h4[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[4]]
  Recon_MinT_FP_ETS_h5[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[5]]
  Recon_MinT_FP_ETS_h6[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[6]]
  
  Recon_MinT_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[1]]
  Recon_MinT_FP_ARIMA_h2[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[2]]
  Recon_MinT_FP_ARIMA_h3[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[3]]
  Recon_MinT_FP_ARIMA_h4[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[4]]
  Recon_MinT_FP_ARIMA_h5[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[5]]
  Recon_MinT_FP_ARIMA_h6[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[6]]

  
}

End <- Sys.time()



# This data is saved in Empirical-study/Results/NonPara-approach/Without-OptG
# save.image("Results/Forecasting_OvernightTrips_NonParaMethod_Fdistributions.RData")


