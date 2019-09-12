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

for (j in 1:1) {#p
  
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
    
    #To store unreconcile future paths of states
    Unrecon_future_paths_States_ETS <- list(min(H, nrow(Test)))
    Unrecon_future_paths_States_ARIMA <- list(min(H, nrow(Test)))
    
    #To store unreconcile future paths of states
    Unrecon_future_paths_Zones_ETS <- list(min(H, nrow(Test)))
    Unrecon_future_paths_Zones_ARIMA <- list(min(H, nrow(Test)))
    
    #To store unreconcile future paths of states
    Unrecon_future_paths_Regions_ETS <- list(min(H, nrow(Test)))
    Unrecon_future_paths_Regions_ARIMA <- list(min(H, nrow(Test)))
    
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
      
      Unrecon_future_paths_States_ETS[[h]] <- Unrecon_future_paths_ETS[[h]][,(l1+1):(l1+l2)]
      Unrecon_future_paths_States_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(l1+1):(l1+l2)]
      
      Unrecon_future_paths_Zones_ETS[[h]] <- Unrecon_future_paths_ETS[[h]][,(l1+l2+1):(l1+l2+l3)]
      Unrecon_future_paths_Zones_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
      
      Unrecon_future_paths_Regions_ETS[[h]] <- Unrecon_future_paths_ETS[[h]][,(n-m+1):n]
      Unrecon_future_paths_Regions_ARIMA[[h]] <- Unrecon_future_paths_ARIMA[[h]][,(n-m+1):n]
      
      
    }
  
      
    
      
    ##Calculating different G matrices required for reconciliation of ETS base forecasts##
      
    #Bottom up 
    
    Null.ma <- matrix(0,m,(n-m))
    BU_G <- cbind(Null.ma, diag(1,m,m))
    
    #OLS G
    OLS_G <- solve(t(S) %*% S) %*% t(S)
    
    #MinT shrink G
    targ <- lowerD(ForeError_all_ETS)
    shrink <- shrink.estim(ForeError_all_ETS,targ)
    Shr.cov_ETS <- shrink[[1]]
    Inv_Shr.cov_ETS <- solve(Shr.cov_ETS)
    
    MinT.Shr_G_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
    
    #WLS G
    Cov_WLS_ETS <- diag(diag(Shr.cov_ETS), n, n)
    Inv_WLS_ETS <- solve(Cov_WLS_ETS)
    
    WLS_G_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
    
    
    
    ###Reconciliation of base forecasts from ETS###
    
    #List to store reconciled future paths from the full hierarchy
    Reconciled_future_paths_BU_ETS <- list()
    Reconciled_future_paths_OLS_ETS <- list()
    Reconciled_future_paths_WLS_ETS <- list()
    Reconciled_future_paths_MinT.Shr_ETS <- list()
    
    #List to store reconciled future paths from states
    Reconciled_future_paths_States_BU_ETS <- list()
    Reconciled_future_paths_States_OLS_ETS <- list()
    Reconciled_future_paths_States_WLS_ETS <- list()
    Reconciled_future_paths_States_MinT.Shr_ETS <- list()
    
    #List to store reconciled future paths from zones
    Reconciled_future_paths_Zones_BU_ETS <- list()
    Reconciled_future_paths_Zones_OLS_ETS <- list()
    Reconciled_future_paths_Zones_WLS_ETS <- list()
    Reconciled_future_paths_Zones_MinT.Shr_ETS <- list()
    
    #List to store reconciled future paths from regions
    Reconciled_future_paths_Regions_BU_ETS <- list()
    Reconciled_future_paths_Regions_OLS_ETS <- list()
    Reconciled_future_paths_Regions_WLS_ETS <- list()
    Reconciled_future_paths_Regions_MinT.Shr_ETS <- list()
    
    #To store univariate scores
    CRPS_BU_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_OLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_WLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_MinT.Shr_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_Unrecon_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    
    
    for (h in 1: min(H, nrow(Test))) {
      
      Reconciled_future_paths_BU_ETS[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ETS[[h]]))
      Reconciled_future_paths_OLS_ETS[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ETS[[h]]))
      Reconciled_future_paths_WLS_ETS[[h]] <- t(S %*% WLS_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
      Reconciled_future_paths_MinT.Shr_ETS[[h]] <- t(S %*% MinT.Shr_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
      
      #Reconciled densities for states
      Reconciled_future_paths_States_BU_ETS[[h]] <- Reconciled_future_paths_BU_ETS[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_OLS_ETS[[h]] <- Reconciled_future_paths_OLS_ETS[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_WLS_ETS[[h]] <- Reconciled_future_paths_WLS_ETS[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_MinT.Shr_ETS[[h]] <- Reconciled_future_paths_MinT.Shr_ETS[[h]][,(l1+1):(l1+l2)]
      
      #Reconciled densities for zones
      Reconciled_future_paths_Zones_BU_ETS[[h]] <- Reconciled_future_paths_BU_ETS[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_OLS_ETS[[h]] <- Reconciled_future_paths_OLS_ETS[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_WLS_ETS[[h]] <- Reconciled_future_paths_WLS_ETS[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_MinT.Shr_ETS[[h]] <- Reconciled_future_paths_MinT.Shr_ETS[[h]][,(l1+l2+1):(l1+l2+l3)]
      
      #Reconciled densities for regions
      Reconciled_future_paths_Regions_BU_ETS[[h]] <- Reconciled_future_paths_BU_ETS[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_OLS_ETS[[h]] <- Reconciled_future_paths_OLS_ETS[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_WLS_ETS[[h]] <- Reconciled_future_paths_WLS_ETS[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_MinT.Shr_ETS[[h]] <- Reconciled_future_paths_MinT.Shr_ETS[[h]][,(n-m+1):n]
      
      #Calculating CRPS for univariate predictive densities
      
      for (i in 1:n) {
        
        CRPS_Unrecon_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Unrecon_future_paths_ETS[[h]][,i],
                                             method = "edf")
        CRPS_BU_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_BU_ETS[[h]][,i],
                                        method = "edf")
        CRPS_OLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_OLS_ETS[[h]][,i],
                                         method = "edf")
        CRPS_WLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_WLS_ETS[[h]][,i],
                                         method = "edf")
        CRPS_MinT.Shr_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_MinT.Shr_ETS[[h]][,i],
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
    ES_full_BU_ETS <- mapply(Energy_score, Data = Reconciled_future_paths_BU_ETS, Real = Test.list_full)
    ES_full_OLS_ETS <- mapply(Energy_score, Reconciled_future_paths_OLS_ETS, Real = Test.list_full)
    ES_full_WLS_ETS <- mapply(Energy_score, Reconciled_future_paths_WLS_ETS, Real = Test.list_full)
    ES_full_MinT.Shr_ETS <- mapply(Energy_score, Reconciled_future_paths_MinT.Shr_ETS, Real = Test.list_full)
    ES_full_Unrecon_ETS <- mapply(Energy_score, Unrecon_future_paths_ETS, Real = Test.list_full)
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ETS <- mapply(Variogram_score, Data = Reconciled_future_paths_BU_ETS, Real = Test.list_full)
    VS_full_OLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_OLS_ETS, Real = Test.list_full)
    VS_full_WLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_WLS_ETS, Real = Test.list_full)
    VS_full_MinT.Shr_ETS <- mapply(Variogram_score, Reconciled_future_paths_MinT.Shr_ETS, Real = Test.list_full)
    VS_full_Unrecon_ETS <- mapply(Variogram_score, Unrecon_future_paths_ETS, Real = Test.list_full)
    
    #Calculating Energy score for predicive densities of states
    ES_states_BU_ETS <- mapply(Energy_score, Data = Reconciled_future_paths_States_BU_ETS, 
                               Real = Test.list_states)
    ES_states_OLS_ETS <- mapply(Energy_score, Reconciled_future_paths_States_OLS_ETS, 
                                Real = Test.list_states)
    ES_states_WLS_ETS <- mapply(Energy_score, Reconciled_future_paths_States_WLS_ETS, 
                                Real = Test.list_states)
    ES_states_MinT.Shr_ETS <- mapply(Energy_score, Reconciled_future_paths_States_MinT.Shr_ETS, 
                                     Real = Test.list_states)
    ES_states_Unrecon_ETS <- mapply(Energy_score, Unrecon_future_paths_States_ETS, 
                                    Real = Test.list_states)
    
    #Calculating Variogram score for predicive densities of states
    VS_states_BU_ETS <- mapply(Variogram_score, Data = Reconciled_future_paths_States_BU_ETS, 
                               Real = Test.list_states)
    VS_states_OLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_States_OLS_ETS, 
                                Real = Test.list_states)
    VS_states_WLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_States_WLS_ETS, 
                                Real = Test.list_states)
    VS_states_MinT.Shr_ETS <- mapply(Variogram_score, Reconciled_future_paths_States_MinT.Shr_ETS, 
                                     Real = Test.list_states)
    VS_states_Unrecon_ETS <- mapply(Variogram_score, Unrecon_future_paths_States_ETS, 
                                    Real = Test.list_states)
    
    #Calculating Energy score for predicive densities of zones
    ES_zones_BU_ETS <- mapply(Energy_score, Data = Reconciled_future_paths_Zones_BU_ETS, 
                               Real = Test.list_zones)
    ES_zones_OLS_ETS <- mapply(Energy_score, Reconciled_future_paths_Zones_OLS_ETS, 
                                Real = Test.list_zones)
    ES_zones_WLS_ETS <- mapply(Energy_score, Reconciled_future_paths_Zones_WLS_ETS, 
                                Real = Test.list_zones)
    ES_zones_MinT.Shr_ETS <- mapply(Energy_score, Reconciled_future_paths_Zones_MinT.Shr_ETS, 
                                     Real = Test.list_zones)
    ES_zones_Unrecon_ETS <- mapply(Energy_score, Unrecon_future_paths_Zones_ETS, 
                                    Real = Test.list_zones)
    
    #Calculating Variogram score for predicive densities of zones
    VS_zones_BU_ETS <- mapply(Variogram_score, Data = Reconciled_future_paths_Zones_BU_ETS, 
                               Real = Test.list_zones)
    VS_zones_OLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_Zones_OLS_ETS, 
                                Real = Test.list_zones)
    VS_zones_WLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_Zones_WLS_ETS, 
                                Real = Test.list_zones)
    VS_zones_MinT.Shr_ETS <- mapply(Variogram_score, Reconciled_future_paths_Zones_MinT.Shr_ETS, 
                                     Real = Test.list_zones)
    VS_zones_Unrecon_ETS <- mapply(Variogram_score, Unrecon_future_paths_Zones_ETS, 
                                    Real = Test.list_zones)
    
    
    #Calculating Energy score for predicive densities of regions
    ES_regions_BU_ETS <- mapply(Energy_score, Data = Reconciled_future_paths_Regions_BU_ETS, 
                              Real = Test.list_regions)
    ES_regions_OLS_ETS <- mapply(Energy_score, Reconciled_future_paths_Regions_OLS_ETS, 
                               Real = Test.list_regions)
    ES_regions_WLS_ETS <- mapply(Energy_score, Reconciled_future_paths_Regions_WLS_ETS, 
                               Real = Test.list_regions)
    ES_regions_MinT.Shr_ETS <- mapply(Energy_score, Reconciled_future_paths_Regions_MinT.Shr_ETS, 
                                    Real = Test.list_regions)
    ES_regions_Unrecon_ETS <- mapply(Energy_score, Unrecon_future_paths_Regions_ETS, 
                                   Real = Test.list_regions)
    
    #Calculating Variogram score for predicive densities of zones
    VS_regions_BU_ETS <- mapply(Variogram_score, Data = Reconciled_future_paths_Regions_BU_ETS, 
                              Real = Test.list_regions)
    VS_regions_OLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_Regions_OLS_ETS, 
                               Real = Test.list_regions)
    VS_regions_WLS_ETS <- mapply(Variogram_score, Reconciled_future_paths_Regions_WLS_ETS, 
                               Real = Test.list_regions)
    VS_regions_MinT.Shr_ETS <- mapply(Variogram_score, Reconciled_future_paths_Regions_MinT.Shr_ETS, 
                                    Real = Test.list_regions)
    VS_regions_Unrecon_ETS <- mapply(Variogram_score, Unrecon_future_paths_Regions_ETS, 
                                   Real = Test.list_regions)
    
 
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
    
    
    #Adding to ES_states to data frame
    
    DF_MultiV_States %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_Unrecon_ETS, 
          "Variogram score" = VS_states_Unrecon_ETS) -> DF_Base
    DF_Base[names(DF_MultiV_States)] -> DF_Base
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_states_BU_ETS, 
          "Variogram score" = VS_states_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_States)] -> DF_BU
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_OLS_ETS, 
          "Variogram score" = VS_states_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_States)] -> DF_OLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_WLS_ETS, 
          "Variogram score" = VS_states_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_States)] -> DF_WLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_MinT.Shr_ETS, 
          "Variogram score" = VS_states_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
    
    
    #Adding to ES_zones to data frame
    
    DF_MultiV_Zones %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_Unrecon_ETS, 
          "Variogram score" = VS_zones_Unrecon_ETS) -> DF_Base
    DF_Base[names(DF_MultiV_Zones)] -> DF_Base
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_zones_BU_ETS, 
          "Variogram score" = VS_zones_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_Zones)] -> DF_BU
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_OLS_ETS, 
          "Variogram score" = VS_zones_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_WLS_ETS, 
          "Variogram score" = VS_zones_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_MinT.Shr_ETS, 
          "Variogram score" = VS_zones_MinT.Shr_ETS) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
    
    
    
    #Adding to ES_regions to data frame
    
    DF_MultiV_Regions %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_Unrecon_ETS, 
          "Variogram score" = VS_regions_Unrecon_ETS) -> DF_Base
    DF_Base[names(DF_MultiV_Regions)] -> DF_Base
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_regions_BU_ETS, 
          "Variogram score" = VS_regions_BU_ETS) -> DF_BU
    DF_BU[names(DF_MultiV_Regions)] -> DF_BU
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_OLS_ETS, 
          "Variogram score" = VS_regions_OLS_ETS) -> DF_OLS
    DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_WLS_ETS, 
          "Variogram score" = VS_regions_WLS_ETS) -> DF_WLS
    DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_MinT.Shr_ETS, 
          "Variogram score" = VS_regions_MinT.Shr_ETS) -> DF_MinT.Shr
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
    
    #MinT shrink G
    targ <- lowerD(ForeError_all_ARIMA)
    shrink <- shrink.estim(ForeError_all_ARIMA,targ)
    Shr.cov_ARIMA <- shrink[[1]]
    Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
    
    MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
    
    #WLS G
    Cov_WLS_ARIMA <- diag(diag(Shr.cov_ARIMA), n, n)
    Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
    
    WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
    
    
    
    ###Reconciliation of base forecasts from ARIMA###
    
    #List to store reconciled future paths from the full hierarchy
    Reconciled_future_paths_BU_ARIMA <- list()
    Reconciled_future_paths_OLS_ARIMA <- list()
    Reconciled_future_paths_WLS_ARIMA <- list()
    Reconciled_future_paths_MinT.Shr_ARIMA <- list()
    
    #List to store reconciled future paths from states
    Reconciled_future_paths_States_BU_ARIMA <- list()
    Reconciled_future_paths_States_OLS_ARIMA <- list()
    Reconciled_future_paths_States_WLS_ARIMA <- list()
    Reconciled_future_paths_States_MinT.Shr_ARIMA <- list()
    
    #List to store reconciled future paths from zones
    Reconciled_future_paths_Zones_BU_ARIMA <- list()
    Reconciled_future_paths_Zones_OLS_ARIMA <- list()
    Reconciled_future_paths_Zones_WLS_ARIMA <- list()
    Reconciled_future_paths_Zones_MinT.Shr_ARIMA <- list()
    
    #List to store reconciled future paths from regions
    Reconciled_future_paths_Regions_BU_ARIMA <- list()
    Reconciled_future_paths_Regions_OLS_ARIMA <- list()
    Reconciled_future_paths_Regions_WLS_ARIMA <- list()
    Reconciled_future_paths_Regions_MinT.Shr_ARIMA <- list()
    
    #To store univariate scores
    CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
    
    
    for (h in 1: min(H, nrow(Test))) {
      
      Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
      Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
      Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
      Reconciled_future_paths_MinT.Shr_ARIMA[[h]] <- t(S %*% MinT.Shr_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
      
      #Reconciled densities for states
      Reconciled_future_paths_States_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(l1+1):(l1+l2)]
      Reconciled_future_paths_States_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(l1+1):(l1+l2)]
      
      #Reconciled densities for zones
      Reconciled_future_paths_Zones_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
      Reconciled_future_paths_Zones_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(l1+l2+1):(l1+l2+l3)]
      
      #Reconciled densities for regions
      Reconciled_future_paths_Regions_BU_ARIMA[[h]] <- Reconciled_future_paths_BU_ARIMA[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_OLS_ARIMA[[h]] <- Reconciled_future_paths_OLS_ARIMA[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_WLS_ARIMA[[h]] <- Reconciled_future_paths_WLS_ARIMA[[h]][,(n-m+1):n]
      Reconciled_future_paths_Regions_MinT.Shr_ARIMA[[h]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,(n-m+1):n]
      
      #Calculating CRPS for univariate predictive densities
      
      for (i in 1:n) {
        
        CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Unrecon_future_paths_ARIMA[[h]][,i],
                                               method = "edf")
        CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_BU_ARIMA[[h]][,i],
                                          method = "edf")
        CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_OLS_ARIMA[[h]][,i],
                                           method = "edf")
        CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_WLS_ARIMA[[h]][,i],
                                           method = "edf")
        CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,i],
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
    ES_full_BU_ARIMA <- mapply(Energy_score, Data = Reconciled_future_paths_BU_ARIMA, Real = Test.list_full)
    ES_full_OLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_OLS_ARIMA, Real = Test.list_full)
    ES_full_WLS_ARIMA <- mapply(Energy_score, Reconciled_future_paths_WLS_ARIMA, Real = Test.list_full)
    ES_full_MinT.Shr_ARIMA <- mapply(Energy_score, Reconciled_future_paths_MinT.Shr_ARIMA, Real = Test.list_full)
    ES_full_Unrecon_ARIMA <- mapply(Energy_score, Unrecon_future_paths_ARIMA, Real = Test.list_full)
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ARIMA <- mapply(Variogram_score, Data = Reconciled_future_paths_BU_ARIMA, Real = Test.list_full)
    VS_full_OLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_OLS_ARIMA, Real = Test.list_full)
    VS_full_WLS_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_WLS_ARIMA, Real = Test.list_full)
    VS_full_MinT.Shr_ARIMA <- mapply(Variogram_score, Reconciled_future_paths_MinT.Shr_ARIMA, Real = Test.list_full)
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
    VS_regions_Unrecon_ARIMA <- mapply(Variogram_score, Unrecon_future_paths_Regions_ARIMA, 
                                       Real = Test.list_regions)
    
    
    #Adding to ES_full to data frame
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
    
    
    #Adding to ES_states to data frame
    
    DF_MultiV_States %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_Unrecon_ARIMA, 
          "Variogram score" = VS_states_Unrecon_ARIMA) -> DF_Base
    DF_Base[names(DF_MultiV_States)] -> DF_Base
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_states_BU_ARIMA, 
          "Variogram score" = VS_states_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_States)] -> DF_BU
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_OLS_ARIMA, 
          "Variogram score" = VS_states_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_States)] -> DF_OLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_WLS_ARIMA, 
          "Variogram score" = VS_states_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_States)] -> DF_WLS
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_states_MinT.Shr_ARIMA, 
          "Variogram score" = VS_states_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_States)] -> DF_MinT.Shr
    DF_MultiV_States <- rbind(DF_MultiV_States, DF_MinT.Shr)
    
    
    #Adding to ES_zones to data frame
    
    DF_MultiV_Zones %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_Unrecon_ARIMA, 
          "Variogram score" = VS_zones_Unrecon_ARIMA) -> DF_Base
    DF_Base[names(DF_MultiV_Zones)] -> DF_Base
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_zones_BU_ARIMA, 
          "Variogram score" = VS_zones_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_Zones)] -> DF_BU
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_OLS_ARIMA, 
          "Variogram score" = VS_zones_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_Zones)] -> DF_OLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_WLS_ARIMA, 
          "Variogram score" = VS_zones_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_Zones)] -> DF_WLS
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_zones_MinT.Shr_ARIMA, 
          "Variogram score" = VS_zones_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Zones)] -> DF_MinT.Shr
    DF_MultiV_Zones <- rbind(DF_MultiV_Zones, DF_MinT.Shr)
    
    
    
    #Adding to ES_regions to data frame
    
    DF_MultiV_Regions %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_Unrecon_ARIMA, 
          "Variogram score" = VS_regions_Unrecon_ARIMA) -> DF_Base
    DF_Base[names(DF_MultiV_Regions)] -> DF_Base
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_Base)
    
    cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))),
          "Energy score" = ES_regions_BU_ARIMA, 
          "Variogram score" = VS_regions_BU_ARIMA) -> DF_BU
    DF_BU[names(DF_MultiV_Regions)] -> DF_BU
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_BU)
    
    cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_OLS_ARIMA, 
          "Variogram score" = VS_regions_OLS_ARIMA) -> DF_OLS
    DF_OLS[names(DF_MultiV_Regions)] -> DF_OLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_OLS)
    
    cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_WLS_ARIMA, 
          "Variogram score" = VS_regions_WLS_ARIMA) -> DF_WLS
    DF_WLS[names(DF_MultiV_Regions)] -> DF_WLS
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_WLS)
    
    cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), 
          "Energy score" = ES_regions_MinT.Shr_ARIMA, 
          "Variogram score" = VS_regions_MinT.Shr_ARIMA) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_MultiV_Regions)] -> DF_MinT.Shr
    DF_MultiV_Regions <- rbind(DF_MultiV_Regions, DF_MinT.Shr)
    
    
    #Addinng CRPS to the DF
    
    DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
      dplyr::select("F-method", "Replication") -> Fltr
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), 
          "CRPS" = c(t(CRPS_Unrecon_ARIMA))) -> DF_Base
    DF_Base[names(DF_UniV)] -> DF_Base
    DF_UniV <- rbind(DF_UniV, DF_Base)
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), 
          "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
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
    
    cbind(Fltr, "Series" = rep(names(AllTS), min(H, nrow(Test))), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
          "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = n), "CRPS" = c(t(CRPS_MinT.Shr_ARIMA))) -> DF_MinT.Shr
    DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
    DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
    
  
}

End <- Sys.time()



DF_MultiV_Total[complete.cases(DF_MultiV_Total[ , "R-method"]),] -> DF_MultiV_Total
DF_MultiV_States[complete.cases(DF_MultiV_States[ , "R-method"]),] -> DF_MultiV_States
DF_MultiV_Zones[complete.cases(DF_MultiV_Zones[ , "R-method"]),] -> DF_MultiV_Zones
DF_MultiV_Regions[complete.cases(DF_MultiV_Regions[ , "R-method"]),] -> DF_MultiV_Regions

save.image("Forecasting_OvernightTrips_NonParaMethod.Rdata")
write.csv(DF_MultiV_Total, "DF_MultiV_Total.csv")
write.csv(DF_MultiV_States, "DF_MultiV_States.csv")
write.csv(DF_MultiV_Zones, "DF_MultiV_Zones.csv")
write.csv(DF_MultiV_Regions, "DF_MultiV_Regions.csv")