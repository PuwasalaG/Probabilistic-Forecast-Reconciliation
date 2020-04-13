#This script fits base models.

library(tidyverse)
library(slider)
library(furrr)
library(tsibble)
library(readr)
library(tidyr)
library(fable)

#Clear workspace
rm(list=ls())


#Function for Shaefer Strimmer Shrinkage

shrink.estim <- function(res)
{
  n<-nrow(res)
  covm <- crossprod(res)/nrow(res)
  tar <- diag(diag(covm))
  corm <- stats::cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  xs <- xs[stats::complete.cases(xs),]
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- stats::cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  W <- lambda * tar + (1 - lambda) * covm
  return(W)
}

#Read Simulation Table
simtable<-read_csv('SimulationTable.csv')

### Extract flags from simulation scenario

scen<-1 #If running within R uncomment this.  This will only run first scenario
#scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this


simj<-simtable[scen,] #Extract row of table
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
modelj<-simj$model #Is model ARIMA or ETS

#Read in data
data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))

#Sample sizes
H<-1 # Maximum Forecast Horizon
N<-500 # Size of window
L<-4 # Lags to leave at beginning of window
B<-1000 # Number of sample paths for probabilistic forecasts
W<-1000 # Number of windows


#Make data windows
data_windows<-list_window<-slider::slide(data,~.x,.after=N+L-1,.complete = T)


#Function for one window
forecast_window <- function(data_w){
  #Find start of window
  window<-max(data_w$Time)
  
  data_w%>%pivot_longer(-Time,names_to = 'Var')%>%
    as_tsibble(key = Var,index = Time)->d
  if(modelj=='arima'){
    m<-model(d,.model=ARIMA(value))
  }else{
    m<-model(d,.model=ETS(value))
  }
            
  f<-forecast(m,h=H)
  
  #Find Residuals
  m%>%residuals%>%
    select(-.model)%>%
    pivot_wider(id_cols = Time,
                names_from = Var,
                values_from = .resid)%>%
    select(-Time)%>%
    as.matrix()->E
  
  #Find Sigma (sample and shrink)
  Sigma_sam<-t(E)%*%E/(nrow(E)-1)
  Sigma_shr<-shrink.estim(E)
  
  if(innovationsj=='bootstrap'){
    p<-generate(m,bootstrap=T,times=B,h=H)
  }else{
    p<-generate(m,bootstrap=F,times=B,h=H)
  }
  
  
  
  return(list(mable=m%>%add_column(.window=window),
              forecast=f%>%add_column(.window=window),
              paths=p%>%add_column(.window=window),
              Sigma_sam=Sigma_sam,
              Sigma_shr=Sigma_shr))
}

#Get all results
all<-map(data_windows[1:W],forecast_window)%>%
  purrr::transpose() #Transpose to change access order in list

#Isolate paths and combine to single df
paths<-all$paths%>%
  map_dfr(function(x){return(as_tibble(x))})

#Isolate mable and combine to single df

mable<-all$mable%>%
  map_dfr(function(x){return(as_tibble(x))})

#Isolate mable and combine to single df

forecast<-all$forecast%>%
  map_dfr(function(x){return(as_tibble(x))})



#Save output
saveRDS(paths,paste0('../Base_Results/',
                   distj,'_',
                   trendj,'_',
                   modelj,'_',
                   innovationsj,'_paths.rds'))

saveRDS(mable,paste0('../Base_Results/',
                     distj,'_',
                     trendj,'_',
                     modelj,'_',
                     innovationsj,'_mable.rds'))

saveRDS(forecast,paste0('../Base_Results/',
                     distj,'_',
                     trendj,'_',
                     modelj,'_',
                     innovationsj,'_forecast.rds'))

saveRDS(all$Sigma_sam,paste0('../Base_Results/',
                        distj,'_',
                        trendj,'_',
                        modelj,'_',
                        innovationsj,'_Sigma_sam.rds'))

saveRDS(all$Sigma_shr,paste0('../Base_Results/',
                        distj,'_',
                        trendj,'_',
                        modelj,'_',
                        innovationsj,'_Sigma_shr.rds'))

