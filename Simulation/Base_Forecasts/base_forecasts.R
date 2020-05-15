#This script selects and fits base models for each window. 
#The forecast mean, forecast standard deviation, two estimates of the covariance matrix and residuals are retained.
library(magrittr)
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
  covm <- cov(res)
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

#scen<-1 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this


simj<-simtable[scen,] #Extract row of table
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
modelj<-simj$model #Is model ARIMA or ETS

#Read in data
data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))

#Sample sizes
N<-500 # Size of window
L<-4 # Lags to leave at beginning of window
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
            
  forecast(m,h=1)%>%
    arrange(match(Var,c("Tot","A","B","AA","AB","BA","BB")))->f
  fc_mean<-map_dbl(f$.distribution,use_series,mean)
  fc_sd<-map_dbl(f$.distribution,use_series,sd)
  
  #Find Residuals
  residuals(m)%>%
    select(-.model)%>%
    arrange(match(Var,c("Tot","A","B","AA","AB","BA","BB")))%>%
    pivot_wider(id_cols = Time,
                names_from = Var,
                values_from = .resid)%>%
    select(-Time)%>%
    as.matrix()%>%t()->E
  rownames(E)<-NULL
  
  #Find Sigma (sample and shrink)
  Sigma_sam<-cov(t(E))
  Sigma_shr<-shrink.estim(t(E))
  
  
  return(list(mable=m%>%add_column(.window=window),
              fc_mean=fc_mean,
              fc_sd=fc_sd,
              fc_Sigma_sam=Sigma_sam,
              fc_Sigma_shr=Sigma_shr,
              resid=E))
}
#Get all results
all<-map(data_windows[1:W],forecast_window)

all_nomable<-map(all,function(x){x[-1]}) #Delete mable
all_mable<-map(all,function(x){x[1]}) #Extract mable
  

#Save output
saveRDS(all_nomable,paste0('../Base_Results/',
                   distj,'_',
                   trendj,'_',
                   modelj,'_base.rds'))

#Save output
saveRDS(all_mable,paste0('../Base_Results/',
                           distj,'_',
                           trendj,'_',
                           modelj,'_mable.rds'))
