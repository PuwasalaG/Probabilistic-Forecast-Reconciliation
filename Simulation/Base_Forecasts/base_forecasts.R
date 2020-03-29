#This script fits base models.
library(tidyverse)
library(slider)
library(furrr)
library(tsibble)
library(readr)
library(tidyr)
library(fable)

#Read Simulation Table
simtable<-read_csv('SimulationTable.csv')

### Extract flags from simulation scenario

#scen<-1 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this


simj<-simtable[scen,] #Extract row of table
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
modelj<-simj$model #Is model ARIMA or ETS

#Read in data
data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))

#Sample sizes
H<-3 # Maximum Forecast Horizon
N<-500 # Size of window
L<-4 # Lags to leave at beginning of window
B<-1000 # Number of sample paths for probabilistic forecasts
R<-1000 # Number of replications


#Make data windows
data_windows<-list_window<-slider::slide(data,~.x,.after=N+L-1,.complete = T)


#Function for one window
forecast_window <- function(data_w){
  data_w%>%pivot_longer(-Time,names_to = 'Var')%>%
    as_tsibble(key = Var,index = Time)->d
  if(modelj=='arima'){
    m<-model(d,arima=ARIMA(value))
  }else{
    m<-model(d,ets=ETS(value))
  }
            
  f<-forecast(m,h=H)
  if(innovationsj=='bootstrap'){
    p<-generate(m,bootstrap=T,times=B,h=H)
  }else{
    p<-generate(m,bootstrap=F,times=B,h=H)
  }
  return(list(mable=m,forecast=f,paths=p))
}

#Get all results
all<-map(data_windows[1:R],forecast_window)

#Save output
saveRDS(all,paste0('../Base_Results/',
                   distj,'_',
                   trendj,'_',
                   modelj,'_',
                   innovationsj,'.rds'))
