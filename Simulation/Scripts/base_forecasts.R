#This script fits base models.
library(tidyverse)
library(slider)
library(furrr)
library(tsibble)
library(readr)
library(tidyr)
library(fable)

data<-read_csv('../Data/Bottom_Level_Gaussian_Simulated.csv')

H<-1 # Maximum Forecast Horizon
N<-500 # Size of window
L<-4 # Lags to leave at beginning of window
B<-1 # Number of sample paths for probabilistic forecasts


#Make data windows
data_windows<-list_window<-slider::slide(data,~.x,.after=N,.complete = T)



forecast_window <- function(data){
  data%>%pivot_longer(-Time,names_to = 'Var')%>%
    as_tsibble(key = Var,index = Time)%>%
    model(arima=ARIMA(value))->m
  f<-forecast(m,h=H)
  p<-generate(m,bootstrap=T,times=B,h=H)
  return(list(mable=m,forecast=f,paths=p))
}

plan(multicore,workers=availableCores()-1)
a<-map(data_windows[1:1000],forecast_window)
plan(sequential)
saveRDS(a,'baseforecasts.rds')
