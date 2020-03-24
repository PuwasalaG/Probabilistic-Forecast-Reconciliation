#This script fits base models.
library(tsibble)
library(tidyverse)
library(fable)
library(slider)
library(furrr)

data<-read_csv('../Data/Bottom_Level_Gaussian_Simulated.csv')

H<-1 #Forecast Horizon
N<-500 #Size of window


#Make data windows
data_windows<-list_window<-slider::slide(data,~.x,.after=N,.complete = T)



forecast_window <- function(data){
  data%>%pivot_longer(-Time,names_to = 'Var')%>%
    as_tsibble(key = Var,index = Time)%>%
    model(ets=ETS(value))->m
  p<-fabletools::generate(m,times=5,bootstrap=T)
  return(list(mable=m,paths=p))
}

plan(multicore,workers=availableCores()-1)
a<-map(data_windows[1:21],forecast_window)
plan(sequential)

