#### Code for handling multiple simulations scenarios with a single script ####

### This code creates a table of data for different simulations scenarios ###
library(tidyr)
library(readr)
score<-c('energy','variogram') #Is energy or variogram score optimised
dist<-c('gaussian','nongaussian') #Is the DGP Gaussian or non-Gaussian
trend<-c('stationary','nonstationary') #Are the data stationary or non-stationary
model<-c('arima','ets') #Should models be ARIMA or ETS?
dep<-c('independent', 'joint') #Should innovations be drawn independently or jointly?
innovations<-c('bootstrap','gaussian') #Should innovations be bootstrapped?

#Put into a single table
simtable<-expand_grid(score,dist,trend,model,dep,innovations)

#Export as .csv

write_csv(simtable,'SimulationTable.csv')
