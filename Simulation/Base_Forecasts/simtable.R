#### Code for handling multiple simulations scenarios with a single script ####

### This code creates a table of data for different simulations scenarios ###
library(tidyr)
library(readr)
dist<-c('gaussian','nongaussian') #Is the DGP Gaussian or non-Gaussian
trend<-c('stationary','nonstationary') #Are the data stationary or non-stationary
innovations<-c('bootstrap','gaussian') #Should innovations be bootstrapped?
model<-c('arima','ets') #Should models be ARIMA or ETS?
dep<-c('independent', 'joint') #Should innovations be drawn independently or jointly?

#Put into a single table
simtable<-expand_grid(dist,trend,innovations,model,dep)

#Export as .csv

write_csv(simtable,'SimulationTable.csv')
