#### Code for handling multiple simulations scenarios with a single script ####

### This code creates a table of data for different simulations scenarios ###
library(tidyr)
library(readr)
dist<-c('gaussian','nongaussian') #Is the DGP Gaussian or non-Gaussian
trend<-c('stationary','nonstationary') #Are the data stationary or non-stationary
model<-c('arima','ets') #Should models be ARIMA or ETS?

#Put into a single table
simtable<-expand_grid(dist,trend,model)

#Export as .csv

write_csv(simtable,'SimulationTable.csv')
