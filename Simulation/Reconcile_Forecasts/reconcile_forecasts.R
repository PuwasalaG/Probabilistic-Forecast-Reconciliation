#This script reconciles forecasts.

library(tidyverse)
library(furrr)
library(readr)
library(ProbReco)

#Clear workspace
rm(list=ls())


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
depj<-simj$dep #Are innovations drawn independently or jointly?

#Read in data
data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))


#Read in simulated paths
paths<-read_csv(paste0('../Base_Results/',distj,'_',
                      trendj,'_',
                      modelj,'_',
                      innovationsj,'_paths.rds'))
