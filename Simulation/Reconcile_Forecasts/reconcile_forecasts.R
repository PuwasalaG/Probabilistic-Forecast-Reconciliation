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


#Read Simulation Table
simtable<-read_csv('SimulationTable.csv')

### Extract flags from simulation scenario

#scen<-1 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this


simj<-simtable[scen,] #Extract row of table
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
modelj<-simj$model #Is model ARIMA or ETS
innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
depj<-simj$dep #Are innovations drawn independently or jointly?

#Read in base forecast
data<-read_csv(paste0('../Base_Results/',distj,'_',trendj,'_',modelj,'_base.csv'))

all<-0 #Eventually write code to do reconciled forecasts

#Save output
saveRDS(all,paste0('../Reconciled_Results/',
                   distj,'_',
                   trendj,'_',
                   modelj,'_',
                   depj,'_',
                   innovationsj,'_reco.rds'))

