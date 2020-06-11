#This script learns reconciliation weights using 
#score optimisation (with in-sample predictions)

library(tidyverse)
library(ProbReco)

#Clear workspace
rm(list=ls())


#Read Simulation Table
simtable<-read_csv('SimulationTable.csv')

### Extract flags from simulation scenario

scen<-31 #If running within R uncomment this.  This will only run first scenario
#scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
scorej<-simj$score #Is variogram or energy score used
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
modelj<-simj$model #Is model ARIMA or ETS
innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
depj<-simj$dep #Are innovations drawn independently or jointly?

#Sample sizes
N<-500 # Size of base training window
L<-4 # Lags to leave at beginning of window
inW<-250 # Size of inner windows (not used here but kept to make consistent with other code)
outW<-500 # Size of outer window (for evaluation)
Q<-100 #Number of iterates to approximate energy score
m<-7 #Number of series

#Read in data
data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))

#Read in base forecast
fc<-readRDS(paste0('../Base_Results/',distj,'_',trendj,'_',modelj,'_base.rds'))



#Set up S matrix
S<-matrix(c(1,1,1,1,
            1,1,0,0,
            0,0,1,1,
            1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1),7,4,byrow = T)

#Initialise list for storing all output
all<-as.list(rep(NA,outW))


#Loop over evaluation periods
for (eval in 1:outW){
  print('eval')
  print(eval)
  #Select data
  data%>%
    filter((Time>=(eval+inW))&(Time<(N+L+eval+inW)))%>%
    select(-Time)%>%
    as.matrix()%>%
    t->y
  
  
  #Reconstruct forecasts (from residuals)
  fc_i<-fc[[(eval+inW-1)]]
  yhat<-y-fc_i$resid
  

  #Train reconciliation weights using SGA 
  
  tt<-system.time(
    try(opt<-inscoreopt(y,
                        yhat,
                        S,
                        control = list(maxIter=500,tol=1E-12),
                        basedep = depj,
                        basedist = innovationsj,
                        Q=Q,
                        score=list(score=scorej,alpha=1),
                        trace = T))->err)
  if(class(err)=='try-error'){
    opt<-list(d=rep(0,4),
              G=solve(t(S)%*%S,t(S)),
              val=0,
              G_vec_store=solve(t(S)%*%S,t(S)),
              val_store=0)
  }
  
  print(tt)

  
  
  #Store
  all[[eval]]<-opt

}


#Save output
saveRDS(all,paste0('../Reconciled_Results_in/',
                   scorej,'_',
                   distj,'_',
                   trendj,'_',
                   modelj,'_',
                   depj,'_',
                   innovationsj,'_optreco.rds'))

