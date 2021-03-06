#This script trains reconciliation weights for each simulation scenario

library(mvtnorm)
library(tidyverse)
library(ProbReco)

#Clear workspace
rm(list=ls())


#Read Simulation Table
simtable<-read_csv('SimulationTable.csv')

### Extract flags from simulation scenario

#scen<- 36 #If running within R uncomment this.  This will only run first scenario
scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
scorej<-simj$score #Is DGP Gaussian or nonGaussian
distj<-simj$dist #Is DGP Gaussian or nonGaussian
trendj<-simj$trend #Is DGP stationary or nonStationary
modelj<-simj$model #Is model ARIMA or ETS
innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
depj<-simj$dep #Are innovations drawn independently or jointly?

#Sample sizes
N<-500 # Size of base training window
L<-4 # Lags to leave at beginning of window
inW<-250 # Size of inner windows (for training reco weights)
outW<-500 # Size of outer window (for evaluation)
Q<-200 #Number of iterates to approximate energy score
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
  #Select correct realised values
  obs_i<-function(i){
    data%>%
      filter(Time==i)%>%
      pivot_longer(-Time,names_to = 'var')%>%
      arrange(match(var,c("Tot","A","B","AA","AB","BA","BB")))%>%
      pull(value)
  }
  all_y<-map((N+L+eval):(N+L+eval+inW-1),obs_i)
  
  #Set up list of Probabilistic Forecast (4 Alternatives)
  
  
  if ((innovationsj=='gaussian')&&(depj=='independent')){
    #Independent Gaussian
    make_genfunc<-function(input){
      f<-function(){
        fc_mean<-input$fc_mean
        fc_sd<-input$fc_sd
        out<-matrix(rnorm((Q*m),mean=fc_mean,sd=fc_sd),m,Q)
        return(out)
      }
      return(f)
    }
  }else if((innovationsj=='gaussian')&&(depj=='joint')){
    #Joint Gaussian (using sample estimate of covariance)
    make_genfunc<-function(input){
      f<-function(){
        fc_mean<-input$fc_mean
        fc_sigma<-input$fc_Sigma_sam
        out<-t(rmvnorm(Q,fc_mean,fc_sigma))
        return(out)
      }
      return(f)
    }
  }else if((innovationsj=='bootstrap')&&(depj=='independent')){
    #Bootstrapping residuals ignoring dependence
    make_genfunc<-function(input){
      f<-function(){
        fc_mean<-input$fc_mean
        fc_r<-input$resid
        out<-matrix(0,m,Q)
        for (j in 1:m){
          ind<-sample(1:ncol(fc_r),Q,replace=T)
          out[j,]<-fc_r[j,ind]+fc_mean[j]
        }
        
        return(out)
      }
      return(f)
    }
  }else if((innovationsj=='bootstrap')&&(depj=='joint')){
    #Bootstrapping residuals jointly
    make_genfunc<-function(input){
      f<-function(){
        fc_mean<-input$fc_mean
        fc_r<-input$resid
        ind<-sample(1:ncol(fc_r),Q,replace=T)
        out<-fc_r[,ind]+fc_mean
        return(out)
      }
      return(f)
    }
  }
  
  
  all_prob<-map(fc[eval:(eval+inW-1)],make_genfunc)
  
  #Set match TRUE if bootstrapping
  
  match<-(innovationsj=='bootstrap')
  
  #Train reconciliation weights using SGA 
  
  tt<-system.time(
    try(opt<-scoreopt(all_y,
                      all_prob,
                      S,
                      score = list(score=scorej,alpha=1),
                      trace = T,
                      control = list(maxIter=600, tol=1E-12),
                      match=match))->err)
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
saveRDS(all,paste0('../Reconciled_Results/',
                   scorej,'_',
                   distj,'_',
                   trendj,'_',
                   modelj,'_',
                   depj,'_',
                   innovationsj,'_optreco.rds'))

