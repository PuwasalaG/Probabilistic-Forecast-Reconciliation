#This script does score optimal reconciliation for each window. 
#The forecast mean, forecast standard deviation, two estimates of the covariance matrix and residuals are retained.

#Evaluate reconciled forecasts for the j^th window

library(tidyverse)
library(mvtnorm)
library(Matrix)
library(ProbReco)
#Clear workspace
rm(list=ls())


#i<- 196 #If running within R uncomment this.  This will only run one window
i<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this should start from L+N+inW 

# Order of variables

order<-c("Total","non-Renewable","Renewable",
         "Coal","Gas","Solar",
         'Hydro (inc. Pumps)',"Battery",
         "Black Coal","Brown Coal",
         "Gas (CCGT)", "Gas (OCGT)", "Gas (Reciprocating)", "Gas (Steam)",
         "Solar (Rooftop)","Solar (Utility)",
         "Hydro", "Pumps",
         "Battery (Charging)", "Battery (Discharging)",
         "Distillate","Biomass","Wind")


#Define S Matrix
S<-matrix(c(rep(1,15),#All
            c(rep(1,6),rep(0,6),1,rep(0,2)), #non-Renewables
            c(rep(0,6),rep(1,6),0,rep(1,2)), # Renewables
            c(rep(1,2),rep(0,13)), # Coal
            c(rep(0,2),rep(1,4),rep(0,9)), #Gas
            c(rep(0,6),rep(1,2),rep(0,7)), #Solar
            c(rep(0,8),rep(1,2),rep(0,5)), # Hydro
            c(rep(0,10),rep(1,2),rep(0,3)) # Battery
),8,15,byrow = T)

S<-rbind(S,diag(1,15))



#Read in data
data<-readRDS('../Data/nem_generation_by_source.rds')




arrange(data,match(Source,order))->datlong


#Sample sizes
N<-112 # Size of window
L<-28 # Lags to leave at beginning of window
Q<-200#Number of draws to estimate energy score
inW<-56 #inner window for training reco weights
m<-23 #Number of series


startdate<-min(data$date)
enddate<-max(data$date)
alldate<-seq.Date(startdate,enddate,by=1)

J<-length(alldate)

  #Select correct realised values
  obs_j<-function(j){
    data%>%
      filter(date==alldate[j])%>%
      arrange(match(Source,order))%>%
      pull(Generation)->y
  }
  all_y<-map((i-inW+1):i,obs_j)
  
  #Set up list of Probabilistic Forecast (4 Alternatives)
  
  #Construct list of fc lists
  
  fc_j<-function(j){
    fc_j<-readRDS(paste0('../Base_Results/base_',j,'.rds'))
    return(fc_j)
  }
  fc<-map((i-inW):(i-1),fc_j)

  #Initialise  all
  allE<-as.list(rep(NA,4))
  allV<-as.list(rep(NA,4))
  #Loop for each base method
for (bb in 1:4){  
  if (bb==1){
    #Gaussian independent
    innovationsj='gaussian'
    depj='independent'
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
  }else if(bb==2){
    #Gaussian dependent
    innovationsj='gaussian'
    depj='joint'
    make_genfunc<-function(input){
      f<-function(){
        fc_mean<-input$fc_mean
        fc_sigma<-input$fc_Sigma_sam
        out<-t(rmvnorm(Q,fc_mean,fc_sigma))
        return(out)
      }
      return(f)
    }
  }else if(bb==3){
    #Bootstrap independent
    innovationsj='bootstrap'
    depj='independent'
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
  }else if(bb==4){
    #Joint bootstrapping
    innovationsj='bootstrap'
    depj='joint'
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
  
  
  all_prob<-map(fc,make_genfunc)
  
  #Set match TRUE if bootstrapping
  
  match<-(innovationsj=='bootstrap')
  
  #Train reconciliation weights using SGA 
  tracei<-(round(i/10)==(i/10))
  tt1<-system.time(
    try(optE<-scoreopt(all_y,
                      all_prob,
                      S,
                      score = list(score='energy',alpha=1),
                      trace = tracei,
                      control = list(maxIter=5000, tol=1E-12),
                      match=match))->err)
  if(class(err)=='try-error'){
    optE<-list(d=rep(0,4),
              G=solve(t(S)%*%S,t(S)),
              val=0,
              G_vec_store=solve(t(S)%*%S,t(S)),
              val_store=0)
  }
  
  tt2<-system.time(
    try(optV<-scoreopt(all_y,
                       all_prob,
                       S,
                       score = list(score='variogram',alpha=1),
                       trace = tracei,
                       control = list(maxIter=5000, tol=1E-12),
                       match=match))->err)
  if(class(err)=='try-error'){
    optV<-list(d=rep(0,4),
               G=solve(t(S)%*%S,t(S)),
               val=0,
               G_vec_store=solve(t(S)%*%S,t(S)),
               val_store=0)
  }
  
  print(tt1)
  print(tt2)
  print(class(err))
  allE[[bb]]<-optE
  allV[[bb]]<-optV
}  
  




#Save output
saveRDS(allE,paste0('Reconciliation_Results/scoreopt_energy_',i,'.rds'))
saveRDS(allV,paste0('Reconciliation_Results/scoreopt_variogram_',i,'.rds'))

