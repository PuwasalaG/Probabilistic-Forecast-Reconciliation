#Evaluate reconciled forecasts

library(tidyverse)

#Clear workspace
rm(list=ls())


#Read Simulation Table
simtable<-read_csv('../Reconcile_Forecasts/SimulationTable.csv')




evalN<-500 #Number of evaluation periods
Q<-2000 #Number of draws to estimate energy score
inW<-500#inner window for training reco weights
L<-4 #Lags to leave at beginning
N<-500 #Training sample size for 
m<-7 #Number of series

#Set up S matrix
S<-matrix(c(1,1,1,1,
            1,1,0,0,
            0,0,1,1,
            1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1),7,4,byrow = T)

#Predefine some reconciliation matrices

SG_bu<-S%*%cbind(matrix(0,4,3),diag(rep(1,4)))
SG_ols<-S%*%solve(t(S)%*%S,t(S))

#Energy score
energy_score<-function(y,x,xs){
  dif1<-x-xs
  dif2<-matrix(y,nrow(x),ncol(x))-x
  
  term1<-apply(dif1,2,function(v){sqrt(sum(v^2))})%>%sum
  term2<-apply(dif2,2,function(v){sqrt(sum(v^2))})%>%sum
  
  return(((0.5*term1)-term2)/ncol(x))
  
}



#Function to evaluate one scenario
evaluate_scenario<-function(scen){
  simj<-simtable[scen,] #Extract row of table
  distj<-simj$dist #Is DGP Gaussian or nonGaussian
  trendj<-simj$trend #Is DGP stationary or nonStationary
  modelj<-simj$model #Is model ARIMA or ETS
  innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
  depj<-simj$dep #Are innovations drawn independently or jointly?
  
  #Read in data
  data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))
  
  #Read in base forecast
  fc<-readRDS(paste0('../Base_Results/',distj,'_',trendj,'_',modelj,'_base.rds'))
  fc_nomable<-map(fc,function(x){x[-1]}) #Delete mable
  rm(fc)
  #Read in optimal
  optreco<-readRDS(paste0('../Reconciled_Results/',
                       distj,'_',
                       trendj,'_',
                       modelj,'_',
                       depj,'_',
                       innovationsj,'_optreco.rds'))
  
  Base<-rep(NA,evalN)
  BottomUp<-rep(NA,evalN)
  OLS<-rep(NA,evalN)
  MinTShr<-rep(NA,evalN)
  MinTSam<-rep(NA,evalN)
  ScoreOpt<-rep(NA,evalN)
  
  for (i in 1:evalN){
    
    #Get realisation
    
    data%>%
      filter(Time==N+L+inW+i)%>%
      pivot_longer(-Time,names_to = 'var')%>%
      arrange(match(var,c("Tot","A","B","AA","AB","BA","BB")))%>%
      pull(value)->y
    
    #Base forecasts
    
    fc_i<-fc_nomable[[inW+i]]
    


    
    if ((innovationsj=='gaussian')&&(depj=='independent')){
      #Gaussian independent
      fc_mean<-fc_i$fc_mean
      fc_sd<-fc_i$fc_sd
      x<-matrix(rnorm((Q*m),mean=fc_mean,sd=fc_sd),m,Q)
      xs<-matrix(rnorm((Q*m),mean=fc_mean,sd=fc_sd),m,Q)
    }else if((innovationsj=='gaussian')&&(depj=='joint')){
      #Gaussian dependent
      fc_mean<-fc_i$fc_mean
      fc_sigma<-fc_i$fc_Sigma_sam
      x<-t(rmvnorm(Q,fc_mean,fc_sigma))
      xs<-t(rmvnorm(Q,fc_mean,fc_sigma))
    }else if((innovationsj=='bootstrap')&&(depj=='independent')){
      #Bootstrap independent
      fc_mean<-fc_i$fc_mean
      fc_r<-fc_i$resid
      x<-matrix(0,m,Q)
      xs<-matrix(0,m,Q)
      for (j in 1:m){
        ind<-sample(1:ncol(fc_r),Q,replace=T)
        x[j,]<-fc_r[j,ind]+fc_mean[j]
        ind<-sample(1:ncol(fc_r),Q,replace=T)
        xs[j,]<-fc_r[j,ind]+fc_mean[j]
      }
    }else if((innovationsj=='bootstrap')&&(depj=='joint')){
      #Joint bootstrapping
      fc_mean<-fc_i$fc_mean
      fc_r<-fc_i$resid
      ind<-sample(1:ncol(fc_r),Q,replace=T)
      x<-fc_r[,ind]+fc_mean
      ind<-sample(1:ncol(fc_r),Q,replace=T)
      xs<-fc_r[,ind]+fc_mean
    }    
    
    #Base forecast
    Base[i]<-energy_score(y,x,xs)
    #Bottom up
    BottomUp[i]<-energy_score(y,SG_bu%*%x,SG_bu%*%xs)
    #OLS
    OLS[i]<-energy_score(y,SG_ols%*%x,SG_ols%*%xs)
    #MinT (shr)
    SW_MinTShr<-fc_i$fc_Sigma_shr%*%S
    SG_MinTShr<-S%*%solve(t(SW_MinTShr)%*%S,t(SW_MinTShr))
    MinTShr[i]<-energy_score(y,SG_MinTShr%*%x,SG_MinTShr%*%xs)
    
    #MinT (sam)
    SW_MinTSam<-fc_i$fc_Sigma_sam%*%S
    SG_MinTSam<-S%*%solve(t(SW_MinTSam)%*%S,t(SW_MinTSam))
    MinTSam[i]<-energy_score(y,SG_MinTSam%*%x,SG_MinTSam%*%xs)
    
    #ScoreOpt
    xopt<-S%*%(optreco[[i]]$a+optreco[[i]]$G%*%x)
    xsopt<-S%*%(optreco[[i]]$a+optreco[[i]]$G%*%xs)
    ScoreOpt[i]<-energy_score(y,xopt,xsopt)
    
  }
    
  res<-tibble(EvaluationPeriod=1:evalN,Base,BottomUp,OLS,MinTSam,MinTShr,ScoreOpt)  
  res_long<-pivot_longer(res,-EvaluationPeriod,names_to = 'Method',values_to = 'EnergyScore')
  res_long%>%add_column(DGPDistribution=distj,
                        DGPStationary=trendj,
                        BaseModel=modelj,
                        BaseDependence=depj,
                        BaseDistribution=innovationsj)->res_final
  return(res_final)
}

all_results<-map_dfr(17:19,evaluate_scenario)
