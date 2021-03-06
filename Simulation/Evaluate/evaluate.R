#Evaluate reconciled forecasts

library(tidyverse)
library(mvtnorm)
library(Matrix)
#Clear workspace
rm(list=ls())

insample<-T #A flag that determines whether to use results based on in-sample predictions

#Read Simulation Table
simtable<-read_csv('../Reconcile_Forecasts_in/SimulationTable.csv')




evalN<-500 #Number of evaluation periods
Q<-10000 #Number of draws to estimate energy score
inW<-250#inner window for training reco weights
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
  return(((-0.5*term1)+term2)/ncol(x))
  
}

#Variogram score
variogram_score<-function(y,x,xs){
  term1<-0
  for (i in 1:(length(y)-1)){
    for (j in (i+1):length(y)){
      term2<-0
      for (q in 1:ncol(x)){
        term2<-term2+abs(x[i,q]-xs[j,q])
      }
      term2<-term2/ncol(x)
      term1<-term1+(abs(y[i]-y[j])-term2)^2
    
    }
  }
  return(term1)
}


#Function to evaluate one scenario
evaluate_scenario<-function(scen){
  
  simj<-simtable[scen,] #Extract row of table
  scorej<-simj$score #Is energy or variogram score optimised
  distj<-simj$dist #Is DGP Gaussian or nonGaussian
  trendj<-simj$trend #Is DGP stationary or nonStationary
  modelj<-simj$model #Is model ARIMA or ETS
  innovationsj<-simj$innovations # Are innovations Gaussian or bootstrapped
  depj<-simj$dep #Are innovations drawn independently or jointly?
  
  #Read in data
  data<-read_csv(paste0('../Data/',distj,'_',trendj,'.csv'))
  
  #Read in base forecast
  fc<-readRDS(paste0('../Base_Results/',distj,'_',trendj,'_',modelj,'_base.rds'))
  #Read in optimal

  
  optreco_energy<-readRDS(paste0('../Reconciled_Results/',
                       'energy','_',
                       distj,'_',
                       trendj,'_',
                       modelj,'_',
                       depj,'_',
                       innovationsj,'_optreco.rds'))
  optreco_energy_in<-readRDS(paste0('../Reconciled_Results_in/',
                          'energy','_',
                          distj,'_',
                          trendj,'_',
                          modelj,'_',
                          depj,'_',
                          innovationsj,'_optreco.rds'))
  optreco_variogram<-readRDS(paste0('../Reconciled_Results/',
                                 'variogram','_',
                                 distj,'_',
                                 trendj,'_',
                                 modelj,'_',
                                 depj,'_',
                                 innovationsj,'_optreco.rds'))
  optreco_variogram_in<-readRDS(paste0('../Reconciled_Results_in/',
                                    'variogram','_',
                                    distj,'_',
                                    trendj,'_',
                                    modelj,'_',
                                    depj,'_',
                                    innovationsj,'_optreco.rds'))
  
  
  Base<-rep(NA,evalN)
  BottomUp<-rep(NA,evalN)
  OLS<-rep(NA,evalN)
  WLS<-rep(NA,evalN)
  JPP<-rep(NA,evalN)
  MinTShr<-rep(NA,evalN)
  MinTSam<-rep(NA,evalN)
  BTTH<-rep(NA,evalN)
  ScoreOptE<-rep(NA,evalN)
  ScoreOptEIn<-rep(NA,evalN)
  ScoreOptV<-rep(NA,evalN)
  ScoreOptVIn<-rep(NA,evalN)
  
  Basev<-rep(NA,evalN)
  BottomUpv<-rep(NA,evalN)
  OLSv<-rep(NA,evalN)
  WLSv<-rep(NA,evalN)
  JPPv<-rep(NA,evalN)
  MinTShrv<-rep(NA,evalN)
  MinTSamv<-rep(NA,evalN)
  BTTHv<-rep(NA,evalN)
  ScoreOptEv<-rep(NA,evalN)
  ScoreOptEInv<-rep(NA,evalN)
  ScoreOptVv<-rep(NA,evalN)
  ScoreOptVInv<-rep(NA,evalN)
  for (i in 1:evalN){
    
    #Get realisation
    
    data%>%
      filter(Time==N+L+inW+i)%>%
      pivot_longer(-Time,names_to = 'var')%>%
      arrange(match(var,c("Tot","A","B","AA","AB","BA","BB")))%>%
      pull(value)->y
    
    #Base forecasts
    
    fc_i<-fc[[inW+i]]
    


    
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
    Basev[i]<-variogram_score(y,x,xs)
    #Bottom up
    BottomUp[i]<-energy_score(y,SG_bu%*%x,SG_bu%*%xs)
    BottomUpv[i]<-variogram_score(y,SG_bu%*%x,SG_bu%*%xs)
    
    #OLS
    OLS[i]<-energy_score(y,SG_ols%*%x,SG_ols%*%xs)
    OLSv[i]<-variogram_score(y,SG_ols%*%x,SG_ols%*%xs)
    
    
    #WLS (structural)
    SW_wls<-solve(diag(rowSums(S)),S)
    SG_wls<-S%*%solve(t(SW_wls)%*%S,t(SW_wls))
    WLS[i]<-energy_score(y,SG_wls%*%x,SG_wls%*%xs)
    WLSv[i]<-variogram_score(y,SG_wls%*%x,SG_wls%*%xs)
    
    #JPP
    JPP[i]<-energy_score(y,SG_wls%*%t(apply(x,1,sort)),SG_wls%*%t(apply(xs,1,sort)))
    JPPv[i]<-variogram_score(y,SG_wls%*%t(apply(x,1,sort)),SG_wls%*%t(apply(xs,1,sort)))
    
    #MinT (shr)
    SW_MinTShr<-solve(fc_i$fc_Sigma_shr,S)
    SG_MinTShr<-S%*%solve(t(SW_MinTShr)%*%S,t(SW_MinTShr))
    MinTShr[i]<-energy_score(y,SG_MinTShr%*%x,SG_MinTShr%*%xs)
    MinTShrv[i]<-variogram_score(y,SG_MinTShr%*%x,SG_MinTShr%*%xs)
    
    #BTTH
    #Find order
    o<-apply(fc_i$resid[4:7,],1,order) #Find order stats
    oi<-apply(o,2,invPerm) #Find inverse permutation
    rb<-t(apply(x[4:7,1:ncol(fc_i$resid)],1,sort)) #Rank sample (504  only)
    rbs<-t(apply(xs[4:7,1:ncol(fc_i$resid)],1,sort)) #Rank copy (504  only)
    #Permute
    cb<-matrix(NA,nrow(rb),ncol(rb))
    cbs<-matrix(NA,nrow(rbs),ncol(rbs))
    for (j in 1:4){
      cb[j,]<-(rb[j,oi[,j]])
      cbs[j,]<-(rbs[j,oi[,j]])
    }
    mean_adj<-SG_MinTShr%*%fc_i$fc_mean-fc_i$fc_mean
    x_btth<-(S%*%cb)+matrix(mean_adj,7,ncol(cb))
    xs_btth<-(S%*%cbs)+matrix(mean_adj,7,ncol(cbs))
    BTTH[i]<-energy_score(y,x_btth,xs_btth)
    BTTHv[i]<-variogram_score(y,x_btth,xs_btth)
      
    #MinT (sam)
    SW_MinTSam<-solve(fc_i$fc_Sigma_sam,S)
    SG_MinTSam<-S%*%solve(t(SW_MinTSam)%*%S,t(SW_MinTSam))
    MinTSam[i]<-energy_score(y,SG_MinTSam%*%x,SG_MinTSam%*%xs)
    MinTSamv[i]<-variogram_score(y,SG_MinTSam%*%x,SG_MinTSam%*%xs)
    
    #ScoreOpt (Energy)
    xopt<-S%*%(optreco_energy[[i]]$d+optreco_energy[[i]]$G%*%x)
    xsopt<-S%*%(optreco_energy[[i]]$d+optreco_energy[[i]]$G%*%xs)
    ScoreOptE[i]<-energy_score(y,xopt,xsopt)
    ScoreOptEv[i]<-variogram_score(y,xopt,xsopt)
    
    #ScoreOptIn (Energy)
    xopt<-S%*%(optreco_energy_in[[i]]$d+optreco_energy_in[[i]]$G%*%x)
    xsopt<-S%*%(optreco_energy_in[[i]]$d+optreco_energy_in[[i]]$G%*%xs)
    ScoreOptEIn[i]<-energy_score(y,xopt,xsopt)
    ScoreOptEInv[i]<-variogram_score(y,xopt,xsopt)
    
    #ScoreOpt (variogram)
    xopt<-S%*%(optreco_variogram[[i]]$d+optreco_variogram[[i]]$G%*%x)
    xsopt<-S%*%(optreco_variogram[[i]]$d+optreco_variogram[[i]]$G%*%xs)
    ScoreOptV[i]<-energy_score(y,xopt,xsopt)
    ScoreOptVv[i]<-variogram_score(y,xopt,xsopt)

    #ScoreOptIn (variogram)
    xopt<-S%*%(optreco_variogram_in[[i]]$d+optreco_variogram_in[[i]]$G%*%x)
    xsopt<-S%*%(optreco_variogram_in[[i]]$d+optreco_variogram_in[[i]]$G%*%xs)
    ScoreOptVIn[i]<-energy_score(y,xopt,xsopt)
    ScoreOptVInv[i]<-variogram_score(y,xopt,xsopt)
    
  }
    
  res<-tibble(EvaluationPeriod=1:evalN,Base,BottomUp,JPP,BTTH,OLS,WLS,MinTSam,MinTShr,ScoreOptE,ScoreOptEIn,ScoreOptV,ScoreOptVIn)  
  res_long_energy<-pivot_longer(res,-EvaluationPeriod,names_to = 'Method',values_to = 'Score')%>%
    add_column(ScoreEval='Energy')
  resv<-tibble(EvaluationPeriod=1:evalN,Basev,BottomUpv,JPPv,BTTHv,OLSv,WLSv,MinTSamv,MinTShrv,ScoreOptEv,ScoreOptEInv,ScoreOptVv,ScoreOptVInv)  
  res_long_variogram<-pivot_longer(resv,-EvaluationPeriod,names_to = 'Method',values_to = 'Score')%>%
    add_column(ScoreEval='Variogram')%>%
    mutate(Method=gsub('.{1}$','',Method))
  
  rbind(res_long_energy,
        res_long_variogram)%>%
    add_column(DGPDistribution=distj,
                        DGPStationary=trendj,
                        BaseModel=modelj,
                        BaseDependence=depj,
                        BaseDistribution=innovationsj)->res_final
  return(res_final)
}


all_results<-map_dfr(1:32,evaluate_scenario)

saveRDS(all_results,'all_results.rds')
write_csv(all_results,'all_results.csv')



# all_results%>%
#   mutate(BaseMethod=paste(BaseDependence,BaseDistribution))%>%
#   ggplot(aes(x=Method, y=EnergyScore))+
#   geom_boxplot()+
#   facet_grid(rows = vars(BaseMethod),col= vars(BaseModel))

all_results%>%
  filter(ScoreEval=='Energy')%>%
  group_by(Method,BaseDependence,BaseDistribution,BaseModel,DGPDistribution,DGPStationary)%>%
  summarise(meanScore=mean(Score),medianScore=median(Score))%>%
  pivot_wider(id_cols = c('DGPStationary',
                          'DGPDistribution',
                          'BaseModel','BaseDependence',
                          'BaseDistribution'),
              names_from = Method,values_from = meanScore)%>%
  arrange(desc(DGPStationary),
          DGPDistribution,
          BaseModel,
          BaseDependence,
          BaseDistribution)->summary_mean
#  mutate_if(is.numeric, list(dif = ~ (. - Base)/Base))%>%
#  select(c(1:6,17:23))->

write_csv(summary_mean,'meanScore.csv')




all_results%>%
  filter(ScoreEval=='Energy')%>%
  group_by(Method,BaseDependence,BaseDistribution,BaseModel,DGPDistribution,DGPStationary)%>%
  summarise(meanScore=mean(Score),medianScore=median(Score))%>%
  pivot_wider(id_cols = c('DGPStationary',
                          'DGPDistribution',
                          'BaseModel','BaseDependence',
                          'BaseDistribution'),
              names_from = Method,values_from = medianScore)%>%
  arrange(desc(DGPStationary),
          DGPDistribution,
          BaseModel,
          BaseDependence,
          BaseDistribution)->summary_median

write_csv(summary_median,'medianScore.csv')
