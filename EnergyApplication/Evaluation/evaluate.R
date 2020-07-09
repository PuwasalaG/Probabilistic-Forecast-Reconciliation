#Evaluate reconciled forecasts for the j^th window

library(tidyverse)
library(mvtnorm)
library(Matrix)
library(ProbReco)
#Clear workspace
rm(list=ls())


i<- 196 #If running within R uncomment this.  This will only run one window
#i<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this should start from L+N+inW 

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
Q<-10000 #Number of draws to estimate energy score
inW<-56 #inner window for training reco weights
m<-23 #Number of series


startdate<-min(data$date)
enddate<-max(data$date)
alldate<-seq.Date(startdate,enddate,by=1)

J<-length(alldate)


#Predefine some reconciliation matrices

SG_bu<-S%*%cbind(matrix(0,15,8),diag(rep(1,15)))
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


#Read in base forecast
fc_i<-readRDS(paste0('../Base_Results/base_',i,'.rds'))


#Get realisation
data%>%
  filter(date==alldate[i+1])%>%
  arrange(match(Source,order))%>%
  pull(Generation)->y


#Get y and yhat (for inscoreopt)
data%>%
  filter((date>=alldate[i-N+1])&(date<=alldate[i]))%>%
  arrange(match(Source,order))%>%
  pivot_wider(id_cols = date,names_from=Source, values_from = Generation)%>%
  select(-date)%>%
  as.matrix%>%
  t->yin

yhat<-yin-fc_i$resid      

#Get out of sample trained reconciliation weights

alloptE<-readRDS(paste0('../Reconciliation/Reconciliation_Results/scoreopt_energy_',i,'.rds'))
alloptV<-readRDS(paste0('../Reconciliation/Reconciliation_Results/scoreopt_variogram_',i,'.rds'))

eval<-function(bb){
  if (bb==1){
    #Gaussian independent
    innovationsj='gaussian'
    depj='independent'
    fc_mean<-fc_i$fc_mean
    fc_sd<-fc_i$fc_sd
    x<-matrix(rnorm((Q*m),mean=fc_mean,sd=fc_sd),m,Q)
    xs<-matrix(rnorm((Q*m),mean=fc_mean,sd=fc_sd),m,Q)
  }else if(bb==2){
    #Gaussian dependent
    innovationsj='gaussian'
    depj='joint'
    fc_mean<-fc_i$fc_mean
    fc_sigma<-fc_i$fc_Sigma_sam
    x<-t(rmvnorm(Q,fc_mean,fc_sigma))
    xs<-t(rmvnorm(Q,fc_mean,fc_sigma))
  }else if(bb==3){
    #Bootstrap independent
    innovationsj='bootstrap'
    depj='independent'
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
  }else if(bb==4){
    #Joint bootstrapping
    innovationsj='bootstrap'
    depj='joint'
    fc_mean<-fc_i$fc_mean
    fc_r<-fc_i$resid
    ind<-sample(1:ncol(fc_r),Q,replace=T)
    x<-fc_r[,ind]+fc_mean
    ind<-sample(1:ncol(fc_r),Q,replace=T)
    xs<-fc_r[,ind]+fc_mean
  }    
    
  #Base forecast
  Base<-energy_score(y,x,xs)
  Basev<-variogram_score(y,x,xs)

  #Bottom up
  BottomUp<-energy_score(y,SG_bu%*%x,SG_bu%*%xs)
  BottomUpv<-variogram_score(y,SG_bu%*%x,SG_bu%*%xs)
  
  #OLS
  OLS<-energy_score(y,SG_ols%*%x,SG_ols%*%xs)
  OLSv<-variogram_score(y,SG_ols%*%x,SG_ols%*%xs)

  #WLS (structural)
  SW_wls<-solve(diag(rowSums(S)),S)
  SG_wls<-S%*%solve(t(SW_wls)%*%S,t(SW_wls))
  WLS<-energy_score(y,SG_wls%*%x,SG_wls%*%xs)
  WLSv<-variogram_score(y,SG_wls%*%x,SG_wls%*%xs)
    
  #JPP
  JPP<-energy_score(y,SG_wls%*%t(apply(x,1,sort)),SG_wls%*%t(apply(xs,1,sort)))
  JPPv<-variogram_score(y,SG_wls%*%t(apply(x,1,sort)),SG_wls%*%t(apply(xs,1,sort)))
    
  #MinT (shr)
  SW_MinTShr<-solve(fc_i$fc_Sigma_shr,S)
  SG_MinTShr<-S%*%solve(t(SW_MinTShr)%*%S,t(SW_MinTShr))
  MinTShr<-energy_score(y,SG_MinTShr%*%x,SG_MinTShr%*%xs)
  MinTShrv<-variogram_score(y,SG_MinTShr%*%x,SG_MinTShr%*%xs)
    
  #BTTH
  #Find order
  o<-apply(fc_i$resid[9:23,],1,order) #Find order stats
  oi<-apply(o,2,invPerm) #Find inverse permutation
  rb<-t(apply(x[9:23,1:ncol(fc_i$resid)],1,sort)) #Rank sample (504  only)
  rbs<-t(apply(xs[9:23,1:ncol(fc_i$resid)],1,sort)) #Rank copy (504  only)
  #Permute
  cb<-matrix(NA,nrow(rb),ncol(rb))
  cbs<-matrix(NA,nrow(rbs),ncol(rbs))
  for (j in 1:15){
    cb[j,]<-(rb[j,oi[,j]])
    cbs[j,]<-(rbs[j,oi[,j]])
  }
  mean_adj<-SG_MinTShr%*%fc_i$fc_mean-fc_i$fc_mean
  x_btth<-(S%*%cb)+matrix(mean_adj,23,ncol(cb))
  xs_btth<-(S%*%cbs)+matrix(mean_adj,23,ncol(cbs))
  BTTH<-energy_score(y,x_btth,xs_btth)
  BTTHv<-variogram_score(y,x_btth,xs_btth)
      
  #MinT (sam)
  SW_MinTSam<-solve(fc_i$fc_Sigma_sam,S)
  SG_MinTSam<-S%*%solve(t(SW_MinTSam)%*%S,t(SW_MinTSam))
  MinTSam<-energy_score(y,SG_MinTSam%*%x,SG_MinTSam%*%xs)
  MinTSamv<-variogram_score(y,SG_MinTSam%*%x,SG_MinTSam%*%xs)
  
  #Score opt in
  
  #Train reconciliation weights using SGA 
  
  tt1<-system.time(
    try(optE<-inscoreopt(yin,
                        yhat,
                        S,
                        control = list(maxIter=50,tol=1E-12),
                        basedep = depj,
                        basedist = innovationsj,
                        Q=200,
                        score=list(score='energy',alpha=1),
                        trace = T))->err)
  if(class(err)=='try-error'){
    opt<-list(d=rep(0,4),
              G=solve(t(S)%*%S,t(S)),
              val=0,
              G_vec_store=solve(t(S)%*%S,t(S)),
              val_store=0)
  }
  
  print(tt1)
  print(class(err))
  tt2<-system.time(
    try(optV<-inscoreopt(yin,
                        yhat,
                        S,
                        control = list(maxIter=50,tol=1E-12),
                        basedep = depj,
                        basedist = innovationsj,
                        Q=200,
                        score=list(score='variogram',alpha=1),
                        trace = T))->err)
  if(class(err)=='try-error'){
    opt<-list(d=rep(0,4),
              G=solve(t(S)%*%S,t(S)),
              val=0,
              G_vec_store=solve(t(S)%*%S,t(S)),
              val_store=0)
  }
  print(tt2)
  print(class(err))
  
  #ScoreOptIn (Energy)
  xopt<-S%*%(optE$d+optE$G%*%x)
  xsopt<-S%*%(optE$d+optE$G%*%xs)
  ScoreOptEIn<-energy_score(y,xopt,xsopt)
  ScoreOptEInv<-variogram_score(y,xopt,xsopt)
  
  #ScoreOptIn (Variogram)
  xopt<-S%*%(optV$d+optV$G%*%x)
  xsopt<-S%*%(optV$d+optV$G%*%xs)
  ScoreOptVIn<-energy_score(y,xopt,xsopt)
  ScoreOptVInv<-variogram_score(y,xopt,xsopt)
  
  #ScoreOpt (Energy)
  optE<-alloptE[[bb]]
  xopt<-S%*%(optE$d+optE$G%*%x)
  xsopt<-S%*%(optE$d+optE$G%*%xs)
  ScoreOptE<-energy_score(y,xopt,xsopt)
  ScoreOptEv<-variogram_score(y,xopt,xsopt)
  
  #ScoreOptIn (Variogram)
  optV<-alloptV[[bb]]
  xopt<-S%*%(optV$d+optV$G%*%x)
  xsopt<-S%*%(optV$d+optV$G%*%xs)
  ScoreOptV<-energy_score(y,xopt,xsopt)
  ScoreOptVv<-variogram_score(y,xopt,xsopt)
    
res<-tibble(Base,BottomUp,JPP,BTTH,OLS,WLS,MinTSam,MinTShr,ScoreOptE,ScoreOptEIn,ScoreOptV,ScoreOptVIn)%>%
  add_column(ScoreEval='Energy')
res_long_energy<-pivot_longer(res,cols = -ScoreEval,names_to = 'Method',values_to = 'Score')
resv<-tibble(Basev,BottomUpv,JPPv,BTTHv,OLSv,WLSv,MinTSamv,MinTShrv,ScoreOptEv,ScoreOptEInv,ScoreOptVv,ScoreOptVInv)%>%
  add_column(ScoreEval='Variogram')
res_long_variogram<-pivot_longer(resv,-ScoreEval,names_to = 'Method',values_to = 'Score')%>%
    mutate(Method=gsub('.{1}$','',Method))
  
rbind(res_long_energy,
      res_long_variogram)%>%
  add_column(BaseDep=innovationsj,
             BaseDist=depj)->res_final
  return(res_final)
}

all_i<-map_dfr(1:4,eval)%>%add_column(EvalDate=alldate[i+1])

saveRDS(all_i,paste0('Results/results_',i,'.rds'))
