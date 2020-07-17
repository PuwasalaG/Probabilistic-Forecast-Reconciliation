#Script to report results

rm(list=ls())

library(tidyverse)
library(tsutils)
readj<-function(j){
  a<-readRDS(paste0('../Evaluation/Results/results_',j,'.rds'))
  return(a)
}

all<-map_dfr(c(196:365),readj)

all%>%
  filter(ScoreEval=='Energy')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)->MeanScoreEnergy

write_csv(MeanScoreEnergy,'MeanScoreEnergy.csv')

all%>%
  filter(ScoreEval=='Variogram')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)->MeanScoreVariogram

write_csv(MeanScoreVariogram,'MeanScoreVariogram.csv')

for (Dep in c('independent', 'joint')){
  for (Dist in c('gaussian', 'bootstrap')){
    for (Sco in c('Energy','Variogram')){
      all%>%filter(BaseDep==Dist,
                   BaseDist==Dep,
                   ScoreEval==Sco
                   #Method!='WLS',
                   #Method!='MinTSam',
                   #Method!='ScoreOptEIn',
                   #Method!='ScoreOptV',
                   #Method!='ScoreOptVIn'
                   )%>%
        select(EvalDate,Method,Score)%>%
        pivot_wider(id_cols = EvalDate,
                    names_from = Method,
                    values_from = Score)%>%
        select(-EvalDate)%>%
        as.matrix()->nn
      pdf(paste('nemenyi_',Dist,'_',Dep,'_',Sco,'.pdf'))
      nemenyi(nn,plottype = 'matrix')
      dev.off()
    }
  }
}


