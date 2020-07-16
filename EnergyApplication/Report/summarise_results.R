#Script to report results

rm(list=ls())

library(tidyverse)

readj<-function(j){
  a<-readRDS(paste0('../Evaluation/Results/results_',j,'.rds'))
  return(a)
}

all<-map_dfr(c(196,198,199,200,201,217),readj)

all%>%
  filter(ScoreEval=='Energy')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)->MeanScoreEnergy

View(MeanScoreEnergy)

all%>%
  filter(ScoreEval=='Variogram')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)->MeanScoreVariogram

View(MeanScoreVariogram)