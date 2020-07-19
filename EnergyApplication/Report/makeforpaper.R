#Script to report results

rm(list=ls())

library(tidyverse)
library(tsutils)

all<-read_csv('all.csv')

all%>%
  filter(ScoreEval=='Energy',
         Method!='ScoreOptEIn',
         Method!='ScoreOptV',
         Method!='ScoreOptVIn',
         Method!='MinTSam')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)->MeanScoreEnergy

MeanScoreEnergy%>%
  kable(digits=2, format='latex')->me
  
capture.output(print(me),file = 'forPaper/meanenergyscore.tex')

all%>%
  filter(BaseDep=='gaussian',
             BaseDist=='independent',
             ScoreEval=='Energy',
             Method!='MinTSam',
             Method!='ScoreOptEIn',
             Method!='ScoreOptV',
             Method!='ScoreOptVIn')%>%
  select(EvalDate,Method,Score)%>%
  pivot_wider(id_cols = EvalDate,
              names_from = Method,
              values_from = Score)%>%
  select(-EvalDate)%>%
  as.matrix()->nn
pdf(paste('forPaper/nemenyi_ig.pdf'))
nemenyi(nn,plottype = 'matrix')
dev.off()

all%>%filter(BaseDep=='bootstrap',
             BaseDist=='joint',
             ScoreEval=='Energy',
             Method!='MinTSam',
             Method!='ScoreOptEIn',
             Method!='ScoreOptV',
             Method!='ScoreOptVIn')%>%
  select(EvalDate,Method,Score)%>%
  pivot_wider(id_cols = EvalDate,
              names_from = Method,
              values_from = Score)%>%
  select(-EvalDate)%>%
  as.matrix()->nn
pdf(paste('forPaper/nemenyi_jb.pdf'))
nemenyi(nn,plottype = 'matrix')
dev.off()