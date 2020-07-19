#Script to report results

rm(list=ls())

library(tidyverse)
library(tsutils)
library(fable)
library(kableExtra)
library(corrplot)

#Base Results

#Pull data for 21st January
b30<-readRDS('../Base_Results/base_30.rds')

dates<-seq.Date(from=as.Date('2019/10/2'),to=as.Date('2020/01/21'),by=1)
as_tibble(b30$resid)%>%
  add_column(Date=dates)->resid_df

pdf('forPaper/densities.pdf')
resid_df%>%
  select(Date,Total,Coal,Gas,Wind,Distillate,`Solar (Rooftop)`, `Solar (Utility)`,Pumps,Biomass)%>%
  pivot_longer(cols = -Date,names_to = 'Source', values_to = 'Generation')%>%
  ggplot(aes(x=Generation))+
  geom_histogram()+
  facet_wrap(~Source,nrow = 3,3,scales = 'free')
dev.off()

pdf('forPaper/corr.pdf')
b30$fc_Sigma_shr%>%cov2cor%>%corrplot  
dev.off()

#Reconciliation results

all<-read_csv('all.csv')

all%>%
  filter(ScoreEval=='Energy',
         Method!='ScoreOptEIn',
         Method!='ScoreOptV',
         Method!='ScoreOptVIn',
         Method!='WLS',
         Method!='MinTSam')%>%
  mutate(BaseMethod=paste(BaseDep,BaseDist,sep='_'),
         RecoMethod=Method)%>%
  select(-BaseDep,-BaseDist)%>%
  group_by(BaseMethod,RecoMethod)%>%
  summarise(meanScore=mean(Score))%>%
  pivot_wider(id_cols = RecoMethod,
              names_from=BaseMethod,values_from=meanScore)%>%
    select(RecoMethod,
           `Ind. Bootstrap`=bootstrap_independent,
           `Ind. Gaussian`=gaussian_independent,
           `Joint Bootstrap`=bootstrap_joint,
           `Joint Gaussian`=gaussian_joint)->MeanScoreEnergy

MeanScoreEnergy%>%
  kable(digits=2, format='latex')->me
  
capture.output(print(me),file = 'forPaper/meanenergyscore.tex')

all%>%
  filter(BaseDep=='gaussian',
             BaseDist=='independent',
             ScoreEval=='Energy',
             Method!='MinTSam',
             Method!='WLS',
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
nemenyi(nn,plottype = 'matrix',main='')
dev.off()

all%>%filter(BaseDep=='bootstrap',
             BaseDist=='joint',
             ScoreEval=='Energy',
             Method!='WLS',
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
nemenyi(nn,plottype = 'matrix',main='')
dev.off()