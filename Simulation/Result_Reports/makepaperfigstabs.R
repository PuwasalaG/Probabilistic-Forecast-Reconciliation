# Script to make tables used in paper

library(tidyverse)
library(tsutils)
library(kableExtra)
res_all<-read_csv('../Evaluate/all_results.csv')

.simpleCap <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

res_all%>%
  filter(ScoreEval=='Energy')%>%
  mutate(DGP=paste(DGPDistribution,DGPStationary,sep = '_'))%>%
  mutate(BaseMethod=paste(.simpleCap(BaseDependence),.simpleCap(BaseDistribution),sep = ' '))%>%
  select(EvaluationPeriod,Method,Score,DGP,BaseMethod,BaseModel)->res_energy

res_all%>%
  filter(ScoreEval=='Variogram')%>%
  mutate(DGP=paste(DGPDistribution,DGPStationary,sep = '_'))%>%
  mutate(BaseMethod=paste(.simpleCap(BaseDependence),.simpleCap(BaseDistribution),sep = ' '))%>%
  select(EvaluationPeriod,Method,Score,DGP,BaseMethod,BaseModel)->res_variogram

BaseMs<-unique(res_energy$BaseMethod)

res_energy%>%
  filter(BaseModel=='arima')%>%
  filter(DGP=='gaussian_stationary')%>%
  filter(!(Method %in% c('WLS',
                         'MinTSam',
                         'ScoreOptEIn',
                         'ScoreOptVIn')))%>%
  select(-BaseModel,-DGP)%>%
  group_by(Method, BaseMethod)%>%
  summarise(Score=mean(Score))%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP')->tabgse

capture.output(print(tabgse),file = 'PaperTabFigs/tabgse.tex')

pdf('PaperTabFigs/gse.pdf')
        
par(mfrow=c(2,2))
for (b in BaseMs){
  res_energy%>%
    filter(BaseModel=='arima')%>%
    filter(DGP=='gaussian_stationary')%>%
    filter(!(Method %in% c('WLS',
                           'MinTSam',
                           'ScoreOptEIn',
                           'ScoreOptVIn')))%>%
    filter(BaseMethod==b)%>%
    select(-DGP,-BaseMethod, -BaseModel)%>%
    pivot_wider(names_from = Method, values_from = Score)%>%
    select(-EvaluationPeriod)->dat
  datm<-as.matrix(dat) 
  nemenyi(datm,plottype = 'matrix',main=b)
}
dev.off()


res_variogram%>%
  filter(BaseModel=='arima')%>%
  filter(DGP=='gaussian_stationary')%>%
  filter(!(Method %in% c('WLS',
                         'MinTSam',
                         'ScoreOptEIn',
                         'ScoreOptVIn')))%>%
  select(-BaseModel,-DGP)%>%
  group_by(Method, BaseMethod)%>%
  summarise(Score=mean(Score))%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP')->tabgsv

capture.output(print(tabgsv),file = 'PaperTabFigs/tabgsv.tex')

pdf('PaperTabFigs/gsv.pdf')

par(mfrow=c(2,2))
for (b in BaseMs){
  res_variogram%>%
    filter(BaseModel=='arima')%>%
    filter(DGP=='gaussian_stationary')%>%
    filter(!(Method %in% c('WLS',
                           'MinTSam',
                           'ScoreOptEIn',
                           'ScoreOptVIn')))%>%
    filter(BaseMethod==b)%>%
    select(-DGP,-BaseMethod, -BaseModel)%>%
    pivot_wider(names_from = Method, values_from = Score)%>%
    select(-EvaluationPeriod)->dat
  datm<-as.matrix(dat) 
  nemenyi(datm,plottype = 'matrix',main=b)
}
dev.off()


res_energy%>%
  filter(BaseModel=='arima')%>%
  filter(DGP=='nongaussian_stationary')%>%
  filter(!(Method %in% c('WLS',
                         'MinTSam',
                         'ScoreOptEIn',
                         'ScoreOptVIn')))%>%
  select(-BaseModel,-DGP)%>%
  group_by(Method, BaseMethod)%>%
  summarise(Score=mean(Score))%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP')->tabnse
capture.output(print(tabnse),file = 'PaperTabFigs/tabnse.tex')

pdf('PaperTabFigs/nse.pdf')

par(mfrow=c(2,2))
for (b in BaseMs){
  res_energy%>%
    filter(BaseModel=='arima')%>%
    filter(DGP=='nongaussian_stationary')%>%
    filter(!(Method %in% c('WLS',
                           'MinTSam',
                           'ScoreOptEIn',
                           'ScoreOptVIn')))%>%
    filter(BaseMethod==b)%>%
    select(-DGP,-BaseMethod, -BaseModel)%>%
    pivot_wider(names_from = Method, values_from = Score)%>%
    select(-EvaluationPeriod)->dat
  datm<-as.matrix(dat) 
  nemenyi(datm,plottype = 'matrix',main=b)
}
dev.off()

res_variogram%>%
  filter(BaseModel=='arima')%>%
  filter(DGP=='nongaussian_stationary')%>%
  filter(!(Method %in% c('WLS',
                         'MinTSam',
                         'ScoreOptEIn',
                         'ScoreOptVIn')))%>%
  select(-BaseModel,-DGP)%>%
  group_by(Method, BaseMethod)%>%
  summarise(Score=mean(Score))%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP')->tabnsv

pdf('PaperTabFigs/nsv.pdf')

par(mfrow=c(2,2))
for (b in BaseMs){
  res_variogram%>%
    filter(BaseModel=='arima')%>%
    filter(DGP=='nongaussian_stationary')%>%
    filter(!(Method %in% c('WLS',
                           'MinTSam',
                           'ScoreOptEIn',
                           'ScoreOptVIn')))%>%
    filter(BaseMethod==b)%>%
    select(-DGP,-BaseMethod, -BaseModel)%>%
    pivot_wider(names_from = Method, values_from = Score)%>%
    select(-EvaluationPeriod)->dat
  datm<-as.matrix(dat) 
  nemenyi(datm,plottype = 'matrix',main=b)
}
dev.off()

capture.output(print(tabnsv),file = 'PaperTabFigs/tabnsv.tex')