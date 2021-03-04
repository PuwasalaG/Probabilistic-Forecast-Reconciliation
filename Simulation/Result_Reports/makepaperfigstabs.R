# Script to make tables used in paper

library(tidyverse)
library(tsutils)
library(kableExtra)
library(ggthemes)
library(gridExtra)
res_all<-readRDS('../Evaluate/all_results.rds')

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

BaseMs<-unique(res_energy$BaseMethod)[c(2,1,4,3)]

res_energy%>%
  filter(BaseModel=='arima')%>%
  filter(DGP=='gaussian_stationary')%>%
  filter(!(Method %in% c('WLS',
                         'MinTSam',
                         'ScoreOptEIn',
                         'ScoreOptVIn')))%>%
  select(-BaseModel,-DGP)%>%
  group_by(Method, BaseMethod)%>%
  summarise(Score=mean(Score))->figgse
figgse%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean Energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP. Values 
        in bold indicate the best reconciliation 
        method for a given base forecasting method.')->tabgse

capture.output(print(tabgse),file = 'PaperTabFigs/tabgse.tex')


pdf('PaperTabFigs/gse.pdf')
        
par(mfrow=c(2,2),cex.axis=0.7,mai=c(0,0,0.25,0))
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
  nemenyi(datm,plottype = 'matrix',sort=FALSE,main=b)
  
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
  summarise(Score=mean(Score))->figgsv
figgsv%>%  
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean Variogram score for ARIMA 
                        modelling with a Gaussian Stationary DGP. Values 
        in bold indicate the best reconciliation method for a given 
        base forecasting method')->tabgsv

capture.output(print(tabgsv),file = 'PaperTabFigs/tabgsv.tex')



pdf('PaperTabFigs/gsv.pdf')

par(mfrow=c(2,2),cex.axis=0.8,mai=c(0,0,0.25,0))
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
  summarise(Score=mean(Score))->fignse
fignse%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean Energy score for ARIMA 
                        modelling with a Gaussian Stationary DGP. Values 
        in bold indicate the best reconciliation method for a given 
        base forecasting method')->tabnse
capture.output(print(tabnse),file = 'PaperTabFigs/tabnse.tex')



pdf('PaperTabFigs/nse.pdf')

par(mfrow=c(2,2),cex.axis=0.8,mai=c(0,0,0.25,0))
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
  summarise(Score=mean(Score))->fignsv
fignsv%>%
  pivot_wider(id_cols = Method,names_from=BaseMethod, values_from = Score)%>%
  kable(digits = 4,format = 'latex',
        caption = 'Mean Variogram score for ARIMA 
                        modelling with a Gaussian Stationary DGP. Values 
        in bold indicate the best reconciliation method for a given 
        base forecasting method')->tabnsv


pdf('PaperTabFigs/nsv.pdf')

par(mfrow=c(2,2),cex.axis=0.8,mai=c(0,0,0.25,0))
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


##Tables as plots


pdf('PaperTabFigs/energy_meanscore.pdf')
figgse%>%
  rename(`Reconciliation\n Method`=Method)%>%
  mutate(`Base Method`=ordered(BaseMethod,
                               levels = c('Independent Gaussian',
                                          'Independent Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  add_column(DGP='Gaussian DGP')->p1

fignse%>%
  rename(`Reconciliation\n Method`=Method)%>%
  mutate(`Base Method`=ordered(BaseMethod,
                               levels = c('Independent Gaussian',
                                          'Independent Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  add_column(DGP='Non-Gaussian DGP')->p2


both<-rbind(p1,p2)
both%>%  
  ggplot(aes(x=`Base Method`,
             y=`Score`,
             col=`Reconciliation\n Method`,
             group=`Reconciliation\n Method`))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  facet_wrap(~DGP,scales = 'free_y')+
  scale_color_colorblind()



dev.off()




pdf('PaperTabFigs/variogram_meanscore.pdf')

figgsv%>%
  rename(`Reconciliation\n Method`=Method)%>%
  mutate(`Base Method`=ordered(BaseMethod,
                               levels = c('Independent Gaussian',
                                          'Independent Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  add_column(DGP='Gaussian DGP')->p1




fignsv%>%
  rename(`Reconciliation\n Method`=Method)%>%
  mutate(`Base Method`=ordered(BaseMethod,
                               levels = c('Independent Gaussian',
                                          'Independent Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  add_column(DGP='Non-Gaussian DGP')->p2
both<-rbind(p1,p2)
both%>%  
  ggplot(aes(x=`Base Method`,
             y=`Score`,
             col=`Reconciliation\n Method`,
             group=`Reconciliation\n Method`))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  facet_wrap(~DGP,scales = 'free_y')+
  scale_color_colorblind()

dev.off()



