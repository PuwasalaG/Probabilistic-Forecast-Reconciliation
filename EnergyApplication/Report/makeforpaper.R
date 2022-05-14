#Script to report results

rm(list=ls())

library(tidyverse)
library(tsutils)
library(fable)
library(kableExtra)
library(corrplot)
library(tsibble)
library(ggthemes)

#Plots of data

dat<-readRDS('../Data/nem_generation_by_source.rds')%>%
  as_tibble()%>%
  rename(Date=date)


pdf('tsPlots/toptwolevels.pdf')

dat%>%
  filter(Source%in%c('Total',
         'non-Renewable',
         'Renewable'))%>%
  ggplot(aes(x=Date,y=Generation))+
  geom_line()+
  facet_wrap(~Source,
             nrow = 3, 
             ncol = 1,
             scales = 'free_y')+theme_bw()

dev.off()

pdf('tsPlots/fossils.pdf')

dat%>%
  filter(Source%in%c('Coal',
                     'Black Coal',
                     'Brown Coal',
                     'Gas',
                     'Gas (OCGT)',
                     'Gas (CCGT)',
                     'Gas (Reciprocating)',
                     'Gas (Steam)',                     
                     'Distillate'))%>%
  ggplot(aes(x=Date,y=Generation))+
  geom_line()+
  facet_wrap(~Source,
             nrow = 3, 
             ncol = 3,
             scales = 'free_y')+theme_bw()
       
dev.off()

pdf('tsPlots/batteryhydrosolar.pdf')

dat%>%
  filter(Source%in%c('Solar',
                     'Solar (Rooftop)',
                     'Solar (Utility)',
                     'Hydro (inc. Pumps)',
                     'Hydro',
                     'Pumps',
                     'Battery',
                     'Battery (Charging)',
                     'Battery (Discharging)'))%>%
  ggplot(aes(x=Date,y=Generation))+
  geom_line()+
  facet_wrap(~Source,
             nrow = 3, 
             ncol = 3,
             scales = 'free_y')+theme_bw()

dev.off()

pdf('tsPlots/windbiomas.pdf')

dat%>%
  filter(Source%in%c('Wind',
                     'Biomass'))%>%
  ggplot(aes(x=Date,y=Generation))+
  geom_line()+
  facet_wrap(~Source,
             nrow = 2, 
             ncol = 1,
             scales = 'free_y')+theme_bw()

dev.off()

pdf('forPaper/selected.pdf')

dat%>%
  filter(Source%in%c('Total',
                     'Wind',
                     'Solar',
                     'Distillate'))%>%
  mutate(Source=ordered(Source,levels=c('Total',
                               'Wind',
                               'Solar',
                               'Distillate')))%>%
  ggplot(aes(x=Date,y=Generation))+
  geom_line()+
  facet_wrap(~Source,
             nrow = 4, 
             ncol = 1,
             scales = 'free_y')+theme_bw()

dev.off()

# #Base Results
# 
# #Pull data for 21st January
# b30<-readRDS('../Base_Results/base_30.rds')
# 
# dates<-seq.Date(from=as.Date('2019/10/2'),to=as.Date('2020/01/21'),by=1)
# as_tibble(b30$resid)%>%
#   add_column(Date=dates)->resid_df
# 
# pdf('forPaper/densities.pdf')
# resid_df%>%
#   select(Date,Total,Coal,Gas,Wind,Distillate,`Solar (Rooftop)`, `Solar (Utility)`,Pumps,Biomass)%>%
#   pivot_longer(cols = -Date,names_to = 'Source', values_to = 'Generation')%>%
#   ggplot(aes(x=Generation))+
#   geom_histogram()+
#   facet_wrap(~Source,nrow = 3,3,scales = 'free')
# dev.off()
# 
# pdf('forPaper/corr.pdf')
# b30$fc_Sigma_shr%>%cov2cor%>%corrplot  
# dev.off()

#Base Results

#Pull data for 21st January
b225<-readRDS('../Base_Results/base_226.rds')

dates<-seq.Date(from=as.Date('2019/09/11'),to=as.Date('2020/01/21'),by=1)
as_tibble(b225$resid %>% t())%>%
  add_column(Date=dates)->resid_df

order<-c("Total","non-Renewable","Renewable",
         "Coal","Gas","Solar",
         'Hydro (inc. Pumps)',"Battery",
         "Black Coal","Brown Coal",
         "Gas (CCGT)", "Gas (OCGT)", "Gas (Reciprocating)", "Gas (Steam)",
         "Solar (Rooftop)","Solar (Utility)",
         "Hydro", "Pumps",
         "Battery (Charging)", "Battery (Discharging)",
         "Distillate","Biomass","Wind")
names(resid_df) <- c(order, "Date")

pdf('forPaper/densities.pdf')
resid_df%>%
  select(Date,Total,Coal,Gas,Wind,Distillate,`Solar (Rooftop)`, `Solar (Utility)`,Pumps,Biomass)%>%
  pivot_longer(cols = -Date,names_to = 'Source', values_to = 'Generation')%>%
  ggplot(aes(x=Generation))+
  geom_histogram()+
  facet_wrap(~Source,nrow = 3,3,scales = 'free')+theme_bw()
dev.off()

pdf('forPaper/corr.pdf')
fc_sigma_shr <- b225$fc_Sigma_shr
rownames(fc_sigma_shr) <- order
colnames(fc_sigma_shr) <- order
fc_sigma_shr%>%cov2cor%>%corrplot  
dev.off()


#Reconciliation results

all<-read_csv('all.csv')

all%>%
  filter(ScoreEval=='Energy',
         Method!='ScoreOptEIn',
         #Method!='ScoreOptV',
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
  filter(ScoreEval=='Variogram',
         Method!='ScoreOptEIn',
         #Method!='ScoreOptV',
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
         `Joint Gaussian`=gaussian_joint)->MeanScoreVariogram

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


pdf('forPaper/meanscore.pdf')

MeanScoreEnergy%>%
  pivot_longer(cols = -RecoMethod,
               names_to = 'Base Method',
               values_to='Score')%>%
  mutate(`Base Method`=ordered(`Base Method`,
                               levels = c('Ind. Gaussian',
                                          'Ind. Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  rename(`Reconciliation\n Method`=RecoMethod)%>%
  add_column(Sc='Energy')->p1

MeanScoreVariogram%>%
  pivot_longer(cols = -RecoMethod,
               names_to = 'Base Method',
               values_to='Score')%>%
  mutate(`Base Method`=ordered(`Base Method`,
                               levels = c('Ind. Gaussian',
                                          'Ind. Bootstrap',
                                          'Joint Gaussian',
                                          'Joint Bootstrap'),
                               labels = c('Indep.\n Gaussian',
                                          'Indep.\n Bootstrap',
                                          'Joint\n Gaussian',
                                          'Joint\n Bootstrap')))%>%
  rename(`Reconciliation\n Method`=RecoMethod)%>%
  add_column(Sc='Variogram')->p2

rbind(p1,p2)%>%  
  ggplot(aes(x=`Base Method`,
             y=`Score`,
             col=`Reconciliation\n Method`,
             group=`Reconciliation\n Method`))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  facet_wrap(~Sc,scales = 'free_y')+
  scale_color_colorblind()+theme_bw()

dev.off()

pdf('forPaper/meanenergyscore.pdf')
p1%>%  
  filter(`Reconciliation\n Method`!='ScoreOptV')%>%
  ggplot(aes(x=`Base Method`,
             y=`Score`,
             col=`Reconciliation\n Method`,
             group=`Reconciliation\n Method`))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  scale_color_colorblind()+theme_bw()

dev.off()

