library(tidyverse)
library(fable)
library(ProbReco)
order<-c("Total","non-Renewable","Renewable",
         "Coal","Gas","Solar",
         'Hydro (inc. Pumps)',"Battery",
         "Black Coal","Brown Coal",
         "Gas (CCGT)", "Gas (OCGT)", "Gas (Reciprocating)", "Gas (Steam)",
         "Solar (Rooftop)","Solar (Utility)",
         "Hydro", "Pumps",
         "Battery (Charging)", "Battery (Discharging)",
         "Distillate","Biomass","Wind")

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

data<-readRDS('../Data/nem_generation_by_source.rds')

arrange(data,match(Source,order))->datlong

datlong%>%  
  pivot_wider(id_cols = date,names_from = Source,values_from = Generation)%>%
  select(-date)%>%
  as.matrix()->y

m<-model(datlong,arima=ARIMA())
# f<-forecast(m,h = 30)
# 
# 
# autoplot(filter(f,Source%in%c('Total','Renewable','non-Renewable')),
#                 filter(data,Source%in%c('Total','Renewable','non-Renewable')))
# 
# autoplot(filter(f,Source%in%c('Coal','Gas','Distillate','Battery','Hydro (inc. Pumps)','Solar','Wind','Biomass')),
#          filter(data,Source%in%c('Coal','Gas','Distillate','Battery','Hydro (inc. Pumps)','Solar','Wind','Biomass')))
# 
# autoplot(filter(f,Source%in%c('Solar (Rooftop)','Solar (Utility)')),
#          filter(data,Source%in%c('Solar (Rooftop)','Solar (Utility)')))
# 
# autoplot(filter(f,Source%in%c('Battery (Charging)','Battery (Discharging)')),
#          filter(data,Source%in%c('Battery (Charging)','Battery (Discharging)')))
# 
# autoplot(filter(f,Source%in%c('Black Coal','Brown Coal')),
#          filter(data,Source%in%c('Black Coal','Brown Coal')))
# 
# autoplot(filter(f,Source%in%c('Hydro','Pumps')),
#          filter(data,Source%in%c('Hydro','Pumps')))
# 
# autoplot(filter(f,Source%in%c('Gas (CCGT)','Gas (OCGT)','Gas (Reciprocating)','Gas (Steam)')),
#          filter(data,Source%in%c('Gas (CCGT)','Gas (OCGT)','Gas (Reciprocating)','Gas (Steam)')))
# 

m%>%
  residuals%>%
  arrange(match(Source,order))->rlong

rlong%>%
  pivot_wider(id_cols = date,names_from = Source,values_from = .resid)%>%
  select(-date)%>%as.matrix()->r

left_join(datlong,rlong)%>%
  mutate(Predicted=Generation-.resid)%>%
  rename(Realised=Generation)%>%
  select(-.model,-.resid)%>%
  pivot_longer(cols = c(-date,-Source),names_to = 'Value',values_to='Generation')%>%
  ggplot(aes(x=date,y=Generation,col=Value))+geom_line()+
  facet_wrap(~Source,nrow = 6,ncol = 4,scales = 'free_y')
  

yhat<-y-r

tt<-system.time(opt<-inscoreopt(y = t(y),yhat = t(yhat), S = S,control = list(maxIter=10),trace = T))
