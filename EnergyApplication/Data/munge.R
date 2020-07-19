# Script that munges the data from OpenNEM constructing a hierarchy.

library(tidyverse)
library(tsibble)
dat<-read_csv('daily.csv')
dat%>%
  head(-1)%>% #Remove last Observation
  select(date,contains(' -  GWh'))%>%
  rename_all(~gsub(' -  GWh','',.x))%>%
  mutate(date=as.Date(date),
         Battery=rowSums(select(., contains("Battery"))),
         Gas = rowSums(select(., contains("Gas"))),
         Solar = rowSums(select(., contains("Solar"))),
         Coal = rowSums(select(., contains("Coal"))),
         `Hydro (inc. Pumps)` = Hydro + Pumps,
         Renewable=Biomass+Hydro+Solar+Wind,
         `non-Renewable`=Coal+Distillate+Gas,
         Total=Renewable+`non-Renewable`+Battery+Pumps)%>%
  pivot_longer(cols=-date,names_to = 'Source',values_to = 'Generation')->alldata
  


data_ts<-as_tsibble(alldata,key = Source)

saveRDS(data_ts,'nem_generation_by_source.rds')
