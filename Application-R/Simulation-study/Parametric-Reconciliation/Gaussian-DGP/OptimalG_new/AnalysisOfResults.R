##Following codes will analyse the reults we obtained from Non-Parametric approach

library(tidyverse)



DF_MultiV_Full_GausDGP <- read.csv("Results/DF_MultiV_Full_GaussianDGP.csv")[,-1]


DF_MultiV_Full_GausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_GausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_GausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Bottom up") -> DF_MultScores_AllTS_GausDGP

##--Calculate the skill scores--# 



##--For ARIMA--##

DF_MultScores_AllTS_GausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> BU_E.ES_AllTS_GausDGP 

DF_MultScores_AllTS_GausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> BU_E.VS_AllTS_GausDGP  


DF_MultScores_AllTS_GausDGP %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/BU_E.ES_AllTS_GausDGP))*100, digits = 2),
         SS_E.VS = round((1-(`E.VS`/BU_E.VS_AllTS_GausDGP))*100, digits = 2)) -> DF_MultScore_SS_AllTS_GausDGP

DF_MultScore_SS_AllTS_GausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`) -> DF_MultScore_SS_AllTS_GausDGP


DF_MultScore_SS_AllTS_GausDGP %>%
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_GausDGP

DF_MultScore_SS_AllTS_GausDGP %>%
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_GausDGP

# View(SS_E.ES_AllTS_GausDGP)
# View(SS_E.VS_AllTS_GausDGP)



########################################
### Bottom level of the Hierarchy ###
########################################



DF_MultiV_Bot_GausDGP <- read.csv("Results/DF_MultiV_Bot_GaussianDGP.csv")[,-1]


DF_MultiV_Bot_GausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Variogram.score`)) -> DF_MultiV_Bot_GausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_GausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_GausDGP

##--Calculate the skill scores--# 

DF_MultScores_BotTS_GausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% 
  as_vector() -> BU_E.LS_BotTS_GausDGP 

DF_MultScores_BotTS_GausDGP %>% 
  mutate(SS_E.LS = round((1-(`E.LS`/BU_E.LS_BotTS_GausDGP))*100, digits = 2)) -> DF_MultScore_SS_BotTS_GausDGP

# DF_MultScore_SS_BotTS_GausDGP %>%  
#   dplyr::select(-`E.LS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.LS`) -> SS_E.LS_BotTS_GausDGP

# View(SS_E.LS_BotTS_GausDGP)


DF_MultScore_SS_BotTS_GausDGP %>% 
  ungroup() %>% 
  pull(SS_E.LS) -> SS_E.LS

DF_MultScore_SS_AllTS_GausDGP %>% 
  ungroup() %>% 
  add_column(SS_E.LS = SS_E.LS) -> SkillScore_full_hier

SkillScore_full_hier %>% 
  gather(key = key, value = value, SS_E.ES, SS_E.VS, SS_E.LS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% View()
