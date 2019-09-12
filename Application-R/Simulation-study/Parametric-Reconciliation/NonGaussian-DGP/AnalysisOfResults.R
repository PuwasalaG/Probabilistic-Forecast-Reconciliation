##Following codes will analyse the reults we obtained from Non-Parametric approach

library(tidyverse)



DF_MultiV_Full_NonGausDGP <- read.csv("Results/DF_MultiV_Full_NonGaussianDGP.csv")[,-1]


DF_MultiV_Full_NonGausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_NonGausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_NonGausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_AllTS_NonGausDGP

##--Calculate the skill scores--# 



##--For ARIMA--##

DF_MultScores_AllTS_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_AllTS_NonGausDGP 

DF_MultScores_AllTS_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_AllTS_NonGausDGP  


DF_MultScores_AllTS_NonGausDGP %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_AllTS_NonGausDGP))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_AllTS_NonGausDGP))*100, digits = 4)) -> DF_MultScore_SS_AllTS_NonGausDGP

DF_MultScore_SS_AllTS_NonGausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_NonGausDGP

DF_MultScore_SS_AllTS_NonGausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_NonGausDGP

# View(SS_E.ES_AllTS_NonGausDGP)
# View(SS_E.VS_AllTS_NonGausDGP)



########################################
### Bottom level of the Hierarchy ###
########################################



DF_MultiV_Bot_NonGausDGP <- read.csv("Results/DF_MultiV_Bot_GaussianDGP.csv")[,-1]


DF_MultiV_Bot_NonGausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Bot_NonGausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_NonGausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_NonGausDGP

##--Calculate the skill scores--# 



##--For ARIMA--##

DF_MultScores_BotTS_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% 
  as_vector() -> Base_E.ES_BotTS_NonGausDGP 

DF_MultScores_BotTS_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_BotTS_NonGausDGP  

DF_MultScores_BotTS_NonGausDGP %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_BotTS_NonGausDGP))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_BotTS_NonGausDGP))*100, digits = 4)) -> DF_MultScore_SS_BotTS_NonGausDGP

DF_MultScore_SS_BotTS_NonGausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_BotTS_NonGausDGP

DF_MultScore_SS_BotTS_NonGausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_BotTS_NonGausDGP

# View(SS_E.ES_BotTS_NonGausDGP)
# View(SS_E.VS_BotTS_NonGausDGP)
