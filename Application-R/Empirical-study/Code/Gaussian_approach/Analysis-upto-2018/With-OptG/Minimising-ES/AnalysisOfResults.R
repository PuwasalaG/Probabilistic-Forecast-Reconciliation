##Following codes will analyse the reults we obtained from Non-Parametric approach

library(tidyverse)

###########################################
### All level of the Hierarchy ###
###########################################

DF_MultiV_Total_1_25 <- read.csv("Results/DF_MultiV_Total_1-25.csv")[,-1]
DF_MultiV_Total_26_50 <- read.csv("Results/DF_MultiV_Total_26-50.csv")[,-1]
DF_MultiV_Total_51_75 <- read.csv("Results/DF_MultiV_Total_51-75.csv")[,-1]
DF_MultiV_Total_76_89 <- read.csv("Results/DF_MultiV_Total_76-89.csv")[,-1]
DF_MultiV_Total_90_100 <- read.csv("Results/DF_MultiV_Total_90-100.csv")[,-1]


rbind(DF_MultiV_Total_1_25, DF_MultiV_Total_26_50, DF_MultiV_Total_51_75, 
      DF_MultiV_Total_76_89, DF_MultiV_Total_90_100) %>%
  as.data.frame() -> DF_MultiV_Total
write.csv(x=DF_MultiV_Total, file = "DF_MultiV_Total.csv")

DF_MultiV_Total %>% 
  dplyr::select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  dplyr::summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_AllTS


##--Calculate the skill scores--# 


DF_MultScores_AllTS %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_AllTS

DF_MultScores_AllTS %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_AllTS


DF_MultScores_AllTS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_AllTS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_AllTS))*100, digits = 4)) -> DF_MultScore_SS_AllTS

DF_MultScore_SS_AllTS %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS

DF_MultScore_SS_AllTS %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS

# View(SS_E.ES_AllTS)
# View(SS_E.VS_AllTS)


###########################################
    ### State level of the Hierarchy ###
###########################################


DF_MultiV_States_1_25 <- read.csv("Results/DF_MultiV_States_1-25.csv")[,-1]
DF_MultiV_States_26_50 <- read.csv("Results/DF_MultiV_States_26-50.csv")[,-1]
DF_MultiV_States_51_75 <- read.csv("Results/DF_MultiV_States_51-75.csv")[,-1]
DF_MultiV_States_76_89 <- read.csv("Results/DF_MultiV_States_76-89.csv")[,-1]
DF_MultiV_States_90_100 <- read.csv("Results/DF_MultiV_States_90-100.csv")[,-1]


rbind(DF_MultiV_States_1_25, DF_MultiV_States_26_50, DF_MultiV_States_51_75, 
      DF_MultiV_States_76_89, DF_MultiV_States_90_100) %>%
  as.data.frame() -> DF_MultiV_States
write.csv(x=DF_MultiV_States, file = "DF_MultiV_States.csv")

DF_MultiV_States %>% 
  dplyr::select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  dplyr::summarise(E.ES = mean(`Energy.score`), 
                   E.VS = mean(`Variogram.score`)) -> DF_MultScores_States


##--Calculate the skill scores--# 


DF_MultScores_States %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_States

DF_MultScores_States %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_States


DF_MultScores_States %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_States))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_States))*100, digits = 4)) -> DF_MultScore_SS_States

DF_MultScore_SS_States %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_States

DF_MultScore_SS_States %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_States

# View(SS_E.ES_States)
# View(SS_E.VS_States)


##########################################
    ### Zone level of the Hierarchy ###
##########################################


DF_MultiV_Zones_1_25 <- read.csv("Results/DF_MultiV_Zones_1-25.csv")[,-1]
DF_MultiV_Zones_26_50 <- read.csv("Results/DF_MultiV_Zones_26-50.csv")[,-1]
DF_MultiV_Zones_51_75 <- read.csv("Results/DF_MultiV_Zones_51-75.csv")[,-1]
DF_MultiV_Zones_76_89 <- read.csv("Results/DF_MultiV_Zones_76-89.csv")[,-1]
DF_MultiV_Zones_90_100 <- read.csv("Results/DF_MultiV_Zones_90-100.csv")[,-1]


rbind(DF_MultiV_Zones_1_25, DF_MultiV_Zones_26_50, DF_MultiV_Zones_51_75, 
      DF_MultiV_Zones_76_89, DF_MultiV_Zones_90_100) %>%
  as.data.frame() -> DF_MultiV_Zones
write.csv(x=DF_MultiV_Zones, file = "DF_MultiV_Zones.csv")

DF_MultiV_Zones %>% 
  dplyr::select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  dplyr::summarise(E.ES = mean(`Energy.score`), 
                   E.VS = mean(`Variogram.score`)) -> DF_MultScores_Zones


##--Calculate the skill scores--# 


DF_MultScores_Zones %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Zones

DF_MultScores_Zones %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Zones


DF_MultScores_Zones %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Zones))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Zones))*100, digits = 4)) -> DF_MultScore_SS_Zones

DF_MultScore_SS_Zones %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_Zones

DF_MultScore_SS_Zones %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_Zones

# View(SS_E.ES_Zones)
# View(SS_E.VS_Zones)


#######################################################
    ### Regional(Bottom) level of the Hierarchy ###
#######################################################




DF_MultiV_Regions_1_25 <- read.csv("Results/DF_MultiV_Regions_1-25.csv")[,-1]
DF_MultiV_Regions_26_50 <- read.csv("Results/DF_MultiV_Regions_26-50.csv")[,-1]
DF_MultiV_Regions_51_75 <- read.csv("Results/DF_MultiV_Regions_51-75.csv")[,-1]
DF_MultiV_Regions_76_89 <- read.csv("Results/DF_MultiV_Regions_76-89.csv")[,-1]
DF_MultiV_Regions_90_100 <- read.csv("Results/DF_MultiV_Regions_90-100.csv")[,-1]


rbind(DF_MultiV_Regions_1_25, DF_MultiV_Regions_26_50, DF_MultiV_Regions_51_75, 
      DF_MultiV_Regions_76_89, DF_MultiV_Regions_90_100) %>%
  as.data.frame() -> DF_MultiV_Regions

write.csv(x=DF_MultiV_Regions, file = "DF_MultiV_Regions.csv")

DF_MultiV_Regions %>% 
  dplyr::select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  dplyr::summarise(E.ES = mean(`Energy.score`), 
                   E.VS = mean(`Variogram.score`)) -> DF_MultScores_Regions


##--Calculate the skill scores--# 


DF_MultScores_Regions %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Regions

DF_MultScores_Regions %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Regions


DF_MultScores_Regions %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Regions))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Regions))*100, digits = 4)) -> DF_MultScore_SS_Regions

DF_MultScore_SS_Regions %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_Regions

DF_MultScore_SS_Regions %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_Regions

# View(SS_E.ES_Regions)
# View(SS_E.VS_Regions)



################################
    ###Univariate Scores###
################################

DF_UniV_1_25 <- read.csv("Results/DF_UniV_1-25.csv")[,-1]
DF_UniV_26_50 <- read.csv("Results/DF_UniV_26-50.csv")[,-1]
DF_UniV_51_75 <- read.csv("Results/DF_UniV_51-75.csv")[,-1]
DF_UniV_76_89 <- read.csv("Results/DF_UniV_76-89.csv")[,-1]
DF_UniV_90_100 <- read.csv("Results/DF_UniV_90-100.csv")[,-1]


rbind(DF_UniV_1_25, DF_UniV_26_50, DF_UniV_51_75, 
      DF_UniV_76_89, DF_UniV_90_100) %>%
  as.data.frame() -> DF_UniV

DF_UniV <- DF_UniV[complete.cases(DF_UniV[ , "R.method"]),] 

write.csv(x=DF_UniV, file = "DF_UniV.csv")
