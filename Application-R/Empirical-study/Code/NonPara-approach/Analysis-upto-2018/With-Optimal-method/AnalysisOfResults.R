##Following codes will analyse the reults we obtained from Non-Parametric approach

library(tidyverse)



DF_MultiV_Total_1_10 <- read.csv("Results/DF_MultiV_Total_1-10.csv")[,-1]
DF_MultiV_Total_11_20 <- read.csv("Results/DF_MultiV_Total_11-20.csv")[,-1]
DF_MultiV_Total_21_30 <- read.csv("Results/DF_MultiV_Total_21-30.csv")[,-1]
DF_MultiV_Total_31_40 <- read.csv("Results/DF_MultiV_Total_31-40.csv")[,-1]
DF_MultiV_Total_41_50 <- read.csv("Results/DF_MultiV_Total_41-50.csv")[,-1]
DF_MultiV_Total_51_60 <- read.csv("Results/DF_MultiV_Total_51-60.csv")[,-1]
# DF_MultiV_Total_61_70 <- read.csv("Results/DF_MultiV_Total_61-70.csv")[,-1]
# DF_MultiV_Total_71_80 <- read.csv("Results/DF_MultiV_Total_71-80.csv")[,-1]
# DF_MultiV_Total_81_88 <- read.csv("Results/DF_MultiV_Total_81-88.csv")[,-1]

rbind(DF_MultiV_Total_1_10, DF_MultiV_Total_11_20, DF_MultiV_Total_21_30, 
      DF_MultiV_Total_31_40, DF_MultiV_Total_41_50, DF_MultiV_Total_51_60) %>% 
  as.data.frame() -> DF_MultiV_Total

# rbind(DF_MultiV_Total_1_10, DF_MultiV_Total_11_20, DF_MultiV_Total_21_30, 
#       DF_MultiV_Total_31_40, DF_MultiV_Total_41_50, DF_MultiV_Total_51_60, 
#       DF_MultiV_Total_61_70, DF_MultiV_Total_71_80, DF_MultiV_Total_81_88) %>% 
#   as.data.frame() -> DF_MultiV_Total
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


DF_MultiV_States_1_10 <- read.csv("Results/DF_MultiV_States_1-10.csv")[,-1]
DF_MultiV_States_11_20 <- read.csv("Results/DF_MultiV_States_11-20.csv")[,-1]
DF_MultiV_States_21_30 <- read.csv("Results/DF_MultiV_States_21-30.csv")[,-1]
DF_MultiV_States_31_40 <- read.csv("Results/DF_MultiV_States_31-40.csv")[,-1]
DF_MultiV_States_41_50 <- read.csv("Results/DF_MultiV_States_41-50.csv")[,-1]
DF_MultiV_States_51_60 <- read.csv("Results/DF_MultiV_States_51-60.csv")[,-1]
# DF_MultiV_States_61_70 <- read.csv("Results/DF_MultiV_States_61-70.csv")[,-1]
# DF_MultiV_States_71_80 <- read.csv("Results/DF_MultiV_States_71-80.csv")[,-1]
# DF_MultiV_States_81_88 <- read.csv("Results/DF_MultiV_States_81-88.csv")[,-1]

rbind(DF_MultiV_States_1_10, DF_MultiV_States_11_20, DF_MultiV_States_21_30, 
      DF_MultiV_States_31_40, DF_MultiV_States_41_50, DF_MultiV_States_51_60) %>% 
  as.data.frame() -> DF_MultiV_States

# rbind(DF_MultiV_States_1_10, DF_MultiV_States_11_20, DF_MultiV_States_21_30, 
#       DF_MultiV_States_31_40, DF_MultiV_States_41_50, DF_MultiV_States_51_60, 
#       DF_MultiV_States_61_70, DF_MultiV_States_71_80, DF_MultiV_States_81_88) %>% 
#   as.data.frame() -> DF_MultiV_States
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


DF_MultiV_Zones_1_10 <- read.csv("Results/DF_MultiV_Zones_1-10.csv")[,-1]
DF_MultiV_Zones_11_20 <- read.csv("Results/DF_MultiV_Zones_11-20.csv")[,-1]
DF_MultiV_Zones_21_30 <- read.csv("Results/DF_MultiV_Zones_21-30.csv")[,-1]
DF_MultiV_Zones_31_40 <- read.csv("Results/DF_MultiV_Zones_31-40.csv")[,-1]
DF_MultiV_Zones_41_50 <- read.csv("Results/DF_MultiV_Zones_41-50.csv")[,-1]
DF_MultiV_Zones_51_60 <- read.csv("Results/DF_MultiV_Zones_51-60.csv")[,-1]
# DF_MultiV_Zones_61_70 <- read.csv("Results/DF_MultiV_Zones_61-70.csv")[,-1]
# DF_MultiV_Zones_71_80 <- read.csv("Results/DF_MultiV_Zones_71-80.csv")[,-1]
# DF_MultiV_Zones_81_88 <- read.csv("Results/DF_MultiV_Zones_81-88.csv")[,-1]

rbind(DF_MultiV_Zones_1_10, DF_MultiV_Zones_11_20, DF_MultiV_Zones_21_30, 
      DF_MultiV_Zones_31_40, DF_MultiV_Zones_41_50, DF_MultiV_Zones_51_60) %>% 
  as.data.frame() -> DF_MultiV_Zones

# rbind(DF_MultiV_Zones_1_10, DF_MultiV_Zones_11_20, DF_MultiV_Zones_21_30, 
#       DF_MultiV_Zones_31_40, DF_MultiV_Zones_41_50, DF_MultiV_Zones_51_60, 
#       DF_MultiV_Zones_61_70, DF_MultiV_Zones_71_80, DF_MultiV_Zones_81_88) %>% 
#   as.data.frame() -> DF_MultiV_Zones
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




DF_MultiV_Regions_1_10 <- read.csv("Results/DF_MultiV_Regions_1-10.csv")[,-1]
DF_MultiV_Regions_11_20 <- read.csv("Results/DF_MultiV_Regions_11-20.csv")[,-1]
DF_MultiV_Regions_21_30 <- read.csv("Results/DF_MultiV_Regions_21-30.csv")[,-1]
DF_MultiV_Regions_31_40 <- read.csv("Results/DF_MultiV_Regions_31-40.csv")[,-1]
DF_MultiV_Regions_41_50 <- read.csv("Results/DF_MultiV_Regions_41-50.csv")[,-1]
DF_MultiV_Regions_51_60 <- read.csv("Results/DF_MultiV_Regions_51-60.csv")[,-1]
# DF_MultiV_Regions_61_70 <- read.csv("Results/DF_MultiV_Regions_61-70.csv")[,-1]
# DF_MultiV_Regions_71_80 <- read.csv("Results/DF_MultiV_Regions_71-80.csv")[,-1]
# DF_MultiV_Regions_81_88 <- read.csv("Results/DF_MultiV_Regions_81-88.csv")[,-1]

rbind(DF_MultiV_Regions_1_10, DF_MultiV_Regions_11_20, DF_MultiV_Regions_21_30, 
      DF_MultiV_Regions_31_40, DF_MultiV_Regions_41_50, DF_MultiV_Regions_51_60) %>% 
  as.data.frame() -> DF_MultiV_Regions

# rbind(DF_MultiV_Regions_1_10, DF_MultiV_Regions_11_20, DF_MultiV_Regions_21_30, 
#       DF_MultiV_Regions_31_40, DF_MultiV_Regions_41_50, DF_MultiV_Regions_51_60, 
#       DF_MultiV_Regions_61_70, DF_MultiV_Regions_71_80, DF_MultiV_Regions_81_88) %>% 
#   as.data.frame() -> DF_MultiV_Regions
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

DF_UniV_1_100 <- read.csv("Results/DF_UniV_1-100.csv")[,-1]
DF_UniV_101_152 <- read.csv("Results/DF_UniV_101-152.csv")[,-1]

rbind(DF_UniV_1_100, DF_UniV_101_152) %>% 
  as.data.frame() -> DF_UniV

DF_UniV_ARIMA <- DF_UniV[complete.cases(DF_UniV[ , "R.method"]),] %>% 
  filter(`F.method`=="ARIMA")
DF_UniV_ETS <- DF_UniV[complete.cases(DF_UniV[ , "R.method"]),] %>% 
  filter(`F.method`=="ETS")

write.csv(x=DF_UniV_ARIMA, file = "DF_UniV_ARIMA.csv")
write.csv(x=DF_UniV_ETS, file = "DF_UniV_ETS.csv")
