##Following codes will analyse the reults we obtained from Parametric approach
##with Non-Gaussian data

library(tidyverse)

###########################
  #-Without Optimal G-#
###########################


###--Full Hierarchy--###


DF_MultiV_Full_NonGaussDGP_W.o_Optim <- read.csv("Without-Optimal/Results/DF_MultiV_Full_NonGaussianDGP.csv")[,-1]


DF_MultiV_Full_NonGaussDGP_W.o_Optim %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_NonGaussDGP_W.o_Optim

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_NonGaussDGP_W.o_Optim %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Bottom up") -> DF_MultScores_AllTS_NonGaussDGP_W.o_Optim



##--Calculate the skill scores--# 


DF_MultScores_AllTS_NonGaussDGP_W.o_Optim %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> BU_E.ES_AllTS_NonGaussDGP_W.o_Optim 

DF_MultScores_AllTS_NonGaussDGP_W.o_Optim %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> BU_E.VS_AllTS_NonGaussDGP_W.o_Optim  


DF_MultScores_AllTS_NonGaussDGP_W.o_Optim %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/BU_E.ES_AllTS_NonGaussDGP_W.o_Optim))*100, digits = 2),
         SS_E.VS = round((1-(`E.VS`/BU_E.VS_AllTS_NonGaussDGP_W.o_Optim))*100, digits = 2)) -> DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim

DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim %>%  
  dplyr::select(-`E.ES`, -`E.VS`) -> DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim


# DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_NonGaussDGP_W.o_Optim
# 
# DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_NonGaussDGP_W.o_Optim

# View(SS_E.ES_AllTS_NonGaussDGP_W.o_Optim)
# View(SS_E.VS_AllTS_NonGaussDGP_W.o_Optim)



###--Bottom level of the Hierarchy--###



DF_MultiV_Bot_NonGaussDGP_W.o_Optim <- read.csv("Without-Optimal/Results/DF_MultiV_Bot_NonGaussianDGP.csv")[,-1]


DF_MultiV_Bot_NonGaussDGP_W.o_Optim %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Variogram.score`)) -> DF_MultiV_Bot_NonGaussDGP_W.o_Optim

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_NonGaussDGP_W.o_Optim %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_NonGaussDGP_W.o_Optim

##--Calculate the skill scores--# 

DF_MultScores_BotTS_NonGaussDGP_W.o_Optim %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% 
  as_vector() -> BU_E.LS_BotTS_NonGaussDGP_W.o_Optim

DF_MultScores_BotTS_NonGaussDGP_W.o_Optim %>% 
  mutate(SS_E.LS = round((1-(`E.LS`/BU_E.LS_BotTS_NonGaussDGP_W.o_Optim))*100, digits = 2)) -> DF_MultScore_SS_BotTS_NonGaussDGP_W.o_Optim

# DF_MultScore_SS_BotTS_NonGaussDGP_W.o_Optim %>%  
#   dplyr::select(-`E.LS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.LS`) -> SS_E.LS_BotTS_NonGaussDGP_W.o_Optim

# View(SS_E.LS_BotTS_NonGaussDGP_W.o_Optim)


DF_MultScore_SS_BotTS_NonGaussDGP_W.o_Optim %>% 
  ungroup() %>% 
  pull(SS_E.LS) -> SS_E.LS_W.o_Optim

DF_MultScore_SS_AllTS_NonGaussDGP_W.o_Optim %>% 
  ungroup() %>% 
  add_column(SS_E.LS = SS_E.LS_W.o_Optim) -> SkillScore_full_hier_W.o_Optim

SkillScore_full_hier_W.o_Optim %>% 
  gather(key = key, value = value, SS_E.ES, SS_E.VS, SS_E.LS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% View()


###########################
#--Optimal G_M1--#
###########################


###--Full Hierarchy--###


DF_MultiV_Full_NonGaussDGP_Optim_M1 <- read.csv("OptimalG_M1/Results/DF_MultiV_Full_NonGaussianDGP.csv")[,-1]


DF_MultiV_Full_NonGaussDGP_Optim_M1 %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_NonGaussDGP_Optim_M1

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_NonGaussDGP_Optim_M1 %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Bottom up") -> DF_MultScores_AllTS_NonGaussDGP_Optim_M1



##--Calculate the skill scores--# 


DF_MultScores_AllTS_NonGaussDGP_Optim_M1 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> BU_E.ES_AllTS_NonGaussDGP_Optim_M1 

DF_MultScores_AllTS_NonGaussDGP_Optim_M1 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> BU_E.VS_AllTS_NonGaussDGP_Optim_M1  


DF_MultScores_AllTS_NonGaussDGP_Optim_M1 %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/BU_E.ES_AllTS_NonGaussDGP_Optim_M1))*100, digits = 2),
         SS_E.VS = round((1-(`E.VS`/BU_E.VS_AllTS_NonGaussDGP_Optim_M1))*100, digits = 2)) -> DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1

DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1 %>%  
  dplyr::select(-`E.ES`, -`E.VS`) -> DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1


# DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1 %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_NonGaussDGP_Optim_M1
# 
# DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1 %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_NonGaussDGP_Optim_M1

# View(SS_E.ES_AllTS_NonGaussDGP_Optim_M1)
# View(SS_E.VS_AllTS_NonGaussDGP_Optim_M1)



###--Bottom level of the Hierarchy--###



DF_MultiV_Bot_NonGaussDGP_Optim_M1 <- read.csv("OptimalG_M1/Results/DF_MultiV_Bot_NonGaussianDGP.csv")[,-1]


DF_MultiV_Bot_NonGaussDGP_Optim_M1 %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Variogram.score`)) -> DF_MultiV_Bot_NonGaussDGP_Optim_M1

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_NonGaussDGP_Optim_M1 %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_NonGaussDGP_Optim_M1

##--Calculate the skill scores--# 

DF_MultScores_BotTS_NonGaussDGP_Optim_M1 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% 
  as_vector() -> BU_E.LS_BotTS_NonGaussDGP_Optim_M1

DF_MultScores_BotTS_NonGaussDGP_Optim_M1 %>% 
  mutate(SS_E.LS = round((1-(`E.LS`/BU_E.LS_BotTS_NonGaussDGP_Optim_M1))*100, digits = 2)) -> DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M1

# DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M1 %>%  
#   dplyr::select(-`E.LS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.LS`) -> SS_E.LS_BotTS_NonGaussDGP_Optim_M1

# View(SS_E.LS_BotTS_NonGaussDGP_Optim_M1)


DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M1 %>% 
  ungroup() %>% 
  pull(SS_E.LS) -> SS_E.LS_Optim_M1

DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M1 %>% 
  ungroup() %>% 
  add_column(SS_E.LS = SS_E.LS_Optim_M1) -> SkillScore_full_hier_Optim_M1

SkillScore_full_hier_Optim_M1 %>% 
  gather(key = key, value = value, SS_E.ES, SS_E.VS, SS_E.LS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% View()



###########################
    #--Optimal G_M2--#
###########################


###--Full Hierarchy--###


DF_MultiV_Full_NonGaussDGP_Optim_M2 <- read.csv("OptimalG_M2/Results/DF_MultiV_Full_NonGaussianDGP.csv")[,-1]


DF_MultiV_Full_NonGaussDGP_Optim_M2 %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_NonGaussDGP_Optim_M2

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_NonGaussDGP_Optim_M2 %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Bottom up") -> DF_MultScores_AllTS_NonGaussDGP_Optim_M2



##--Calculate the skill scores--# 


DF_MultScores_AllTS_NonGaussDGP_Optim_M2 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> BU_E.ES_AllTS_NonGaussDGP_Optim_M2 

DF_MultScores_AllTS_NonGaussDGP_Optim_M2 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> BU_E.VS_AllTS_NonGaussDGP_Optim_M2  


DF_MultScores_AllTS_NonGaussDGP_Optim_M2 %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/BU_E.ES_AllTS_NonGaussDGP_Optim_M2))*100, digits = 2),
         SS_E.VS = round((1-(`E.VS`/BU_E.VS_AllTS_NonGaussDGP_Optim_M2))*100, digits = 2)) -> DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2

DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2 %>%  
  dplyr::select(-`E.ES`, -`E.VS`) -> DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2


# DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2 %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_NonGaussDGP_Optim_M2
# 
# DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2 %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_NonGaussDGP_Optim_M2

# View(SS_E.ES_AllTS_NonGaussDGP_Optim_M2)
# View(SS_E.VS_AllTS_NonGaussDGP_Optim_M2)



###--Bottom level of the Hierarchy--###



DF_MultiV_Bot_NonGaussDGP_Optim_M2 <- read.csv("OptimalG_M2/Results/DF_MultiV_Bot_NonGaussianDGP.csv")[,-1]


DF_MultiV_Bot_NonGaussDGP_Optim_M2 %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Variogram.score`)) -> DF_MultiV_Bot_NonGaussDGP_Optim_M2

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_NonGaussDGP_Optim_M2 %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_NonGaussDGP_Optim_M2

##--Calculate the skill scores--# 

DF_MultScores_BotTS_NonGaussDGP_Optim_M2 %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% 
  as_vector() -> BU_E.LS_BotTS_NonGaussDGP_Optim_M2

DF_MultScores_BotTS_NonGaussDGP_Optim_M2 %>% 
  mutate(SS_E.LS = round((1-(`E.LS`/BU_E.LS_BotTS_NonGaussDGP_Optim_M2))*100, digits = 2)) -> DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M2

# DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M2 %>%  
#   dplyr::select(-`E.LS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.LS`) -> SS_E.LS_BotTS_NonGaussDGP_Optim_M2

# View(SS_E.LS_BotTS_NonGaussDGP_Optim_M2)


DF_MultScore_SS_BotTS_NonGaussDGP_Optim_M2 %>% 
  ungroup() %>% 
  pull(SS_E.LS) -> SS_E.LS_Optim_M2

DF_MultScore_SS_AllTS_NonGaussDGP_Optim_M2 %>% 
  ungroup() %>% 
  add_column(SS_E.LS = SS_E.LS_Optim_M2) -> SkillScore_full_hier_Optim_M2

SkillScore_full_hier_Optim_M2 %>% 
  gather(key = key, value = value, SS_E.ES, SS_E.VS, SS_E.LS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% View()


