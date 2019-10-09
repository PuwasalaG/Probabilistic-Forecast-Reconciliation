##Following codes will analyse the reults we obtained from Non-Parametric approach

library(tidyverse)
library(gridExtra)
library(grid)
library(ggpubr)

#############################
   ##Gaussian Approach##
#############################

DF_MultiV_Gauss_Total <- read.csv("../Gaussian_approach/DF_MultiV_Total.csv")
### Total Hierarchy ###

DF_MultiV_Gauss_Total %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_AllTS

DF_MultScores_Gauss_AllTS %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_AllTS

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Gauss_AllTS %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_Gauss_AllTS_ETS

# DF_MultScores_Gauss_AllTS %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_Gauss_AllTS_ARIMA

#Calculate the skill scores 


#For ETS

DF_MultScores_Gauss_AllTS_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_AllTS_ETS 

DF_MultScores_Gauss_AllTS_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_AllTS_ETS 


DF_MultScores_Gauss_AllTS_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_AllTS_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_AllTS_ETS))*100, digits = 4)) -> DF_MultScore_SS_Gauss_AllTS_ETS

DF_MultScore_SS_Gauss_AllTS_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_Gauss_AllTS_ETS

DF_MultScore_SS_Gauss_AllTS_ETS %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_AllTS_ETS

# View(SS_E.ES_Gauss_AllTS_ETS)
# View(SS_E.VS_Gauss_AllTS_ETS)

SS_E.ES_Gauss_AllTS_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_ETS_ES

SS_E.VS_Gauss_AllTS_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_ETS_VS


### -- State level of the Hierarchy -- ###

DF_MultiV_Gauss_States <- read.csv("../Gaussian_approach/DF_MultiV_States.csv")

DF_MultiV_Gauss_States %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_States

DF_MultScores_Gauss_States %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_States

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Gauss_States %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_Gauss_States_ETS

# DF_MultScores_Gauss_States %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_Gauss_States_ARIMA

##-- Calculate the skill scores 

DF_MultScores_Gauss_States_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_States_ETS 

DF_MultScores_Gauss_States_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_States_ETS 


DF_MultScores_Gauss_States_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_States_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_States_ETS))*100, digits = 4)) -> DF_MultScore_SS_Gauss_States_ETS

DF_MultScore_SS_Gauss_States_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_States_ETS

DF_MultScore_SS_Gauss_States_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_States_ETS

# View(SS_E.ES_Gauss_States_ETS)
# View(SS_E.VS_Gauss_States_ETS)

SS_E.ES_Gauss_States_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ETS_ES

SS_E.VS_Gauss_States_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ETS_VS


### -- Zone level of the Hierarchy -- ###

DF_MultiV_Gauss_Zones <- read.csv("../Gaussian_approach/DF_MultiV_Zones.csv")

DF_MultiV_Gauss_Zones %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_Zones

DF_MultScores_Gauss_Zones %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_Zones

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Gauss_Zones %>% 
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
                `F.method`!="ARIMA") -> DF_MultScores_Gauss_Zones_ETS

# DF_MultScores_Gauss_Zones %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_Gauss_Zones_ARIMA

##--Calculate the skill scores 


DF_MultScores_Gauss_Zones_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Zones_ETS 

DF_MultScores_Gauss_Zones_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_Zones_ETS 


DF_MultScores_Gauss_Zones_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Zones_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Zones_ETS))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Zones_ETS

DF_MultScore_SS_Gauss_Zones_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_Zones_ETS

DF_MultScore_SS_Gauss_Zones_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_Zones_ETS

# View(SS_E.ES_Zones_ETS)
# View(SS_E.VS_Zones_ETS)

SS_E.ES_Gauss_Zones_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ETS_ES

SS_E.VS_Gauss_Zones_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ETS_VS


### -- Regional(Bottom) level of the Hierarchy -- ###

DF_MultiV_Gauss_Regions <- read.csv("../Gaussian_approach/DF_MultiV_Regions.csv")

DF_MultiV_Gauss_Regions %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_Regions

DF_MultScores_Gauss_Regions %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_Regions

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Gauss_Regions %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_Gauss_Regions_ETS

# DF_MultScores_Gauss_Regions %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_Gauss_Regions_ARIMA

#Calculate the skill scores 


#For ETS

DF_MultScores_Gauss_Regions_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Regions_ETS 

DF_MultScores_Gauss_Regions_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_Regions_ETS 


DF_MultScores_Gauss_Regions_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Regions_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Regions_ETS))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Regions_ETS

DF_MultScore_SS_Gauss_Regions_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_Regions_ETS

DF_MultScore_SS_Gauss_Regions_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_Regions_ETS

# View(SS_E.ES_Regions_ETS)
# View(SS_E.VS_Regions_ETS)

SS_E.ES_Gauss_Regions_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ETS_ES

SS_E.VS_Gauss_Regions_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ETS_VS


#############################
##Non-Parametric Approach##
#############################

DF_MultiV_NonPara_Total <- read.csv("../NonPara_approach/DF_MultiV_Total.csv")
### Total Hierarchy ###

DF_MultiV_NonPara_Total %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_AllTS

DF_MultScores_NonPara_AllTS %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_AllTS

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_NonPara_AllTS %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_NonPara_AllTS_ETS

# DF_MultScores_NonPara_AllTS %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_NonPara_AllTS_ARIMA

#Calculate the skill scores 


#For ARIMA

DF_MultScores_NonPara_AllTS_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_AllTS_ETS 

DF_MultScores_NonPara_AllTS_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_AllTS_ETS 


DF_MultScores_NonPara_AllTS_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_AllTS_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_AllTS_ETS))*100, digits = 4)) -> DF_MultScore_SS_NonPara_AllTS_ETS

DF_MultScore_SS_NonPara_AllTS_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_AllTS_ETS

DF_MultScore_SS_NonPara_AllTS_ETS %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_AllTS_ETS

# View(SS_E.ES_NonPara_AllTS_ETS)
# View(SS_E.VS_NonPara_AllTS_ETS)

SS_E.ES_NonPara_AllTS_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_ETS_ES

SS_E.VS_NonPara_AllTS_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_ETS_VS


### -- State level of the Hierarchy -- ###

DF_MultiV_NonPara_States <- read.csv("../NonPara_approach/DF_MultiV_States.csv")

DF_MultiV_NonPara_States %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_States

DF_MultScores_NonPara_States %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_States

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_NonPara_States %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_NonPara_States_ETS

# DF_MultScores_NonPara_States %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_NonPara_States_ARIMA

##-- Calculate the skill scores 

DF_MultScores_NonPara_States_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_States_ETS 

DF_MultScores_NonPara_States_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_States_ETS 


DF_MultScores_NonPara_States_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_States_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_States_ETS))*100, digits = 4)) -> DF_MultScore_SS_NonPara_States_ETS

DF_MultScore_SS_NonPara_States_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_NonPara_States_ETS

DF_MultScore_SS_NonPara_States_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_States_ETS

# View(SS_E.ES_NonPara_States_ETS)
# View(SS_E.VS_NonPara_States_ETS)

SS_E.ES_NonPara_States_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_ETS_ES

SS_E.VS_NonPara_States_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_ETS_VS


### -- Zone level of the Hierarchy -- ###

DF_MultiV_NonPara_Zones <- read.csv("../NonPara_approach/DF_MultiV_Zones.csv")

DF_MultiV_NonPara_Zones %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Zones

DF_MultScores_NonPara_Zones %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_Zones

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_NonPara_Zones %>% 
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
                `F.method`!="ARIMA") -> DF_MultScores_NonPara_Zones_ETS

# DF_MultScores_NonPara_Zones %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_NonPara_Zones_ARIMA

##--Calculate the skill scores 


DF_MultScores_NonPara_Zones_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Zones_ETS 

DF_MultScores_NonPara_Zones_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_Zones_ETS 


DF_MultScores_NonPara_Zones_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Zones_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Zones_ETS))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Zones_ETS

DF_MultScore_SS_NonPara_Zones_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_NonPara_Zones_ETS

DF_MultScore_SS_NonPara_Zones_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_Zones_ETS

# View(SS_E.ES_Zones_ETS)
# View(SS_E.VS_Zones_ETS)

SS_E.ES_NonPara_Zones_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_ETS_ES

SS_E.VS_NonPara_Zones_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_ETS_VS


### -- Regional(Bottom) level of the Hierarchy -- ###

DF_MultiV_NonPara_Regions <- read.csv("../NonPara_approach/DF_MultiV_Regions.csv")

DF_MultiV_NonPara_Regions %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Regions

DF_MultScores_NonPara_Regions %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_Regions

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultScores_NonPara_Regions %>%
  dplyr::filter(`F.method`=="ETS" | `R.method`=="Base",
                `F.method`!="ARIMA") -> DF_MultScores_NonPara_Regions_ETS

# DF_MultScores_NonPara_Regions %>% 
#   dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
#                 `F.method`!="ETS") -> DF_MultScores_NonPara_Regions_ARIMA

#Calculate the skill scores 


#For ETS

DF_MultScores_NonPara_Regions_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Regions_ETS 

DF_MultScores_NonPara_Regions_ETS %>% 
  filter(`F.method`=="ETS", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_Regions_ETS 


DF_MultScores_NonPara_Regions_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Regions_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Regions_ETS))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Regions_ETS

DF_MultScore_SS_NonPara_Regions_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_NonPara_Regions_ETS

DF_MultScore_SS_NonPara_Regions_ETS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_Regions_ETS

# View(SS_E.ES_Regions_ETS)
# View(SS_E.VS_Regions_ETS)

SS_E.ES_NonPara_Regions_ETS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_ETS_ES

SS_E.VS_NonPara_Regions_ETS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_ETS_VS

rm(list=ls()[! ls() %in% c("Plot_Gauss_AllTS_ETS_ES", "Plot_Gauss_AllTS_ETS_VS", 
                           "Plot_Gauss_States_ETS_ES", "Plot_Gauss_States_ETS_VS", 
                           "Plot_Gauss_Zones_ETS_ES", "Plot_Gauss_Zones_ETS_VS", 
                           "Plot_Gauss_Regions_ETS_ES", "Plot_Gauss_Regions_ETS_VS",
                           "Plot_NonPara_AllTS_ETS_ES", "Plot_NonPara_AllTS_ETS_VS", 
                           "Plot_NonPara_States_ETS_ES", "Plot_NonPara_States_ETS_VS", 
                           "Plot_NonPara_Zones_ETS_ES", "Plot_NonPara_Zones_ETS_VS", 
                           "Plot_NonPara_Regions_ETS_ES", "Plot_NonPara_Regions_ETS_VS")])


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(Plot_Gauss_AllTS_ETS_ES)

grid.arrange( arrangeGrob(Plot_Gauss_AllTS_ETS_ES + theme(legend.position="none"), 
                          Plot_Gauss_AllTS_ETS_VS + theme(legend.position="none"),
                          top="Gaussian Approach", ncol = 2), 
              ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_AllTS

grid.arrange( arrangeGrob(Plot_NonPara_AllTS_ETS_ES + theme(legend.position="none"), 
                          Plot_NonPara_AllTS_ETS_VS + theme(legend.position="none"),
                          top="Non-parametric Approach", ncol = 2), 
              ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_AllTS

grid.arrange(arrangeGrob(Plot_Gauss_AllTS, Plot_NonPara_AllTS), 
             ncol = 1, mylegend, heights=c(20, 1))


grid.arrange( arrangeGrob(Plot_Gauss_States_ETS_ES + theme(legend.position="none"), 
                          Plot_Gauss_Zones_ETS_ES + theme(legend.position="none"), 
                          Plot_Gauss_Regions_ETS_ES + theme(legend.position="none")), 
              arrangeGrob(Plot_Gauss_States_ETS_VS + theme(legend.position="none"), 
                          Plot_Gauss_Zones_ETS_VS + theme(legend.position="none"), 
                          Plot_Gauss_Regions_ETS_VS + theme(legend.position="none")), 
              ncol=2, heights=c(10, 0.5), top = "Gaussian Approach") -> Plot_Gauss_levels

grid.arrange( arrangeGrob(Plot_NonPara_States_ETS_ES + theme(legend.position="none"), 
                          Plot_NonPara_Zones_ETS_ES + theme(legend.position="none"), 
                          Plot_NonPara_Regions_ETS_ES + theme(legend.position="none")), 
              arrangeGrob(Plot_NonPara_States_ETS_VS + theme(legend.position="none"), 
                          Plot_NonPara_Zones_ETS_VS + theme(legend.position="none"), 
                          Plot_NonPara_Regions_ETS_VS + theme(legend.position="none")), 
              ncol=2, heights=c(10, 0.5), top = "Non-parametric Approach") -> Plot_NonPara_levels

grid.arrange(arrangeGrob(Plot_Gauss_levels, Plot_NonPara_levels), 
             ncol = 1, mylegend, heights = c(20,1))


###--Accuracy measure of univariate forecast distributions--##

rm(list = ls())

DF_UniV_Gauss_ETS <- read.csv("../Gaussian_approach/DF_UniV_ETS.csv")

DF_UniV_Gauss_ETS %>% 
  select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>% 
  group_by(Series, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_Gauss_AllTS

##-- Skill score for the Top level series

DF_UnivScores_Gauss_AllTS %>% 
  filter(Series=="Total", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>% 
  as_vector() -> Base_E.CRPS_Total_Gauss_ETS 


DF_UnivScores_Gauss_AllTS %>% 
  filter(Series == "Total") %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_Gauss_ETS))*100, 
                           digits = 4)) -> DF_UnivScore_SS_Gauss_Top.series

DF_UnivScore_SS_Gauss_Top.series %>% 
  ungroup() %>% 
  select(`R.method`, `Forecast.Horizon`, SS_E.CRPS) %>% 
  filter(`R.method` != "Base") %>% 
  mutate(`R.method` = recode(`R.method`, "MinT Shrink" = "MinT(Shrink)")) %>%
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`, h = `Forecast.Horizon`) %>% 
  ggplot(aes(x = h, y = SS_E.CRPS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (CRPS) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Total_Gauss_UniV

DF_UniV_NonPara_ETS <- read.csv("../NonPara_approach/DF_UniV_ETS.csv")

DF_UniV_NonPara_ETS %>% 
  select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>% 
  group_by(Series, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_NonPara_AllTS

##-- Skill score for the Top level series

DF_UnivScores_NonPara_AllTS %>% 
  filter(Series=="Total", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>% 
  as_vector() -> Base_E.CRPS_Total_NonPara_ETS 


DF_UnivScores_NonPara_AllTS %>% 
  filter(Series == "Total") %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_NonPara_ETS))*100, 
                           digits = 4)) -> DF_UnivScore_SS_NonPara_Top.series

DF_UnivScore_SS_NonPara_Top.series %>% 
  ungroup() %>% 
  select(`R.method`, `Forecast.Horizon`, SS_E.CRPS) %>% 
  filter(`R.method` != "Base") %>% 
  mutate(`R.method` = recode(`R.method`, "MinT Shrink" = "MinT(Shrink)")) %>%
  mutate(`R.method` = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom up"))) %>%
  rename(`Method`=`R.method`, h = `Forecast.Horizon`) %>% 
  ggplot(aes(x = h, y = SS_E.CRPS, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(15, 16, 17, 3)) +
  scale_x_discrete(limits = c(1:12)) +
  theme(legend.position = "bottom") +
  ylab("Skill score (CRPS) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Total_NonPara_UniV

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(Plot_Total_NonPara_UniV)


grid.arrange(arrangeGrob(Plot_Total_Gauss_UniV + theme(legend.position="none"), 
                         top = "Gaussian Approach"), heights=c(20, 1)) -> Plot_Univ1

grid.arrange(arrangeGrob(Plot_Total_NonPara_UniV + theme(legend.position="none"), 
                         top = "Non-parametric Approach"), heights=c(20, 1)) -> Plot_Univ2

grid.arrange(arrangeGrob(Plot_Univ1, Plot_Univ2, ncol = 2), 
             ncol = 1, mylegend, heights = c(20,1))
