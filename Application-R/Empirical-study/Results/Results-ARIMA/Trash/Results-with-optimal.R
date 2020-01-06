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
  dplyr::summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_AllTS

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_Gauss_AllTS %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_Gauss_AllTS_ETS

DF_MultScores_Gauss_AllTS %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
                `F.method`!="ETS") -> DF_MultScores_Gauss_AllTS_ARIMA

#Calculate the skill scores 


#For ARIMA

DF_MultScores_Gauss_AllTS_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_AllTS_ARIMA 

DF_MultScores_Gauss_AllTS_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_AllTS_ARIMA 


DF_MultScores_Gauss_AllTS_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_AllTS_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_AllTS_ARIMA

DF_MultScore_SS_Gauss_AllTS_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_Gauss_AllTS_ARIMA

DF_MultScore_SS_Gauss_AllTS_ARIMA %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_AllTS_ARIMA

# View(SS_E.ES_Gauss_AllTS_ARIMA)
# View(SS_E.VS_Gauss_AllTS_ARIMA)


SS_E.ES_Gauss_AllTS_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, 
                             levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_ARIMA_ES

SS_E.VS_Gauss_AllTS_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_ARIMA_VS


### -- State level of the Hierarchy -- ###

DF_MultiV_Gauss_States <- read.csv("../Gaussian_approach/DF_MultiV_States.csv")

DF_MultiV_Gauss_States %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_States

DF_MultScores_Gauss_States %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_States

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_Gauss_States %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_Gauss_States_ETS

DF_MultScores_Gauss_States %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
                `F.method`!="ETS") -> DF_MultScores_Gauss_States_ARIMA

##-- Calculate the skill scores 

DF_MultScores_Gauss_States_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_States_ARIMA 

DF_MultScores_Gauss_States_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_States_ARIMA 


DF_MultScores_Gauss_States_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_States_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_States_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_States_ARIMA

DF_MultScore_SS_Gauss_States_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_States_ARIMA

DF_MultScore_SS_Gauss_States_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_States_ARIMA

# View(SS_E.ES_Gauss_States_ARIMA)
# View(SS_E.VS_Gauss_States_ARIMA)

SS_E.ES_Gauss_States_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ARIMA_ES

SS_E.VS_Gauss_States_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ARIMA_VS


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

DF_MultScores_Gauss_Zones %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
                `F.method`!="ETS") -> DF_MultScores_Gauss_Zones_ARIMA

##--Calculate the skill scores 


DF_MultScores_Gauss_Zones_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Zones_ARIMA 

DF_MultScores_Gauss_Zones_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_Zones_ARIMA 



DF_MultScores_Gauss_Zones_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Zones_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Zones_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Zones_ARIMA

DF_MultScore_SS_Gauss_Zones_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_Zones_ARIMA

DF_MultScore_SS_Gauss_Zones_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_Zones_ARIMA

# View(SS_E.ES_Zones_ARIMA)
# View(SS_E.VS_Zones_ARIMA)

SS_E.ES_Gauss_Zones_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ARIMA_ES

SS_E.VS_Gauss_Zones_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ARIMA_VS


### -- Regional(Bottom) level of the Hierarchy -- ###

DF_MultiV_Gauss_Regions <- read.csv("../Gaussian_approach/DF_MultiV_Regions.csv")

DF_MultiV_Gauss_Regions %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_Regions

DF_MultScores_Gauss_Regions %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_Gauss_Regions

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_Gauss_Regions %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_Gauss_Regions_ETS

DF_MultScores_Gauss_Regions %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base", 
                `F.method`!="ETS") -> DF_MultScores_Gauss_Regions_ARIMA

#Calculate the skill scores 


#For ARIMA

DF_MultScores_Gauss_Regions_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Regions_ARIMA 

DF_MultScores_Gauss_Regions_ARIMA %>% 
  filter(`F.method`=="ARIMA", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Gauss_Regions_ARIMA 


DF_MultScores_Gauss_Regions_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Regions_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Regions_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Regions_ARIMA

DF_MultScore_SS_Gauss_Regions_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.ES_Gauss_Regions_ARIMA

DF_MultScore_SS_Gauss_Regions_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  filter(`R.method` != "Base") %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_Regions_ARIMA

# View(SS_E.ES_Regions_ARIMA)
# View(SS_E.VS_Regions_ARIMA)

SS_E.ES_Gauss_Regions_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ARIMA_ES

SS_E.VS_Gauss_Regions_ARIMA %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ARIMA_VS


#############################
##Non-Parametric Approach##
#############################

DF_MultiV_NonPara_Total <- read.csv("../NonPara_approach/With-Optimal/DF_MultiV_Total.csv")
### Total Hierarchy ###

DF_MultiV_NonPara_Total %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_AllTS

DF_MultScores_NonPara_AllTS %>% 
  select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_AllTS

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_NonPara_AllTS %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_NonPara_AllTS_ETS


#Calculate the skill scores 


#For ARIMA

DF_MultScores_NonPara_AllTS %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_AllTS 

DF_MultScores_NonPara_AllTS %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_AllTS 


DF_MultScores_NonPara_AllTS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_AllTS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_AllTS))*100, digits = 4)) -> DF_MultScore_SS_NonPara_AllTS

DF_MultScore_SS_NonPara_AllTS %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_AllTS

DF_MultScore_SS_NonPara_AllTS %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_AllTS

# View(SS_E.ES_NonPara_AllTS)
# View(SS_E.VS_NonPara_AllTS)

SS_E.ES_NonPara_AllTS %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, 
                             levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_ES

SS_E.VS_NonPara_AllTS %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_VS


### -- State level of the Hierarchy -- ###

DF_MultiV_NonPara_States <- read.csv("../NonPara_approach/With-Optimal/DF_MultiV_States.csv")
### States Hierarchy ###

DF_MultiV_NonPara_States %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_States

DF_MultScores_NonPara_States %>% 
  select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_States

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_NonPara_States %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_NonPara_States_ETS


#Calculate the skill scores 


#For ARIMA

DF_MultScores_NonPara_States %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_States 

DF_MultScores_NonPara_States %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_States 


DF_MultScores_NonPara_States %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_States))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_States))*100, digits = 4)) -> DF_MultScore_SS_NonPara_States

DF_MultScore_SS_NonPara_States %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_States

DF_MultScore_SS_NonPara_States %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_States

# View(SS_E.ES_NonPara_States)
# View(SS_E.VS_NonPara_States)

SS_E.ES_NonPara_States %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, 
                             levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_ES

SS_E.VS_NonPara_States %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_VS



### -- Zone level of the Hierarchy -- ###

DF_MultiV_NonPara_Zones <- read.csv("../NonPara_approach/With-Optimal/DF_MultiV_Zones.csv")
### Zones Hierarchy ###

DF_MultiV_NonPara_Zones %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Zones

DF_MultScores_NonPara_Zones %>% 
  select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_Zones

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_NonPara_Zones %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_NonPara_Zones_ETS


#Calculate the skill scores 


#For ARIMA

DF_MultScores_NonPara_Zones %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Zones 

DF_MultScores_NonPara_Zones %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_Zones 


DF_MultScores_NonPara_Zones %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Zones))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Zones))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Zones

DF_MultScore_SS_NonPara_Zones %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_Zones

DF_MultScore_SS_NonPara_Zones %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_Zones

# View(SS_E.ES_NonPara_Zones)
# View(SS_E.VS_NonPara_Zones)

SS_E.ES_NonPara_Zones %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, 
                             levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_ES

SS_E.VS_NonPara_Zones %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_VS



### -- Regional(Bottom) level of the Hierarchy -- ###

DF_MultiV_NonPara_Regions <- read.csv("../NonPara_approach/With-Optimal/DF_MultiV_Regions.csv")
### Regions Hierarchy ###

DF_MultiV_NonPara_Regions %>% 
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Regions

DF_MultScores_NonPara_Regions %>% 
  select(-`F.method`) %>% 
  group_by(`R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultScores_NonPara_Regions

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

# DF_MultScores_NonPara_Regions %>% 
#   dplyr::filter(`F.method`=="ETS" | `R.method`=="Base", 
#                 `F.method`!="ARIMA") -> DF_MultScores_NonPara_Regions_ETS


#Calculate the skill scores 


#For ARIMA

DF_MultScores_NonPara_Regions %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Regions 

DF_MultScores_NonPara_Regions %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_NonPara_Regions 


DF_MultScores_NonPara_Regions %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Regions))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Regions))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Regions

DF_MultScore_SS_NonPara_Regions %>%  
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_Regions

DF_MultScore_SS_NonPara_Regions %>% 
  ungroup() %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  filter(`R.method` != "Base") %>% 
  spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
  ungroup() %>% 
  mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_NonPara_Regions

# View(SS_E.ES_NonPara_Regions)
# View(SS_E.VS_NonPara_Regions)

SS_E.ES_NonPara_Regions %>% 
  gather(-`R.method`, key = "h", value = "ES") %>% 
  mutate(`R.method` = factor(`R.method`, 
                             levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_ES

SS_E.VS_NonPara_Regions %>% 
  gather(-`R.method`, key = "h", value = "VS") %>% 
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = VS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_VS



###--Accuracy measure of univariate forecast distributions--##

DF_UniV_Gauss_ARIMA <- read.csv("../Gaussian_approach/DF_UniV_ARIMA.csv")[,-1]

DF_UniV_Gauss_ARIMA %>% 
  dplyr::select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>% 
  group_by(Series, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_Gauss_AllTS

##-- Skill score for the Top level series

DF_UnivScores_Gauss_AllTS %>% 
  filter(Series=="Total", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>% 
  as_vector() -> Base_E.CRPS_Total_Gauss_ARIMA 


DF_UnivScores_Gauss_AllTS %>% 
  filter(Series == "Total") %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_Gauss_ARIMA))*100, 
                           digits = 4)) -> DF_UnivScore_SS_Gauss_Top.series

DF_UnivScore_SS_Gauss_Top.series %>% 
  ungroup() %>% 
  dplyr::select(`R.method`, `Forecast.Horizon`, SS_E.CRPS) %>% 
  filter(`R.method` != "Base") %>% 
  mutate(`R.method` = recode(`R.method`, "MinT Shrink" = "MinT(Shrink)")) %>%
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`, h = `Forecast.Horizon`) %>% 
  ggplot(aes(x = h, y = SS_E.CRPS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (CRPS) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Total_Gauss_UniV

DF_UniV_NonPara <- read.csv("../NonPara_approach/With-Optimal/DF_UniV.csv")

DF_UniV_NonPara %>% 
  dplyr::select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>% 
  group_by(Series, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_NonPara_AllTS

##-- Skill score for the Top level series

DF_UnivScores_NonPara_AllTS %>% 
  filter(Series=="Total", `R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>% 
  as_vector() -> Base_E.CRPS_Total_NonPara 


DF_UnivScores_NonPara_AllTS %>% 
  filter(Series == "Total") %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_NonPara))*100, 
                           digits = 4)) -> DF_UnivScore_SS_NonPara_Top.series

DF_UnivScore_SS_NonPara_Top.series %>% 
  ungroup() %>% 
  dplyr::select(`R.method`, `Forecast.Horizon`, SS_E.CRPS) %>% 
  filter(`R.method` != "Base") %>% 
  mutate(`R.method` = recode(`R.method`, "MinT Shrink" = "MinT(Shrink)")) %>%
  mutate(`R.method` = factor(`R.method`, levels = c("Bottom up", "OLS", "WLS", "MinT(Shrink)", "Optimal"))) %>%
  rename(`Method`=`R.method`, h = `Forecast.Horizon`) %>% 
  ggplot(aes(x = h, y = SS_E.CRPS, color = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  geom_line(aes(group = Method, color = Method)) +
  ggthemes::scale_color_colorblind() +
  scale_x_discrete(limits = c(1:12))+
  theme(legend.position = "bottom") +
  ylab("Skill score (CRPS) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Total_NonPara_UniV

rm(list=ls()[! ls() %in% c("Plot_Gauss_AllTS_ARIMA_ES", "Plot_Gauss_AllTS_ARIMA_VS", 
                           "Plot_Gauss_States_ARIMA_ES", "Plot_Gauss_States_ARIMA_VS", 
                           "Plot_Gauss_Zones_ARIMA_ES", "Plot_Gauss_Zones_ARIMA_VS", 
                           "Plot_Gauss_Regions_ARIMA_ES", "Plot_Gauss_Regions_ARIMA_VS",
                           "Plot_NonPara_AllTS_ES", "Plot_NonPara_AllTS_VS", 
                           "Plot_NonPara_States_ES", "Plot_NonPara_States_VS", 
                           "Plot_NonPara_Zones_ES", "Plot_NonPara_Zones_VS", 
                           "Plot_NonPara_Regions_ES", "Plot_NonPara_Regions_VS",
                           "Plot_Total_Gauss_UniV", "Plot_Total_NonPara_UniV")])


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(Plot_NonPara_AllTS_ES)

grid.arrange(arrangeGrob(Plot_Gauss_AllTS_ARIMA_ES + theme(legend.position="none"), 
                         top = "Gaussian Approach"), heights=c(20, 1)) -> Plot_AllTS_Gauss

grid.arrange(arrangeGrob(Plot_NonPara_AllTS_ES + theme(legend.position="none"), 
                         top = "Non-parametric Approach"), heights=c(20, 1)) -> Plot_AllTS_NonPara

grid.arrange(arrangeGrob(Plot_AllTS_Gauss, Plot_AllTS_NonPara, ncol = 2), 
             ncol = 1, mylegend, heights = c(20,1))


# grid.arrange( arrangeGrob(Plot_Gauss_AllTS_ARIMA_ES + theme(legend.position="none"), 
#                           Plot_Gauss_AllTS_ARIMA_VS + theme(legend.position="none"),
#                           top="Gaussian Approach", ncol = 2), 
#               ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_AllTS
# 
# grid.arrange( arrangeGrob(Plot_NonPara_AllTS_ARIMA_ES + theme(legend.position="none"), 
#                           Plot_NonPara_AllTS_ARIMA_VS + theme(legend.position="none"),
#                           top="Non-parametric Approach", ncol = 2), 
#               ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_AllTS
# 
# grid.arrange(arrangeGrob(Plot_Gauss_AllTS, Plot_NonPara_AllTS), 
#              ncol = 1, mylegend, heights=c(20, 1))


grid.arrange( arrangeGrob(Plot_Total_Gauss_UniV + theme(legend.position="none"),
                          Plot_Gauss_States_ARIMA_ES + theme(legend.position="none"), 
                          Plot_Gauss_Zones_ARIMA_ES + theme(legend.position="none"), 
                          Plot_Gauss_Regions_ARIMA_ES + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Gaussian Approach") -> Plot_Gauss_levels

grid.arrange( arrangeGrob(Plot_Total_NonPara_UniV + theme(legend.position="none"),
                          Plot_NonPara_States_ES + theme(legend.position="none"), 
                          Plot_NonPara_Zones_ES + theme(legend.position="none"), 
                          Plot_NonPara_Regions_ES + theme(legend.position="none"),
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Non-parametric Approach") -> Plot_NonPara_levels

grid.arrange(arrangeGrob(Plot_Gauss_levels, Plot_NonPara_levels, ncol = 2),
             ncol = 1, mylegend, heights = c(20,1))

################################

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
