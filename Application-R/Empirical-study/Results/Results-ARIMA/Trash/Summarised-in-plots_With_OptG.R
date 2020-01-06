
library(tidyverse)
library(gridExtra)
library(grid)
library(ggpubr)

#############################
   ##Gaussian Approach##
#############################

### Total Hierarchy ###

DF_MultiV_Gauss_Total <- read.csv("../Gaussian_approach/With-OptG/DF_MultiV_Total.csv")

DF_MultiV_Gauss_Total %>%
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_AllTS

DF_MultScores_Gauss_AllTS %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_Gauss_AllTS

DF_MultiV_NonPara_Total <- read.csv("../NonPara_approach/With-OptG/DF_MultiV_Total.csv")

DF_MultiV_NonPara_Total %>%
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_AllTS

DF_MultScores_NonPara_AllTS %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "Optimal" = "OptimalG")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_NonPara_AllTS



#### Ploting raw scores for ARIMA####


DF_MultScores_Gauss_AllTS %>%
  ungroup() %>%
  select(-E.VS, -F.method) %>%
  rename(Gauss_E.ES = E.ES) -> DF_MultScores_Gauss_AllTS_ES

DF_MultScores_NonPara_AllTS %>%
  ungroup() %>%
  select(-E.VS, -F.method) %>%
  rename(NonPara_E.ES = E.ES) -> DF_MultScores_NonPara_AllTS_ES

# DF_MultScores_AllTS <- left_join(DF_MultScores_Gauss_AllTS_ES, DF_MultScores_NonPara_AllTS_ES)
#
# DF_MultScores_AllTS %>%
#   rename("Parametric approach" = Gauss_E.ES,
#          "Non parametric approach" = NonPara_E.ES) %>%
#   gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>%
#   mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
#          R.method = recode(R.method, MinT.Shr = "MinT(Shrink)"),
#          R.method = factor(`R.method`, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
#   rename(`Method`=`R.method`) %>% 
#   ggplot(aes(x = Forecast.Horizon, y = Score, color = Method, shape = Method)) + 
#   geom_point(size = 2) + 
#   geom_line(aes(group = Method, color = Method)) +
#   facet_wrap(~Approach) +
#   theme(legend.position = "bottom") +
#   ggthemes::scale_color_colorblind() +
#   scale_shape_manual(values = c(0, 1, 2, 5, 6)) +
#   scale_x_discrete(limits = c(1:12)) +
#   ylab("Energy Score") +
#   xlab("h") +
#   theme(plot.title = element_text(size = 10, face = "italic")) +
#   theme(axis.title.y = element_text(size = 10))

### For ES ###

DF_MultScores_Gauss_AllTS %>%
  ungroup() %>% 
  select(-E.VS) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(480, 620), breaks = c(500, 525, 550, 575, 600)) +
  ylab("ES") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_ES

DF_MultScores_NonPara_AllTS %>%
  ungroup() %>% 
  select(-E.VS) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(480, 620), breaks = c(500, 525, 550, 575, 600)) +
  ylab("ES") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_ES

### For VS ###

DF_MultScores_Gauss_AllTS %>%
  ungroup() %>% 
  select(-E.ES) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.VS/10^3, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(22, 34), breaks = c(22, 24, 26, 28, 30, 32, 34)) +
  ylab( expression(paste("VS ", (x10^{3})))) +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_VS

DF_MultScores_NonPara_AllTS %>%
  ungroup() %>% 
  select(-E.ES) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.VS/10^3, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(22, 34), breaks = c(22, 24, 26, 28, 30, 32, 34)) +
  ylab( expression(paste("VS ", (x10^{3})))) +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_VS


g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
  }

mylegend <- g_legend(Plot_NonPara_AllTS_ES)

grid.arrange( arrangeGrob(Plot_Gauss_AllTS_ES + theme(legend.position="none"),
                          Plot_Gauss_AllTS_VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Analytical Approach") -> Plot_Gauss_AllTS

grid.arrange( arrangeGrob(Plot_NonPara_AllTS_ES + theme(legend.position="none"),
                          Plot_NonPara_AllTS_VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Sampling Approach") -> Plot_NonPara_AllTS

grid.arrange(arrangeGrob(Plot_Gauss_AllTS, Plot_NonPara_AllTS, ncol = 2),
             ncol = 1, mylegend, heights = c(20,1))

# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend <- g_legend(Plot_NonPara_AllTS_VS)
# 
# grid.arrange( arrangeGrob(Plot_Gauss_AllTS_ES + theme(legend.position="none"),
#                           Plot_Gauss_AllTS_VS + theme(legend.position="none"),
#                           ncol = 1),
#               ncol=1, heights=c(10, 0.5), top = "Parametric Approach") -> Plot_Gauss_AllTS
# 
# grid.arrange( arrangeGrob(Plot_NonPara_AllTS_ES + theme(legend.position="none"),
#                           Plot_NonPara_AllTS_VS + theme(legend.position="none"),
#                           ncol = 1),
#               ncol=1, heights=c(10, 0.5), top = "Non Parametric Approach") -> Plot_NonPara_AllTS




###--Skill-scores - ARIMA--###


DF_MultScores_Gauss_AllTS %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_AllTS_ARIMA

DF_MultScores_Gauss_AllTS %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_Gauss_AllTS_ARIMA

DF_MultScores_NonPara_AllTS %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_AllTS_ARIMA

DF_MultScores_NonPara_AllTS %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_NonPara_AllTS_ARIMA

DF_MultScores_Gauss_AllTS %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_AllTS_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_AllTS

DF_MultScores_NonPara_AllTS %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_AllTS_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_NonPara_AllTS



### For ES ###

DF_MultScore_SS_Gauss_AllTS %>%
  ungroup() %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-20, 10), breaks = c(-20, -15, -10, -5, 0, 5, 10)) +
  ylab("ES (%)") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_SS.ES


DF_MultScore_SS_NonPara_AllTS %>%
  ungroup() %>% 
  select(-E.VS) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-20, 10), breaks = c(-20, -15, -10, -5, 0, 5, 10)) +
  ylab("ES (%)") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_SS.ES


## For VS ##


DF_MultScore_SS_Gauss_AllTS %>%
  ungroup() %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-35, 10), breaks = c(-30, -20, -10, 0, 10)) +
  ylab("VS (%)") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_AllTS_SS.VS

DF_MultScore_SS_NonPara_AllTS %>%
  ungroup() %>% 
  select(-E.ES) %>% 
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"), 
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-35, 10), breaks = c(-30, -20, -10, 0, 10)) +
  ylab("VS (%)") +
  xlab("h") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_AllTS_SS.VS


mylegend <- g_legend(Plot_Gauss_AllTS_SS.ES)

grid.arrange( arrangeGrob(Plot_Gauss_AllTS_SS.ES + theme(legend.position="none"),
                          Plot_Gauss_AllTS_SS.VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Analytical Approach") -> Plot_Gauss_SS_AllTS

grid.arrange( arrangeGrob(Plot_NonPara_AllTS_SS.ES + theme(legend.position="none"),
                          Plot_NonPara_AllTS_SS.VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5), top = "Sampling Approach") -> Plot_NonPara_SS_AllTS

grid.arrange(arrangeGrob(Plot_Gauss_SS_AllTS, Plot_NonPara_SS_AllTS, ncol = 2),
             ncol = 1, mylegend, heights = c(20,1))


# ###--Skill-scores with reference to Base-Gaussian - ARIMA--###
# 
# 
# DF_MultScores_Gauss_AllTS %>% 
#   filter(`R.method`=="Incoherent Base") %>%
#   slice() %>%
#   ungroup() %>%
#   dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_AllTS_ARIMA 
# 
# DF_MultScores_Gauss_AllTS %>% 
#   filter(`R.method`=="Incoherent Base") %>%

#   slice() %>%
#   ungroup() %>%
#   dplyr::select(`E.VS`) %>%
#   as_vector() -> Base_E.VS_Gauss_AllTS_ARIMA
#
#
# DF_MultScores_Gauss_AllTS %>%
#   mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_AllTS_ARIMA))*100, digits = 4),
#          SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Gauss_AllTS
#
# DF_MultScores_NonPara_AllTS %>%
#   mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_AllTS_ARIMA))*100, digits = 4),
#          SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_NonPara_AllTS
#
#
#
# DF_MultScore_SS_Gauss_AllTS %>%
#   ungroup() %>%
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>%
#   ungroup() %>%
#   mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_Gauss_AllTS
#
# DF_MultScore_SS_NonPara_AllTS %>%
#   ungroup() %>%
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`, -`F.method`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) %>%
#   ungroup() %>%
#   mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)"))-> SS_E.ES_NonPara_AllTS

# DF_MultScore_SS_Gauss_AllTS_ARIMA %>%
#   ungroup() %>%
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`, -`F.method`) %>%
#   filter(`R.method` != "Incoherent Base") %>% 
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) %>% 
#   ungroup() %>% 
#   mutate(`R.method` = recode(`R.method`, `MinT.Shr` = "MinT(Shrink)")) -> SS_E.VS_Gauss_AllTS_ARIMA

# View(SS_E.ES_Gauss_AllTS_ARIMA)
# View(SS_E.VS_Gauss_AllTS_ARIMA)


### Plot type - 3

SS_E.ES_Gauss_AllTS %>% 
  filter(R.method %in% c("Incoherent Base", "Bottom-up", "MinT(Shrink)")) %>% 
  gather(-R.method, key = h, value = Gauss_ES.SS) -> SS_E.ES_Gauss_AllTS_1

SS_E.ES_NonPara_AllTS %>% 
  filter(R.method %in% c("Incoherent Base", "Bottom-up", "MinT(Shrink)")) %>% 
  gather(-R.method, key = h, value = NonPara_ES.SS) -> SS_E.ES_NonPara_AllTS_1

SS_E.ES_AllTS <- left_join(SS_E.ES_Gauss_AllTS_1, SS_E.ES_NonPara_AllTS_1)

SS_E.ES_AllTS %>% 
  rename("Parametric approach" = Gauss_ES.SS, 
         "Non parametric approach" = NonPara_ES.SS) %>% 
  gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>% 
  filter(Approach != "Parametric approach" | R.method != "Incoherent Base") %>% 
  mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
         R.method = recode(R.method, MinT.Shr = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("Incoherent Base", "Bottom-up", "MinT(Shrink)"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = h, y = Score, group = interaction(Approach, Method))) +
  geom_point(size = 2, aes(color = Method)) +
  geom_line(aes(linetype = Approach)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  ylab("Skill score (ES) %")


###--Plots for different levels--###

################
  #Top Level#
################

DF_UniV_Gauss_ARIMA <- read.csv("../Gaussian_approach/With-OptG/DF_UniV.csv")[,-1]

DF_UniV_Gauss_ARIMA %>%
  dplyr::select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>%
  group_by(Series, `R.method`, `Forecast.Horizon`) %>%
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_Gauss_AllTS

DF_UnivScores_Gauss_AllTS %>% 
  ungroup() %>% 
  filter(Series == "Total") %>% 
  mutate(R.method = recode(R.method, "MinT Shrink" = "MinT(Shrink)", 
                           "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base")) -> DF_UnivScores_Gauss_Top.level

DF_UniV_NonPara <- read.csv("../NonPara_approach/With-OptG/DF_UniV.csv")[,-1]

DF_UniV_NonPara %>%
  dplyr::select(Series, `R.method`, `Forecast.Horizon`, CRPS) %>%
  group_by(Series, `R.method`, `Forecast.Horizon`) %>%
  summarise(E.CRPS = mean(CRPS)) -> DF_UnivScores_NonPara_AllTS

DF_UnivScores_NonPara_AllTS %>% 
  ungroup() %>% 
  filter(Series == "Total") %>% 
  mutate(R.method = recode(R.method, "MinT Shrink" = "MinT(Shrink)", 
                           "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "Optimal" = "OptimalG")) -> DF_UnivScores_NonPara_Top.level


#### Raw Score plots ###

DF_UnivScores_Gauss_Top.level %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.CRPS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(275, 400), breaks = c(275,300, 325, 350, 375, 400)) +
  ylab("CRPS") +
  xlab("h") +
  ggtitle("Australia Total") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Top.level_CRPS

DF_UnivScores_NonPara_Top.level %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = E.CRPS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(275, 400), breaks = c(275,300, 325, 350, 375, 400)) +
  ylab("CRPS") +
  xlab("h") +
  ggtitle("Australia Total") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Top.level_CRPS


#### Skill Score plots ###

DF_UnivScores_Gauss_Top.level %>%
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>%
  as_vector() -> Base_E.CRPS_Total_Gauss_ARIMA

DF_UnivScores_NonPara_Top.level %>%
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.CRPS`) %>%
  as_vector() -> Base_E.CRPS_Total_NonPara_ARIMA

DF_UnivScores_Gauss_Top.level %>%
  filter(Series == "Total") %>%
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_Gauss_ARIMA))*100,
                           digits = 4)) -> DF_UnivScore_SS_Gauss_Top.series

DF_UnivScores_NonPara_Top.level %>%
  filter(Series == "Total") %>%
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_Total_NonPara_ARIMA))*100,
                           digits = 4)) -> DF_UnivScore_SS_NonPara_Top.series

DF_UnivScore_SS_Gauss_Top.series %>%
  ungroup() %>%
  select(-Series, -E.CRPS) -> DF_UnivScore_SS_Gauss_Top.series

DF_UnivScore_SS_NonPara_Top.series %>%
  ungroup() %>%
  select(-Series, -E.CRPS) -> DF_UnivScore_SS_NonPara_Top.series


DF_UnivScore_SS_Gauss_Top.series %>%
  filter(R.method %in% c("MinT(Shrink)", "WLS", "OptimalG", "OLS", "Bottom-up")) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = SS_E.CRPS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(-20, 10), breaks = c(-20, -10, 0, 10)) +
  ylab("CRPS (%)") +
  xlab("h") +
  ggtitle("Australia Total") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Top.level_CRPS.SS

DF_UnivScore_SS_NonPara_Top.series %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = SS_E.CRPS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(-20, 10), breaks = c(-20, -10, 0, 10)) +
  ylab("Skill Score (CRPS)") +
  xlab("h") +
  ggtitle("Australia Total") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Top.level_CRPS.SS


# SS_UnivS_Top.level <- left_join(DF_UnivScore_SS_Gauss_Top.series,
#                                 DF_UnivScore_SS_NonPara_Top.series)
# 
# 
# 
# SS_UnivS_Top.level %>% 
#   rename("Parametric approach" = Gauss_SS_E.CRPS, 
#          "Non parametric approach" = NonPara_SS_E.CRPS) %>% 
#   gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>% 
#   filter(Approach != "Parametric approach" | R.method != "Incoherent Base") %>% 
#   mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
#          R.method = factor(`R.method`, levels = c("Incoherent Base", "Bottom-up", "MinT(Shrink)"))) %>%
#   rename(`Method`=`R.method`) %>% 
#   ggplot(aes(x = Forecast.Horizon, y = Score, group = interaction(Approach, Method))) +
#   geom_point(size = 2, aes(color = Method)) +
#   geom_line(aes(linetype = Approach)) +
#   theme(legend.position = "bottom") +
#   ggthemes::scale_color_colorblind() +
#   scale_x_discrete(limits = c(1:12)) +
#   ylab("Skill score (CRPS) %") +
#   xlab("h")+
#   ggtitle("Top level") +
#   theme(plot.title = element_text(size = 10, face = "italic")) +
#   theme(axis.title.y = element_text(size = 10)) -> Plot_UniV_Top.level


#################
  #State Level#
#################


DF_MultiV_Gauss_States <- read.csv("../Gaussian_approach/With-OptG/DF_MultiV_States.csv")


DF_MultiV_Gauss_States %>%
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_States

DF_MultScores_Gauss_States %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_Gauss_States


DF_MultiV_NonPara_States <- read.csv("../NonPara_approach/With-OptG/DF_MultiV_States.csv")


DF_MultiV_NonPara_States %>%
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_States

DF_MultScores_NonPara_States %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)",
                           "Optimal" = "OptimalG")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_NonPara_States



#### Raw Score plots ###

# From Analytical solution
DF_MultScores_Gauss_States %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ES

DF_MultScores_Gauss_States %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_VS


# From Sampling solution

DF_MultScores_NonPara_States %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_ES

DF_MultScores_NonPara_States %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_VS


#### Skill Score plots ###


DF_MultScores_Gauss_States %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_States

DF_MultScores_Gauss_States %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_Gauss_States

DF_MultScores_NonPara_States %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_States

DF_MultScores_NonPara_States %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_NonPara_States

DF_MultScores_Gauss_States %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_States))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_States))*100, digits = 4)) -> DF_MultScore_SS_Gauss_States

DF_MultScores_NonPara_States %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_States))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_States))*100, digits = 4)) -> DF_MultScore_SS_NonPara_States


DF_MultScore_SS_Gauss_States %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(Gauss_SS_E.ES = SS_E.ES,
         Gauss_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_Gauss_States

DF_MultScore_SS_NonPara_States %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(NonPara_SS_E.ES = SS_E.ES, 
         NonPara_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_NonPara_States

# Analytical solution - plots

DF_MultScore_SS_subset_Gauss_States %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-25, 20), breaks = c(-20, -10, 0, 10, 20)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_ES.SS

DF_MultScore_SS_subset_Gauss_States %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-25, 20), breaks = c(-20, -10, 0, 10, 20)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_States_VS.SS

# Sampling solution - plots

DF_MultScore_SS_subset_NonPara_States %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-10, 20), breaks = c(-10, 0, 10, 20)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_ES.SS

DF_MultScore_SS_subset_NonPara_States %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-10, 20), breaks = c(-10, 0, 10, 20)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("States") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_States_VS.SS



# SS_MultiV_States <- left_join(DF_MultScore_SS_subset_Gauss_States,
#                               DF_MultScore_SS_subset_NonPara_States)

# SS_MultiV_States %>% 
#   rename("Parametric approach" = Gauss_SS_E.ES, 
#          "Non parametric approach" = NonPara_SS_E.ES) %>% 
#   gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>% 
#   filter(Approach != "Parametric approach" | R.method != "Incoherent Base") %>% 
#   mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
#          R.method = factor(`R.method`, levels = c("Incoherent Base", "Bottom-up", "MinT(Shrink)"))) %>%
#   rename(`Method`=`R.method`) %>% 
#   ggplot(aes(x = Forecast.Horizon, y = Score, group = interaction(Approach, Method))) +
#   geom_point(size = 2, aes(color = Method)) +
#   geom_line(aes(linetype = Approach)) +
#   theme(legend.position = "bottom") +
#   ggthemes::scale_color_colorblind() +
#   scale_x_discrete(limits = c(1:12)) +
#   ylab("Skill score (ES) %") +
#   xlab("h")+
#   ggtitle("States") +
#   theme(plot.title = element_text(size = 10, face = "italic")) +
#   theme(axis.title.y = element_text(size = 10)) -> Plot_Multiv_States



#################
  #Zones Level#
#################

DF_MultiV_Gauss_Zones <- read.csv("../Gaussian_approach/With-OptG/DF_MultiV_Zones.csv")


DF_MultiV_Gauss_Zones %>%
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_Zones

DF_MultScores_Gauss_Zones %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_Gauss_Zones


DF_MultiV_NonPara_Zones <- read.csv("../NonPara_approach/With-OptG/DF_MultiV_Zones.csv")


DF_MultiV_NonPara_Zones %>%
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Zones

DF_MultScores_NonPara_Zones %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)",
                           "Optimal" = "OptimalG")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_NonPara_Zones



#### Raw Score plots ###

# From Analytical solution

DF_MultScores_Gauss_Zones %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ES

DF_MultScores_Gauss_Zones %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_VS


# From Sampling solution

DF_MultScores_NonPara_Zones %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_ES

DF_MultScores_NonPara_Zones %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_VS


#### Skill Score plots ###


DF_MultScores_Gauss_Zones %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Zones

DF_MultScores_Gauss_Zones %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_Gauss_Zones

DF_MultScores_NonPara_Zones %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Zones

DF_MultScores_NonPara_Zones %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_NonPara_Zones

DF_MultScores_Gauss_Zones %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Zones))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Zones))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Zones

DF_MultScores_NonPara_Zones %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Zones))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Zones))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Zones


DF_MultScore_SS_Gauss_Zones %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(Gauss_SS_E.ES = SS_E.ES,
         Gauss_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_Gauss_Zones

DF_MultScore_SS_NonPara_Zones %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(NonPara_SS_E.ES = SS_E.ES, 
         NonPara_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_NonPara_Zones

# Analytical solution - plots

DF_MultScore_SS_subset_Gauss_Zones %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 10), breaks = c(-15, -10, -5, 0, 5, 10)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_ES.SS

DF_MultScore_SS_subset_Gauss_Zones %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 10), breaks = c(-15, -10, -5, 0, 5, 10)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Zones_VS.SS

# Sampling solution - plots

DF_MultScore_SS_subset_NonPara_Zones %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 12), breaks = c(-15, -10, -5, 0, 5, 10)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_ES.SS

DF_MultScore_SS_subset_NonPara_Zones %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 12), breaks = c(-15, -10, -5, 0, 5, 10)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("Zones") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Zones_VS.SS




# SS_MultiV_Zones <- left_join(DF_MultScore_SS_subset_Gauss_Zones,
#                               DF_MultScore_SS_subset_NonPara_Zones)

# SS_MultiV_Zones %>% 
#   rename("Parametric approach" = Gauss_SS_E.ES, 
#          "Non parametric approach" = NonPara_SS_E.ES) %>% 
#   gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>% 
#   filter(Approach != "Parametric approach" | R.method != "Incoherent Base") %>% 
#   mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
#          R.method = factor(`R.method`, levels = c("Incoherent Base", "Bottom-up", "MinT(Shrink)"))) %>%
#   rename(`Method`=`R.method`) %>% 
#   ggplot(aes(x = Forecast.Horizon, y = Score, group = interaction(Approach, Method))) +
#   geom_point(size = 2, aes(color = Method)) +
#   geom_line(aes(linetype = Approach)) +
#   theme(legend.position = "bottom") +
#   ggthemes::scale_color_colorblind() +
#   scale_x_discrete(limits = c(1:12)) +
#   ylab("Skill score (ES) %") +
#   xlab("h")+
#   ggtitle("Zones") +
#   theme(plot.title = element_text(size = 10, face = "italic")) +
#   theme(axis.title.y = element_text(size = 10)) -> Plot_Multiv_Zones



#################
  #Region Level#
#################

DF_MultiV_Gauss_Regions <- read.csv("../Gaussian_approach/With-OptG/DF_MultiV_Regions.csv")


DF_MultiV_Gauss_Regions %>%
  dplyr::select(-"Replication") -> DF_MultScores_Gauss_Regions

DF_MultScores_Gauss_Regions %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_Gauss_Regions


DF_MultiV_NonPara_Regions <- read.csv("../NonPara_approach/With-OptG/DF_MultiV_Regions.csv")


DF_MultiV_NonPara_Regions %>%
  dplyr::select(-"Replication") -> DF_MultScores_NonPara_Regions

DF_MultScores_NonPara_Regions %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) %>% 
  ungroup() %>% 
  mutate(R.method = recode(R.method, "Bottom up" = "Bottom-up", 
                           "Base" = "Incoherent Base",
                           "MinT.Shr" = "MinT(Shrink)",
                           "Optimal" = "OptimalG")) %>% 
  filter(F.method == "ARIMA") -> DF_MultScores_NonPara_Regions



#### Raw Score plots ###

# From Analytical solution

DF_MultScores_Gauss_Regions %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ES

DF_MultScores_Gauss_Regions %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_VS


# From Sampling solution

DF_MultScores_NonPara_Regions %>%
  ungroup() %>%
  select(-E.VS) %>%
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG",
                                                  "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.ES, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("ES") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_ES

DF_MultScores_NonPara_Regions %>%
  ungroup() %>%
  select(-E.ES) %>%
  mutate(R.method = recode(R.method, "MinT.Shr" = "MinT(Shrink)"),
         R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up", "Incoherent Base"))) %>%
  rename(`Method`=`R.method`) %>%
  ggplot(aes(x = Forecast.Horizon, y = E.VS, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_line(aes(group = Method, color = Method)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  # scale_y_continuous(limits = c(215, 265), breaks = c(220, 230, 240, 250, 260)) +
  ylab("VS") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_VS


#### Skill Score plots ###


DF_MultScores_Gauss_Regions %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Gauss_Regions

DF_MultScores_Gauss_Regions %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_Gauss_Regions

DF_MultScores_NonPara_Regions %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_NonPara_Regions

DF_MultScores_NonPara_Regions %>% 
  filter(`R.method`=="Incoherent Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>%
  as_vector() -> Base_E.VS_NonPara_Regions

DF_MultScores_Gauss_Regions %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Gauss_Regions))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Gauss_Regions))*100, digits = 4)) -> DF_MultScore_SS_Gauss_Regions

DF_MultScores_NonPara_Regions %>%
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_NonPara_Regions))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_NonPara_Regions))*100, digits = 4)) -> DF_MultScore_SS_NonPara_Regions


DF_MultScore_SS_Gauss_Regions %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(Gauss_SS_E.ES = SS_E.ES,
         Gauss_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_Gauss_Regions

DF_MultScore_SS_NonPara_Regions %>%
  ungroup() %>%
  select(R.method, Forecast.Horizon, SS_E.ES, SS_E.VS) %>%
  rename(NonPara_SS_E.ES = SS_E.ES, 
         NonPara_SS_E.VS = SS_E.VS) -> DF_MultScore_SS_subset_NonPara_Regions

# Analytical solution - plots

DF_MultScore_SS_subset_Gauss_Regions %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 5), breaks = c(-15, -10, -5, 0, 5)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_ES.SS

DF_MultScore_SS_subset_Gauss_Regions %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = Gauss_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-15, 5), breaks = c(-15, -10, -5, 0, 5)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_Gauss_Regions_VS.SS

# Sampling solution - plots

DF_MultScore_SS_subset_NonPara_Regions %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.ES, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-17, 5), breaks = c(-15, -10, -5, 0, 5)) +
  ylab("ES (%)") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_ES.SS

DF_MultScore_SS_subset_NonPara_Regions %>% 
  filter(R.method %in% c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up")) %>% 
  mutate(R.method = factor(`R.method`, levels = c("MinT(Shrink)", "OptimalG", "WLS", "OLS", "Bottom-up"))) %>%
  rename(`Method`=`R.method`) %>% 
  ggplot(aes(x = Forecast.Horizon, y = NonPara_SS_E.VS, color = Method, shape = Method)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = Method, color = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind() +
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 7)) +
  scale_x_discrete(limits = c(1:12)) +
  scale_y_continuous(limits = c(-17, 5), breaks = c(-15, -10, -5, 0, 5)) +
  ylab("VS (%)") +
  xlab("h") +
  ggtitle("Regions") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_NonPara_Regions_VS.SS




# SS_MultiV_Regions <- left_join(DF_MultScore_SS_subset_Gauss_Regions,
#                               DF_MultScore_SS_subset_NonPara_Regions)

# SS_MultiV_Regions %>% 
#   rename("Parametric approach" = Gauss_SS_E.ES, 
#          "Non parametric approach" = NonPara_SS_E.ES) %>% 
#   gather(`Parametric approach`, `Non parametric approach`, key = Approach, value = Score) %>% 
#   filter(Approach != "Parametric approach" | R.method != "Incoherent Base") %>% 
#   mutate(Approach = factor(Approach, levels = c("Parametric approach", "Non parametric approach")),
#          R.method = factor(`R.method`, levels = c("Incoherent Base", "Bottom-up", "MinT(Shrink)"))) %>%
#   rename(`Method`=`R.method`) %>% 
#   ggplot(aes(x = Forecast.Horizon, y = Score, group = interaction(Approach, Method))) +
#   geom_point(size = 2, aes(color = Method)) +
#   geom_line(aes(linetype = Approach)) +
#   theme(legend.position = "bottom") +
#   ggthemes::scale_color_colorblind() +
#   scale_x_discrete(limits = c(1:12)) +
#   ylab("Skill score (ES) %") +
#   xlab("h")+
#   ggtitle("Regions") +
#   theme(plot.title = element_text(size = 10, face = "italic")) +
#   theme(axis.title.y = element_text(size = 10)) -> Plot_Multiv_Regions


#################################
  # RawScore_Analytical_Levels
#################################


mylegend <- g_legend(Plot_Gauss_Regions_ES)

grid.arrange( arrangeGrob(Plot_Gauss_Top.level_CRPS + theme(legend.position="none"),
                          Plot_Gauss_States_ES + theme(legend.position="none"), 
                          Plot_Gauss_Zones_ES + theme(legend.position="none"), 
                          Plot_Gauss_Regions_ES + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_levels_ES
blank <- grid.rect(gp=gpar(col="white"))
grid.arrange( arrangeGrob(blank,
                          Plot_Gauss_States_VS + theme(legend.position="none"), 
                          Plot_Gauss_Zones_VS + theme(legend.position="none"), 
                          Plot_Gauss_Regions_VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_levels_VS

grid.arrange(arrangeGrob(Plot_Gauss_levels_ES, Plot_Gauss_levels_VS, ncol = 2),
             ncol = 1, heights = c(30,1), mylegend) + theme(legend.position = "bottom")


#############################
 # RawScore_Sampling_Levels
#############################


mylegend <- g_legend(Plot_NonPara_Regions_ES)

grid.arrange( arrangeGrob(Plot_NonPara_Top.level_CRPS + theme(legend.position="none"),
                          Plot_NonPara_States_ES + theme(legend.position="none"), 
                          Plot_NonPara_Zones_ES + theme(legend.position="none"), 
                          Plot_NonPara_Regions_ES + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_levels_ES
blank <- grid.rect(gp=gpar(col="white"))
grid.arrange( arrangeGrob(blank,
                          Plot_NonPara_States_VS + theme(legend.position="none"), 
                          Plot_NonPara_Zones_VS + theme(legend.position="none"), 
                          Plot_NonPara_Regions_VS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_levels_VS

grid.arrange(arrangeGrob(Plot_NonPara_levels_ES, Plot_NonPara_levels_VS, ncol = 2),
             ncol = 1, heights = c(30,1), mylegend) + theme(legend.position = "bottom")



##################################
  #SkillScore-Analytical-Levels
##################################

mylegend <- g_legend(Plot_Gauss_States_ES.SS)

grid.arrange( arrangeGrob(Plot_Gauss_Top.level_CRPS.SS + theme(legend.position="none"),
                          Plot_Gauss_States_ES.SS + theme(legend.position="none"), 
                          Plot_Gauss_Zones_ES.SS + theme(legend.position="none"), 
                          Plot_Gauss_Regions_ES.SS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_levels_ES.SS

grid.arrange( arrangeGrob(blank,
                          Plot_Gauss_States_VS.SS + theme(legend.position="none"), 
                          Plot_Gauss_Zones_VS.SS + theme(legend.position="none"), 
                          Plot_Gauss_Regions_VS.SS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_Gauss_levels_VS.SS

grid.arrange(arrangeGrob(Plot_Gauss_levels_ES.SS, Plot_Gauss_levels_VS.SS, ncol = 2),
             ncol = 1, mylegend, heights = c(30,1))



##################################
  #SkillScore-Sampling-Levels
##################################

mylegend <- g_legend(Plot_NonPara_States_ES.SS)

grid.arrange( arrangeGrob(Plot_NonPara_Top.level_CRPS.SS + theme(legend.position="none"),
                          Plot_NonPara_States_ES.SS + theme(legend.position="none"), 
                          Plot_NonPara_Zones_ES.SS + theme(legend.position="none"), 
                          Plot_NonPara_Regions_ES.SS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_levels_ES.SS

grid.arrange( arrangeGrob(blank,
                          Plot_NonPara_States_VS.SS + theme(legend.position="none"), 
                          Plot_NonPara_Zones_VS.SS + theme(legend.position="none"), 
                          Plot_NonPara_Regions_VS.SS + theme(legend.position="none"), 
                          ncol = 1), 
              ncol=1, heights=c(10, 0.5)) -> Plot_NonPara_levels_VS.SS

grid.arrange(arrangeGrob(Plot_NonPara_levels_ES.SS, Plot_NonPara_levels_VS.SS, ncol = 2),
             ncol = 1, mylegend, heights = c(30,1))

