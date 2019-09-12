#Method 1: Estimating W
#Method 2: Estimating through the reparameterisation using cholesky decomposition
#Method 3: Estimating P directly, imposing the constraint PS=I


library(tidyverse)
library(dplyr)



DF_MultiV_1_100 <- read.csv("Results/DF_MultiV_1-100.csv")[,-1]
DF_MultiV_101_200 <- read.csv("Results/DF_MultiV_101-200.csv")[,-1]
DF_MultiV_201_300 <- read.csv("Results/DF_MultiV_201-300.csv")[,-1]
DF_MultiV_301_400 <- read.csv("Results/DF_MultiV_301-400.csv")[,-1]
DF_MultiV_401_500 <- read.csv("Results/DF_MultiV_401-500.csv")[,-1]
DF_MultiV_501_600 <- read.csv("Results/DF_MultiV_501-600.csv")[,-1]
DF_MultiV_601_700 <- read.csv("Results/DF_MultiV_601-700.csv")[,-1]
DF_MultiV_701_800 <- read.csv("Results/DF_MultiV_701-800.csv")[,-1]
DF_MultiV_801_900 <- read.csv("Results/DF_MultiV_801-900.csv")[,-1]
DF_MultiV_901_1000 <- read.csv("Results/DF_MultiV_901-1000.csv")[,-1]

rbind(DF_MultiV_1_100, DF_MultiV_101_200, DF_MultiV_201_300, DF_MultiV_301_400, 
      DF_MultiV_401_500, DF_MultiV_501_600, DF_MultiV_601_700, DF_MultiV_701_800,
      DF_MultiV_801_900, DF_MultiV_901_1000) %>% 
  as.data.frame() -> DF_MultiV

DF_MultiV %>% dplyr::select(-"Replication") -> DF_MultScores

DF_MultScores %>% dplyr::group_by(`Forecast.Horizon`, `R.method`) %>%
 dplyr::summarise(E.ES = mean(`Energy.score`),
                  E.VS = mean(`Variogram.score`)) -> DF_MultScores

DF_MultScores %>% filter(Forecast.Horizon == 1)
DF_MultScores %>% filter(Forecast.Horizon == 2)
DF_MultScores %>% filter(Forecast.Horizon == 3)


DF_MultiV %>% 
  select(-Variogram.score) %>% 
  filter(Forecast.Horizon == 1) %>% 
  spread(key = R.method, value = Energy.score) -> ES_dataframe_h1

DF_MultiV %>% 
  select(-Variogram.score) %>% 
  filter(Forecast.Horizon == 2) %>% 
  spread(key = R.method, value = Energy.score) -> ES_dataframe_h2

DF_MultiV %>% 
  select(-Variogram.score) %>% 
  filter(Forecast.Horizon == 3) %>% 
  spread(key = R.method, value = Energy.score) -> ES_dataframe_h3


wilcox.test(ES_dataframe_h1$Base, ES_dataframe_h1$`Opt-G-Method-1`)

wilcox.test(ES_dataframe_h1$`MinT(Shrink)`, ES_dataframe_h1$`Opt-G-Method-1`)
