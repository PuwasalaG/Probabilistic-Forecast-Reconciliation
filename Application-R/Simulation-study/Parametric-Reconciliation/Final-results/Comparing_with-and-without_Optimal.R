library(tidyverse)
library(kableExtra)


DF_MultiV_Full_GausDGP_Opt <- read.csv("With-Optimal/DF_MultiV_Full_GaussianDGP.csv")[,-1]


DF_MultiV_Full_GausDGP_Opt %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = round(mean(`Energy.score`), digits = 2), 
            E.VS = round(mean(`Variogram.score`), digits = 2)) -> DF_MultiV_Full_GausDGP_Opt


DF_MultiV_Bot_GausDGP_Opt <- read.csv("With-Optimal/DF_MultiV_Bot_GaussianDGP.csv")[,-1]


DF_MultiV_Bot_GausDGP_Opt %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = round(mean(`Log.score`), digits = 2)) -> DF_MultiV_Bot_GausDGP_Opt

DF_MultiV_Bot_GausDGP_Opt %>% 
  left_join(DF_MultiV_Full_GausDGP_Opt) %>% 
  ungroup() %>% 
  mutate(R.method = factor(R.method, 
                           levels = c("Base", "Bottom up", "OLS", "WLS", 
                                      "MinT.Sam", "MinT.Shr", "Optimal"))) -> DF_MultiV_GaussDGP_Opt

DF_MultiV_GaussDGP_Opt %>% 
  ungroup() %>% 
  gather(key = key, value = value, E.LS, E.ES, E.VS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% 
  dplyr::select(R.method, `1_E.LS`, `2_E.LS`, `3_E.LS`, 
                `1_E.ES`, `2_E.ES`, `3_E.ES`, 
                `1_E.VS`, `2_E.VS`, `3_E.VS`) %>% 
  rename("1" = `1_E.LS`, "2" = `2_E.LS`, "3" = `3_E.LS`, 
         "1" = `1_E.ES`, "2" = `2_E.ES`, "3" = `3_E.ES`, 
         "1" = `1_E.VS`, "2" = `2_E.VS`, "3" = `3_E.VS`)%>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Log Score(%)" = 3, 
                                 "Energy Score(%)" = 3, "Variogram Score(%)" = 3))



### Withou optimal ###

DF_MultiV_Full_GausDGP <- read.csv("Without-Optimal/DF_MultiV_Full_GaussianDGP.csv")[,-1]


DF_MultiV_Full_GausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = round(mean(`Energy.score`), digits = 2), 
            E.VS = round(mean(`Variogram.score`), digits = 2)) -> DF_MultiV_Full_GausDGP


DF_MultiV_Bot_GausDGP <- read.csv("Without-Optimal/DF_MultiV_Bot_GaussianDGP.csv")[,-1]


DF_MultiV_Bot_GausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = round(mean(`Log.score`), digits = 2)) -> DF_MultiV_Bot_GausDGP

DF_MultiV_Bot_GausDGP %>% 
  left_join(DF_MultiV_Full_GausDGP) %>% 
  ungroup() %>% 
  mutate(R.method = factor(R.method, 
                           levels = c("Base", "Bottom up", "OLS", "WLS", 
                                      "MinT.Sam", "MinT.Shr", "Optimal"))) -> DF_MultiV_GaussDGP

DF_MultiV_GaussDGP %>% 
  ungroup() %>% 
  gather(key = key, value = value, E.LS, E.ES, E.VS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% 
  dplyr::select(R.method, `1_E.LS`, `2_E.LS`, `3_E.LS`, 
                `1_E.ES`, `2_E.ES`, `3_E.ES`, 
                `1_E.VS`, `2_E.VS`, `3_E.VS`) %>% 
  rename("1" = `1_E.LS`, "2" = `2_E.LS`, "3" = `3_E.LS`, 
         "1" = `1_E.ES`, "2" = `2_E.ES`, "3" = `3_E.ES`, 
         "1" = `1_E.VS`, "2" = `2_E.VS`, "3" = `3_E.VS`)%>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Log Score(%)" = 3, 
                                 "Energy Score(%)" = 3, "Variogram Score(%)" = 3))











