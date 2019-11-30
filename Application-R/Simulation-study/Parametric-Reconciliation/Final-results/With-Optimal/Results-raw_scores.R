---
  title: "Parametric Reconciliation"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
library(tidyverse)
library(kableExtra)
```

# Gaussian DGP 
```{r echo=FALSE}
DF_MultiV_Full_GausDGP <- read.csv("DF_MultiV_Full_GaussianDGP.csv")[,-1]


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


# DF_MultScore_SS_AllTS_GausDGP %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_GausDGP
# 
# DF_MultScore_SS_AllTS_GausDGP %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_GausDGP

# View(SS_E.ES_AllTS_GausDGP)
# View(SS_E.VS_AllTS_GausDGP)



########################################
### Bottom level of the Hierarchy ###
########################################



DF_MultiV_Bot_GausDGP <- read.csv("DF_MultiV_Bot_GaussianDGP.csv")[,-1]


DF_MultiV_Bot_GausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Log.score`)) -> DF_MultiV_Bot_GausDGP

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
  spread(key = temp, value = value) %>% 
  dplyr::select(R.method, `1_SS_E.LS`, `2_SS_E.LS`, `3_SS_E.LS`, 
                `1_SS_E.ES`, `2_SS_E.ES`, `3_SS_E.ES`, 
                `1_SS_E.VS`, `2_SS_E.VS`, `3_SS_E.VS`) %>% 
  rename("1" = `1_SS_E.LS`, "2" = `2_SS_E.LS`, "3" = `3_SS_E.LS`, 
         "1" = `1_SS_E.ES`, "2" = `2_SS_E.ES`, "3" = `3_SS_E.ES`, 
         "1" = `1_SS_E.VS`, "2" = `2_SS_E.VS`, "3" = `3_SS_E.VS`)%>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Log Score(%)" = 3, "Energy Score(%)" = 3, "Variogram Score(%)" = 3))

```

# Non Gaussian DGP

```{r echo=FALSE}
DF_MultiV_Full_NonGausDGP <- read.csv("DF_MultiV_Full_NonGaussianDGP.csv")[,-1]


DF_MultiV_Full_NonGausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.ES = mean(`Energy.score`), 
            E.VS = mean(`Variogram.score`)) -> DF_MultiV_Full_NonGausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Full_NonGausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Bottom up") -> DF_MultScores_AllTS_NonGausDGP

##--Calculate the skill scores--# 



##--For ARIMA--##

DF_MultScores_AllTS_NonGausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> BU_E.ES_AllTS_NonGausDGP 

DF_MultScores_AllTS_NonGausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> BU_E.VS_AllTS_NonGausDGP  


DF_MultScores_AllTS_NonGausDGP %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/BU_E.ES_AllTS_NonGausDGP))*100, digits = 2),
         SS_E.VS = round((1-(`E.VS`/BU_E.VS_AllTS_NonGausDGP))*100, digits = 2)) -> DF_MultScore_SS_AllTS_NonGausDGP

DF_MultScore_SS_AllTS_NonGausDGP %>%  
  dplyr::select(-`E.ES`, -`E.VS`) -> DF_MultScore_SS_AllTS_NonGausDGP


# DF_MultScore_SS_AllTS_NonGausDGP %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_NonGausDGP
# 
# DF_MultScore_SS_AllTS_NonGausDGP %>%  
#   dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_NonGausDGP

# View(SS_E.ES_AllTS_NonGausDGP)
# View(SS_E.VS_AllTS_NonGausDGP)



########################################
### Bottom level of the Hierarchy ###
########################################



DF_MultiV_Bot_NonGausDGP <- read.csv("DF_MultiV_Bot_NonGaussianDGP.csv")[,-1]


DF_MultiV_Bot_NonGausDGP %>% 
  group_by(`F.method`, `R.method`, `Forecast.Horizon`) %>% 
  summarise(E.LS = mean(`Log.score`)) -> DF_MultiV_Bot_NonGausDGP

#DF_MultScores %>% dplyr::filter(`R.method` != "Base") -> DF_MultScore_Recon

DF_MultiV_Bot_NonGausDGP %>% 
  dplyr::filter(`F.method`=="ARIMA" | `R.method`=="Base") -> DF_MultScores_BotTS_NonGausDGP

##--Calculate the skill scores--# 

DF_MultScores_BotTS_NonGausDGP %>% 
  filter(`R.method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% 
  as_vector() -> BU_E.LS_BotTS_NonGausDGP 

DF_MultScores_BotTS_NonGausDGP %>% 
  mutate(SS_E.LS = round((1-(`E.LS`/BU_E.LS_BotTS_NonGausDGP))*100, digits = 2)) -> DF_MultScore_SS_BotTS_NonGausDGP

# DF_MultScore_SS_BotTS_NonGausDGP %>%  
#   dplyr::select(-`E.LS`) %>%
#   spread(key = `Forecast.Horizon`, value = `SS_E.LS`) -> SS_E.LS_BotTS_NonGausDGP

# View(SS_E.LS_BotTS_NonGausDGP)


DF_MultScore_SS_BotTS_NonGausDGP %>% 
  ungroup() %>% 
  pull(SS_E.LS) -> SS_E.LS

DF_MultScore_SS_AllTS_NonGausDGP %>% 
  ungroup() %>% 
  add_column(SS_E.LS = SS_E.LS) -> SkillScore_full_hier

SkillScore_full_hier %>% 
  gather(key = key, value = value, SS_E.ES, SS_E.VS, SS_E.LS) %>% 
  unite(temp, Forecast.Horizon, key) %>% 
  spread(key = temp, value = value) %>% 
  dplyr::select(R.method, `1_SS_E.LS`, `2_SS_E.LS`, `3_SS_E.LS`, 
                `1_SS_E.ES`, `2_SS_E.ES`, `3_SS_E.ES`, 
                `1_SS_E.VS`, `2_SS_E.VS`, `3_SS_E.VS`) %>% 
  rename("1" = `1_SS_E.LS`, "2" = `2_SS_E.LS`, "3" = `3_SS_E.LS`, 
         "1" = `1_SS_E.ES`, "2" = `2_SS_E.ES`, "3" = `3_SS_E.ES`, 
         "1" = `1_SS_E.VS`, "2" = `2_SS_E.VS`, "3" = `3_SS_E.VS`)%>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Log Score(%)" = 3, "Energy Score(%)" = 3, "Variogram Score(%)" = 3))

```


# Comparing univariate predictive accuracy in aggregate levels
```{r echo=FALSE}
DF_UniV_GausDGP <- read.csv("DF_UniV_GaussianDGP.csv")[-1]

DF_UniV_GausDGP %>% 
  dplyr::select(-F.method) %>% 
  group_by(Series, R.method , `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS), 
            E.LS = mean(LS)) -> DF_Mean_UnivScore_GausDGP

DF_Mean_UnivScore_GausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(Series, `E.CRPS`, Forecast.Horizon) %>% 
  spread(key = Series, value = E.CRPS) %>%
  slice(rep(1:n(), 6)) %>% 
  gather(-Forecast.Horizon, key = Series, value = E.CRPS) %>% 
  pull(E.CRPS) -> Base_E.CRPS_BotTS_GausDGP 

DF_Mean_UnivScore_GausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(Series, `E.LS`, Forecast.Horizon) %>% 
  spread(key = Series, value = E.LS) %>%
  slice(rep(1:n(), 6)) %>% 
  gather(-Forecast.Horizon, key = Series, value = E.LS) %>% 
  pull(E.LS) -> Base_E.LS_BotTS_GausDGP 


DF_Mean_UnivScore_GausDGP %>% 
  ungroup() %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_BotTS_GausDGP))*100, digits = 2),
         SS_E.LS = round((1-(`E.LS`/Base_E.LS_BotTS_GausDGP))*100, digits = 2)) %>% 
  mutate(Series = recode(Series, V1 = "AA", V2 = "AB", V3 = "BA", V4 = "BB")) -> DF_Mean_UnivScore_SS_GausDGP

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 1) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_GausDGP_h1

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 2) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_GausDGP_h2

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 3) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_GausDGP_h3

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 1) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_GausDGP_h1

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 2) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_GausDGP_h2

DF_Mean_UnivScore_SS_GausDGP %>% 
  filter(Forecast.Horizon == 3) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_GausDGP_h3



```




```{r echo=FALSE}
DF_UniV_NonGausDGP <- read.csv("DF_UniV_NonGaussianDGP.csv")[-1]

DF_UniV_NonGausDGP %>% 
  dplyr::select(-F.method) %>% 
  group_by(Series, R.method , `Forecast.Horizon`) %>% 
  summarise(E.CRPS = mean(CRPS), 
            E.LS = mean(LS)) -> DF_Mean_UnivScore_NonGausDGP

DF_Mean_UnivScore_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(Series, `E.CRPS`, Forecast.Horizon) %>% 
  spread(key = Series, value = E.CRPS) %>%
  slice(rep(1:n(), 6)) %>% 
  gather(-Forecast.Horizon, key = Series, value = E.CRPS) %>% 
  pull(E.CRPS) -> Base_E.CRPS_BotTS_NonGausDGP 

DF_Mean_UnivScore_NonGausDGP %>% 
  filter(`R.method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(Series, `E.LS`, Forecast.Horizon) %>% 
  spread(key = Series, value = E.LS) %>%
  slice(rep(1:n(), 6)) %>% 
  gather(-Forecast.Horizon, key = Series, value = E.LS) %>% 
  pull(E.LS) -> Base_E.LS_BotTS_NonGausDGP 


DF_Mean_UnivScore_NonGausDGP %>% 
  ungroup() %>% 
  mutate(SS_E.CRPS = round((1-(`E.CRPS`/Base_E.CRPS_BotTS_NonGausDGP))*100, digits = 2),
         SS_E.LS = round((1-(`E.LS`/Base_E.LS_BotTS_NonGausDGP))*100, digits = 2)) %>% 
  mutate(Series = recode(Series, V1 = "AA", V2 = "AB", V3 = "BA", V4 = "BB"))-> DF_Mean_UnivScore_SS_NonGausDGP

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 1) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_NonGausDGP_h1

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 2) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_NonGausDGP_h2

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 3) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.CRPS) %>% 
  spread(key = Series, value = SS_E.LS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.LS_NonGausDGP_h3

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 1) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h1

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 2) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h2

DF_Mean_UnivScore_SS_NonGausDGP %>% 
  filter(Forecast.Horizon == 3) %>% 
  dplyr::select(-E.CRPS, -E.LS, -Forecast.Horizon, -SS_E.LS) %>% 
  spread(key = Series, value = SS_E.CRPS) %>% 
  dplyr::select(R.method, Total, A, B, AA, AB, BA, BB) -> DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h3


```

# For h=1

```{r echo=FALSE}
left_join(DF_Mean_UnivScore_SS.LS_GausDGP_h1,
          DF_Mean_UnivScore_SS.LS_NonGausDGP_h1, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

left_join(DF_Mean_UnivScore_SS.CRPS_GausDGP_h1,
          DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h1, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

```


# For h=2

```{r echo=FALSE}
left_join(DF_Mean_UnivScore_SS.LS_GausDGP_h2,
          DF_Mean_UnivScore_SS.LS_NonGausDGP_h2, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

left_join(DF_Mean_UnivScore_SS.CRPS_GausDGP_h2,
          DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h2, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

```

# For h=3

```{r echo=FALSE}
left_join(DF_Mean_UnivScore_SS.LS_GausDGP_h3,
          DF_Mean_UnivScore_SS.LS_NonGausDGP_h3, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

left_join(DF_Mean_UnivScore_SS.CRPS_GausDGP_h3,
          DF_Mean_UnivScore_SS.CRPS_NonGausDGP_h3, by = "R.method") %>%
  as_tibble() %>% 
  kable(format = "latex") %>% kable_styling("striped") %>% 
  kableExtra::add_header_above(c(" " = 1, "Gaussian DGP" = 7, 
                                 "Non-Gaussian DGP" = 7))

```