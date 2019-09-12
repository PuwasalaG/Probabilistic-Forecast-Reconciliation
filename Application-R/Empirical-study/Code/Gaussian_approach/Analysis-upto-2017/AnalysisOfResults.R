##Following codes will analyse the reults we obtained from Non-Parametric approach

### Total Hierarchy ###
DF_MultiV_Total %>% 
  dplyr::select(-"Replication") -> DF_MultScores_AllTS

DF_MultScores_AllTS %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`)) -> DF_MultScores_AllTS

#DF_MultScores %>% dplyr::filter(`R-method` != "Base") -> DF_MultScore_Recon

DF_MultScores_AllTS %>% 
  dplyr::filter(`F-method`=="ETS" | `R-method`=="Base", 
                `F-method`!="ARIMA") -> DF_MultScores_AllTS_ETS

DF_MultScores_AllTS %>% 
  dplyr::filter(`F-method`=="ARIMA" | `R-method`=="Base", 
                `F-method`!="ETS") -> DF_MultScores_AllTS_ARIMA

#Calculate the skill scores 

#For ETS

DF_MultScores_AllTS_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_AllTS_ETS 

DF_MultScores_AllTS_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_AllTS_ETS 

DF_MultScores_AllTS_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_AllTS_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_AllTS_ETS))*100, digits = 4)) -> DF_MultScore_SS_AllTS_ETS

DF_MultScore_SS_AllTS_ETS %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_ETS

DF_MultScore_SS_AllTS_ETS %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_ETS

# View(SS_E.ES_AllTS_ETS)
# View(SS_E.VS_AllTS_ETS)

#For ARIMA

DF_MultScores_AllTS_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_AllTS_ARIMA 

DF_MultScores_AllTS_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_AllTS_ARIMA 


DF_MultScores_AllTS_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_AllTS_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_AllTS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_AllTS_ARIMA

DF_MultScore_SS_AllTS_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_AllTS_ARIMA

DF_MultScore_SS_AllTS_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_AllTS_ARIMA

# View(SS_E.ES_AllTS_ARIMA)
# View(SS_E.VS_AllTS_ARIMA)


### State level of the Hierarchy ###

DF_MultiV_States %>% 
  dplyr::select(-"Replication") -> DF_MultScores_States

DF_MultScores_States %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`)) -> DF_MultScores_States

#DF_MultScores %>% dplyr::filter(`R-method` != "Base") -> DF_MultScore_Recon

DF_MultScores_States %>% 
  dplyr::filter(`F-method`=="ETS" | `R-method`=="Base", 
                `F-method`!="ARIMA") -> DF_MultScores_States_ETS

DF_MultScores_States %>% 
  dplyr::filter(`F-method`=="ARIMA" | `R-method`=="Base", 
                `F-method`!="ETS") -> DF_MultScores_States_ARIMA

#Calculate the skill scores 

#For ETS

DF_MultScores_States_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_States_ETS 

DF_MultScores_States_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_States_ETS 

DF_MultScores_States_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_States_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_States_ETS))*100, digits = 4)) -> DF_MultScore_SS_States_ETS

DF_MultScore_SS_States_ETS %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_States_ETS

DF_MultScore_SS_States_ETS %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_States_ETS

# View(SS_E.ES_States_ETS)
# View(SS_E.VS_States_ETS)

#For ARIMA

DF_MultScores_States_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_States_ARIMA 

DF_MultScores_States_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_States_ARIMA 


DF_MultScores_States_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_States_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_States_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_States_ARIMA

DF_MultScore_SS_States_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_States_ARIMA

DF_MultScore_SS_States_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_States_ARIMA

# View(SS_E.ES_States_ARIMA)
# View(SS_E.VS_States_ARIMA)



### Zone level of the Hierarchy ###
DF_MultiV_Zones %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Zones

DF_MultScores_Zones %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`)) -> DF_MultScores_Zones

#DF_MultScores %>% dplyr::filter(`R-method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Zones %>% 
  dplyr::filter(`F-method`=="ETS" | `R-method`=="Base", 
                `F-method`!="ARIMA") -> DF_MultScores_Zones_ETS

DF_MultScores_Zones %>% 
  dplyr::filter(`F-method`=="ARIMA" | `R-method`=="Base", 
                `F-method`!="ETS") -> DF_MultScores_Zones_ARIMA

#Calculate the skill scores 

#For ETS

DF_MultScores_Zones_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Zones_ETS 

DF_MultScores_Zones_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_Zones_ETS 

DF_MultScores_Zones_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Zones_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Zones_ETS))*100, digits = 4)) -> DF_MultScore_SS_Zones_ETS

DF_MultScore_SS_Zones_ETS %>% 
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_Zones_ETS

DF_MultScore_SS_Zones_ETS %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_Zones_ETS

# View(SS_E.ES_Zones_ETS)
# View(SS_E.VS_Zones_ETS)

#For ARIMA

DF_MultScores_Zones_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Zones_ARIMA 

DF_MultScores_Zones_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% 
  as_vector() -> Base_E.VS_Zones_ARIMA 


DF_MultScores_Zones_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Zones_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Zones_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Zones_ARIMA

DF_MultScore_SS_Zones_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_Zones_ARIMA

DF_MultScore_SS_Zones_ARIMA %>%  
  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_Zones_ARIMA

# View(SS_E.ES_Zones_ARIMA)
# View(SS_E.VS_Zones_ARIMA)



### Regional(Bottom) level of the Hierarchy ###

DF_MultiV_Regions %>% 
  dplyr::select(-"Replication") -> DF_MultScores_Regions

DF_MultScores_Regions %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`),
            E.LS = mean(`Log score`)) -> DF_MultScores_Regions

#DF_MultScores %>% dplyr::filter(`R-method` != "Base") -> DF_MultScore_Recon

DF_MultScores_Regions %>% 
  dplyr::filter(`F-method`=="ETS" | `R-method`=="Base", 
                `F-method`!="ARIMA") -> DF_MultScores_Regions_ETS

DF_MultScores_Regions %>% 
  dplyr::filter(`F-method`=="ARIMA" | `R-method`=="Base", 
                `F-method`!="ETS") -> DF_MultScores_Regions_ARIMA

#Calculate the skill scores 

#For ETS

DF_MultScores_Regions_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Regions_ETS 

DF_MultScores_Regions_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_Regions_ETS 

DF_MultScores_Regions_ETS %>% 
  filter(`F-method`=="ETS", `R-method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% as_vector() -> BU_E.LS_Regions_ETS 


DF_MultScores_Regions_ETS %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Regions_ETS))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Regions_ETS))*100, digits = 4),
         SS_E.LS = round((1-(`E.LS`/BU_E.LS_Regions_ETS))*100, digits = 4)) -> DF_MultScore_SS_Regions_ETS

DF_MultScore_SS_Regions_ETS %>% 
  ungroup() %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_Regions_ETS

DF_MultScore_SS_Regions_ETS %>%  
  ungroup() %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_Regions_ETS

DF_MultScore_SS_Regions_ETS %>% 
  ungroup() %>% 
  filter(`R-method` != "Base") %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.LS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.LS`) -> SS_E.LS_Regions_ETS


# View(SS_E.ES_Regions_ETS)
# View(SS_E.VS_Regions_ETS)
# View(SS_E.LS_Regions_ETS)

#For ARIMA

DF_MultScores_Regions_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_Regions_ARIMA 

DF_MultScores_Regions_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_Regions_ARIMA 

DF_MultScores_Regions_ARIMA %>% 
  filter(`F-method`=="ARIMA", `R-method`=="Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% as_vector() -> BU_E.LS_Regions_ARIMA 


DF_MultScores_Regions_ARIMA %>% 
  mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_Regions_ARIMA))*100, digits = 4),
         SS_E.VS = round((1-(`E.VS`/Base_E.VS_Regions_ARIMA))*100, digits = 4),
         SS_E.LS = round((1-(`E.LS`/BU_E.LS_Regions_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_Regions_ARIMA

DF_MultScore_SS_Regions_ARIMA %>% 
  ungroup() %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_Regions_ARIMA

DF_MultScore_SS_Regions_ARIMA %>%  
  ungroup() %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_Regions_ARIMA

DF_MultScore_SS_Regions_ARIMA %>% 
  ungroup() %>% 
  filter(`R-method` != "Base") %>% 
  dplyr::select(`Forecast Horizon`, `R-method`, `SS_E.LS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.LS`) -> SS_E.LS_Regions_ARIMA


# View(SS_E.ES_Regions_ARIMA)
# View(SS_E.VS_Regions_ARIMA)
# View(SS_E.LS_Regions_ARIMA)