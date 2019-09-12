## Change Geelong to Geelong and the Bellarine


library(tidyverse)

Data_OvernightTrips <- read_csv("Tourism_Australia_OvernightTrips_upto_2017.csv", skip = 9)
Data_Nights <- read_csv("VN_FullDataSet Edited by George2017.csv", skip = 6)[,-1] %>% 
  rename(Year = `Calendar year`)

Data_OvernightTrips <- Data_OvernightTrips[,names(Data_Nights)]

#Renaming some regions
Data_OvernightTrips %>% 
  rename("Southern Queensland Country" = `Darling Downs`,
         "Townsville" = Northern,
         "Outback QLD" = Outback,
         "West Coast" = `Wilderness West`,
         "Litchfield Kakadu Arnhem" = `Kakadu Arnhem`) -> Data_OvernightTrips
  

write.csv(Data_OvernightTrips, "OvernightTrips_2017.csv")

#Manually added Capricorn and	Gladstone and removed Central QLD from OvernightTrips_2017

#Reading modified OvernightTrips_2017
Data_OvernightTrips_2017 <- read_csv("OvernightTrips_2017.csv")[,-1]

#Reading data upto 2018
Data_OvernightTrips_2018 <- read_csv("Tourism_Australia_OvernightTrips_upto_2018.csv", skip = 9)

Data_OvernightTrips_2018 %>% 
  rename("Outback QLD" = `Outback Queensland`) -> Data_OvernightTrips_2018

Data_OvernightTrips_2018 <- Data_OvernightTrips_2018[,names(Data_OvernightTrips_2017)]

Data_OvernightTrips_2018 %>% 
  mutate(Wimmera = Wimmera+Mallee) %>% 
  dplyr::select(-Mallee) %>% 
  rename(Wimmera_Mallee = Wimmera) -> Data_OvernightTrips_2018

write.csv(Data_OvernightTrips_2018, "OvernightTrips_2018.csv")




