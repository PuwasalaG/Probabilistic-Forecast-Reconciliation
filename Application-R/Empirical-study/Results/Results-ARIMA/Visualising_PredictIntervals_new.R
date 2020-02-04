require(tidyverse)
require(ggplot2)
require(ggfan)
library(gridExtra)
library(grid)
library(ggpubr)
library(forecast)
library(zoo)

load("../NonPara_approach/Without-OptG/Forecasting_OvernightTrips_NonParaMethod_Fdistributions.RData")

###--Density plots for Total and state level--###

# Matrix to store simulated data from forecast densities for each aggregate level series

Density.forecast_total.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Unrecon <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Unrecon <- matrix(0, nrow = B, ncol = p)


for (i in 1:p) {
  
  Density.forecast_total.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,1]
  Density.forecast_B.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,4]
  Density.forecast_F.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,7]
  
  Density.forecast_AD.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,12]
  Density.forecast_BD.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,18]
  Density.forecast_CB.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,21]
  Density.forecast_DA.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,24]
  
  Density.forecast_total.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,1]
  Density.forecast_B.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,4]
  Density.forecast_F.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,7]
  
  Density.forecast_AD.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,12]
  Density.forecast_BD.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,18]
  Density.forecast_CB.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,21]
  Density.forecast_DA.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,24]
  
}

Time <- as.yearmon(1998 + seq(0, 12*21-1)/12)
Time_forecast <- Time[-(1:100)]



Mean_total_MinT <- apply(Density.forecast_total.Mint.shr, 2, mean)
Mean_B_MinT <- apply(Density.forecast_B.Mint.shr, 2, mean)
Mean_C_MinT <- apply(Density.forecast_C.Mint.shr, 2, mean)
Mean_F_MinT <- apply(Density.forecast_F.Mint.shr, 2, mean)

Mean_AD_MinT <- apply(Density.forecast_AD.Mint.shr, 2, mean)
Mean_BD_MinT <- apply(Density.forecast_BD.Mint.shr, 2, mean)
Mean_CB_MinT <- apply(Density.forecast_CB.Mint.shr, 2, mean)
Mean_DA_MinT <- apply(Density.forecast_DA.Mint.shr, 2, mean)

Mean_total_Unrecon <- apply(Density.forecast_total.Unrecon, 2, mean)
Mean_B_Unrecon <- apply(Density.forecast_B.Unrecon, 2, mean)
Mean_C_Unrecon <- apply(Density.forecast_C.Unrecon, 2, mean)
Mean_F_Unrecon <- apply(Density.forecast_F.Unrecon, 2, mean)

Mean_AD_Unrecon <- apply(Density.forecast_AD.Unrecon, 2, mean)
Mean_BD_Unrecon <- apply(Density.forecast_BD.Unrecon, 2, mean)
Mean_CB_Unrecon <- apply(Density.forecast_CB.Unrecon, 2, mean)
Mean_DA_Unrecon <- apply(Density.forecast_DA.Unrecon, 2, mean)


AllTS <- data.frame(Time = Time, AllTS)
AllTS_graph <- AllTS %>% filter(Time > "May 2006")

Percetile_total_MinT <- apply(Density.forecast_total.Mint.shr, 2, quantile, 
                              probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast) %>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")
Percetile_B_MinT <- apply(Density.forecast_B.Mint.shr, 2, quantile, 
                          probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_C_MinT <- apply(Density.forecast_C.Mint.shr, 2, quantile, 
                          probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_F_MinT <- apply(Density.forecast_F.Mint.shr, 2, quantile, 
                          probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_AD_MinT <- apply(Density.forecast_AD.Mint.shr, 2, quantile, 
                           probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_BD_MinT <- apply(Density.forecast_BD.Mint.shr, 2, quantile, 
                           probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_CB_MinT <- apply(Density.forecast_CB.Mint.shr, 2, quantile, 
                           probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_DA_MinT <- apply(Density.forecast_DA.Mint.shr, 2, quantile, 
                           probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")


Percetile_total_Unrecon <- apply(Density.forecast_total.Unrecon, 2, quantile, 
                                 probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_B_Unrecon <- apply(Density.forecast_B.Unrecon, 2, quantile, 
                             probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_C_Unrecon <- apply(Density.forecast_C.Unrecon, 2, quantile, 
                             probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_F_Unrecon <- apply(Density.forecast_F.Unrecon, 2, quantile, 
                             probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_AD_Unrecon <- apply(Density.forecast_AD.Unrecon, 2, quantile, 
                              probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_BD_Unrecon <- apply(Density.forecast_BD.Unrecon, 2, quantile, 
                              probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_CB_Unrecon <- apply(Density.forecast_CB.Unrecon, 2, quantile, 
                              probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")

Percetile_DA_Unrecon <- apply(Density.forecast_DA.Unrecon, 2, quantile, 
                              probs = c(2.5, 10, 90, 97.5)/100, na.rm = T) %>% 
  t() %>% 
  data.frame(Time_fc = Time_forecast)%>% 
  dplyr::rename("L95" = "X2.5.", "L80" = "X10.", "H80" = "X90.", "H95" = "X97.5.")




##--Total series--##

AllTS_total_df <- AllTS %>% 
  select(Time, Total) %>% 
  dplyr::rename(Time = `Time`, Australia = `Total`) %>%
  gather(key = Series, value = Observed, -Time)


ggplot(Percetile_total_MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(aes(x = Time_fc, y = Mean_total_MinT), color = "red") +
  geom_line(data = AllTS_total_df, aes(x=Time, y=Observed)) +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_MinT_NonPara

ggplot(Percetile_total_Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(aes(x = Time_fc, y = Mean_total_Unrecon), color = "red") +
  geom_line(data = AllTS_total_df, aes(x=Time, y=Observed)) +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_Unrecon_NonPara

grid.arrange( arrangeGrob(Plot_total_Unrecon_NonPara + theme(legend.position="none",
                                                             axis.title.y = element_blank()), 
                          Plot_total_MinT_NonPara + theme(legend.position="none",
                                                          axis.title.y = element_blank()),
                          top="Non-parametric Approach", ncol = 2, left = textGrob("Overnight Trips", rot = 90, vjust = 1)), 
              ncol=1,  heights=c(10, 0.5)) -> Plot_Total_NonPara


##--States--##

# Visualising only Queensland, Tasmania and Vistoria

AllTS_states_df <- AllTS %>% 
  select(Time, B, C, `F`) %>% 
  dplyr::rename(Time = `Time`, Victoria = `B`, Queensland = `C`, Tasmania = `F`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

Percetile_B_MinT <- Percetile_B_MinT %>% add_column(Series = "Victoria")
Percetile_C_MinT <- Percetile_C_MinT %>% add_column(Series = "Queensland")
Percetile_F_MinT <- Percetile_F_MinT %>% add_column(Series = "Tasmania")

Percentile_States_MinT <- rbind(Percetile_B_MinT, Percetile_C_MinT, Percetile_F_MinT)


ggplot(Percentile_States_MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_MinT_NonPara

#OLS

Percetile_B_OLS <- Percetile_B_OLS %>% add_column(Series = "Victoria")
Percetile_C_OLS <- Percetile_C_OLS %>% add_column(Series = "Queensland")
Percetile_F_OLS <- Percetile_F_OLS %>% add_column(Series = "Tasmania")

Percentile_States_OLS <- rbind(Percetile_B_OLS, Percetile_C_OLS, Percetile_F_OLS)


ggplot(Percentile_States_OLS) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_OLS_NonPara

#Unrecon

Percetile_B_Unrecon <- Percetile_B_Unrecon %>% add_column(Series = "Victoria")
Percetile_C_Unrecon <- Percetile_C_Unrecon %>% add_column(Series = "Queensland")
Percetile_F_Unrecon <- Percetile_F_Unrecon %>% add_column(Series = "Tasmania")

Percentile_States_Unrecon <- rbind(Percetile_B_Unrecon, Percetile_C_Unrecon, Percetile_F_Unrecon)


ggplot(Percentile_States_Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_Unrecon_NonPara


grid.arrange( arrangeGrob(Plot_States_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_States_OLS_NonPara + theme(legend.position="none"),
                          Plot_States_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  heights=c(10, 0.5)) 


##--Zones--##

# Visualising only AD-South NSW, BD-North East VIC, CB-Central Coast QLD and 
# DA-Metro SA 

AllTS_Zones_df <- AllTS %>% 
  select(Time, AD, BD, CB, DA) %>% 
  dplyr::rename(Time = `Time`, 'South.NSW' = `AD`, 'North.East.VIC' = `BD`, 
                'Central.Coast.QLD' = `CB`, 'Metro.SA' = `DA`) %>%
  gather(key = Series, value = Observed, -Time)


#MinT

Percetile_AD_MinT <- Percetile_AD_MinT %>% add_column(Series = "South.NSW")
Percetile_BD_MinT <- Percetile_BD_MinT %>% add_column(Series = "North.East.VIC")
Percetile_CB_MinT <- Percetile_CB_MinT %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA_MinT <- Percetile_DA_MinT %>% add_column(Series = "Metro.SA")

Percentile_Zones_MinT <- rbind(Percetile_AD_MinT, Percetile_BD_MinT, 
                               Percetile_CB_MinT, Percetile_DA_MinT)


ggplot(Percentile_Zones_MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_MinT_NonPara

#OLS

Percetile_AD_OLS <- Percetile_AD_OLS %>% add_column(Series = "South.NSW")
Percetile_BD_OLS <- Percetile_BD_OLS %>% add_column(Series = "North.East.VIC")
Percetile_CB_OLS <- Percetile_CB_OLS %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA_OLS <- Percetile_DA_OLS %>% add_column(Series = "Metro.SA")

Percentile_Zones_OLS <- rbind(Percetile_AD_OLS, Percetile_BD_OLS, 
                              Percetile_CB_OLS, Percetile_DA_OLS)


ggplot(Percentile_Zones_OLS) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_OLS_NonPara

#Unrecon

Percetile_AD_Unrecon <- Percetile_AD_Unrecon %>% add_column(Series = "South.NSW")
Percetile_BD_Unrecon <- Percetile_BD_Unrecon %>% add_column(Series = "North.East.VIC")
Percetile_CB_Unrecon <- Percetile_CB_Unrecon %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA_Unrecon <- Percetile_DA_Unrecon %>% add_column(Series = "Metro.SA")

Percentile_Zones_Unrecon <- rbind(Percetile_AD_Unrecon, Percetile_BD_Unrecon, 
                                  Percetile_CB_Unrecon, Percetile_DA_Unrecon)


ggplot(Percentile_Zones_Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Unrecon") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_Unrecon_NonPara



grid.arrange( arrangeGrob(Plot_Zones_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_Zones_OLS_NonPara + theme(legend.position="none"),
                          Plot_Zones_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  heights=c(10, 0.5)) 

rm(list=ls()[! ls() %in% c("Plot_total_OLS_NonPara", "Plot_total_MinT_NonPara", 
                           "Plot_total_Unrecon_NonPara", "Plot_states_Unrecon_NonPara",
                           "Plot_states_OLS_NonPara", "Plot_states_MinT_NonPara", 
                           "Plot_Zones_Unrecon_NonPara",
                           "Plot_Zones_OLS_NonPara", "Plot_Zones_MinT_NonPara")])

####################################
##--Gaussian Approach--##
####################################

load("../Gaussian_approach/Without-OptG/Forecasting_OvernightTrips_GaussMethod_Fdistributions.RData")


Percetile_total.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_B.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_C.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_F.MinT <- matrix(0, nrow = p, ncol = 4)

Percetile_AD.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_BD.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_CB.MinT <- matrix(0, nrow = p, ncol = 4)
Percetile_DA.MinT <- matrix(0, nrow = p, ncol = 4)

Percetile_total.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_B.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_C.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_F.OLS <- matrix(0, nrow = p, ncol = 4)

Percetile_AD.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_BD.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_CB.OLS <- matrix(0, nrow = p, ncol = 4)
Percetile_DA.OLS <- matrix(0, nrow = p, ncol = 4)

Percetile_total.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_B.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_C.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_F.Unrecon <- matrix(0, nrow = p, ncol = 4)

Percetile_AD.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_BD.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_CB.Unrecon <- matrix(0, nrow = p, ncol = 4)
Percetile_DA.Unrecon <- matrix(0, nrow = p, ncol = 4)


for (i in 1:p) {
  
  DF_MinT <- data.frame(Series = c("Total", "B", "C", "F", "AD", "BD", "CB", "DA"), 
                        mean = Recon_MinT_mean_ARIMA[[i]][1,c(1,3,4,7,12,18,21,24)],
                        Sd = sqrt(diag(Recon_MinT_Cov_ARIMA[[i]])[c(1,3,4,7,12,18,21,24)]))
  DF_MinT <- DF_MinT %>% 
    mutate(L95 = mean - 1.96*Sd,
           L80 = mean - 1.28*Sd,
           H80 = mean + 1.28*Sd,
           H95 = mean + 1.96*Sd)
  
  Percetile_total.MinT[i,] <- DF_MinT[1, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_B.MinT[i,] <- DF_MinT[2, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_C.MinT[i,] <- DF_MinT[3, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_F.MinT[i,] <- DF_MinT[4, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  Percetile_AD.MinT[i,] <- DF_MinT[5, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_BD.MinT[i,] <- DF_MinT[6, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_CB.MinT[i,] <- DF_MinT[7, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_DA.MinT[i,] <- DF_MinT[8, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  DF_OLS <- data.frame(Series = c("Total", "B", "C", "F", "AD", "BD", "CB", "DA"), 
                       mean = Recon_OLS_mean_ARIMA[[i]][1,c(1,3,4,7,12,18,21,24)],
                       Sd = sqrt(diag(Recon_OLS_Cov_ARIMA[[i]])[c(1,3,4,7,12,18,21,24)]))
  DF_OLS <- DF_OLS %>% 
    mutate(L95 = mean - 1.96*Sd,
           L80 = mean - 1.28*Sd,
           H80 = mean + 1.28*Sd,
           H95 = mean + 1.96*Sd)
  
  Percetile_total.OLS[i,] <- DF_OLS[1, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_B.OLS[i,] <- DF_OLS[2, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_C.OLS[i,] <- DF_OLS[3, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_F.OLS[i,] <- DF_OLS[4, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  Percetile_AD.OLS[i,] <- DF_OLS[5, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_BD.OLS[i,] <- DF_OLS[6, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_CB.OLS[i,] <- DF_OLS[7, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_DA.OLS[i,] <- DF_OLS[8, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  DF_Unrecon <- data.frame(Series = c("Total", "B", "C", "F", "AD", "BD", "CB", "DA"), 
                           mean = Unrecon_mean_ARIMA[[i]][1,c(1,3,4,7,12,18,21,24)],
                           Sd = sqrt(diag(Unrecon_Cov_ARIMA[[i]])[c(1,3,4,7,12,18,21,24)]))
  DF_Unrecon <- DF_Unrecon %>% 
    mutate(L95 = mean - 1.96*Sd,
           L80 = mean - 1.28*Sd,
           H80 = mean + 1.28*Sd,
           H95 = mean + 1.96*Sd)
  
  Percetile_total.Unrecon[i,] <- DF_Unrecon[1, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_B.Unrecon[i,] <- DF_Unrecon[2, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_C.Unrecon[i,] <- DF_Unrecon[3, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_F.Unrecon[i,] <- DF_Unrecon[4, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  Percetile_AD.Unrecon[i,] <- DF_Unrecon[5, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_BD.Unrecon[i,] <- DF_Unrecon[6, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_CB.Unrecon[i,] <- DF_Unrecon[7, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  Percetile_DA.Unrecon[i,] <- DF_Unrecon[8, c("L95", "L80", "H80", "H95")] %>% as.numeric()
  
  
}

Percetile_total.MinT <- Percetile_total.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_B.MinT <- Percetile_B.MinT %>% 
  as.data.frame() %>%
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_C.MinT <- Percetile_C.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_F.MinT <- Percetile_F.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")

Percetile_AD.MinT <- Percetile_AD.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_BD.MinT <- Percetile_BD.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_CB.MinT <- Percetile_CB.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_DA.MinT <- Percetile_DA.MinT %>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")

Percetile_total.OLS<- Percetile_total.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_B.OLS<- Percetile_B.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_C.OLS<- Percetile_C.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_F.OLS<- Percetile_F.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")

Percetile_AD.OLS<- Percetile_AD.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_BD.OLS<- Percetile_BD.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_CB.OLS<- Percetile_CB.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_DA.OLS<- Percetile_DA.OLS%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")


Percetile_total.Unrecon<- Percetile_total.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_B.Unrecon<- Percetile_B.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_C.Unrecon<- Percetile_C.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_F.Unrecon<- Percetile_F.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")

Percetile_AD.Unrecon<- Percetile_AD.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_BD.Unrecon<- Percetile_BD.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_CB.Unrecon<- Percetile_CB.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")
Percetile_DA.Unrecon<- Percetile_DA.Unrecon%>% 
  as.data.frame() %>% 
  add_column(Time_fc = Time_forecast) %>% 
  rename("L95" = "V1", "L80" = "V2", "H80" = "V3", "H95" = "V4")


Time <- as.yearmon(1998 + seq(0, 12*21-1)/12)
Time_forecast <- Time[-(1:100)]

AllTS <- data.frame(Time = Time, AllTS)
AllTS_graph <- AllTS %>% filter(Time > "May 2006")

##--Total series--##

AllTS_total_df <- AllTS %>% 
  select(Time, Total) %>% 
  dplyr::rename(Time = `Time`, Australia = `Total`) %>%
  gather(key = Series, value = Observed, -Time)


ggplot(Percetile_total.MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_total_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_MinT_Gauss

ggplot(Percetile_total.OLS) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_total_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_OLS_Gauss

ggplot(Percetile_total.Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_total_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_Unrecon_Gauss

grid.arrange( arrangeGrob(Plot_total_Unrecon_Gauss + theme(legend.position="none",
                                                           axis.title.y = element_blank()), 
                          Plot_total_OLS_Gauss + theme(legend.position="none",
                                                       axis.title.y = element_blank()),
                          Plot_total_MinT_Gauss + theme(legend.position="none",
                                                        axis.title.y = element_blank()),
                          top="Gaussian Approach", ncol = 3, left = textGrob("Overnight Trips", rot = 90, vjust = 1)), 
              ncol=1,  heights=c(10, 0.5)) -> Plot_Total_Gauss



##--States--##

# Visualising only Queensland, Tasmania and Vistoria

AllTS_states_df <- AllTS %>% 
  select(Time, B, C, `F`) %>% 
  dplyr::rename(Time = `Time`, Victoria = `B`, Queensland = `C`, Tasmania = `F`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

Percetile_B.MinT <- Percetile_B.MinT %>% add_column(Series = "Victoria")
Percetile_C.MinT <- Percetile_C.MinT %>% add_column(Series = "Queensland")
Percetile_F.MinT <- Percetile_F.MinT %>% add_column(Series = "Tasmania")

Percentile_States_MinT <- rbind(Percetile_B.MinT, Percetile_C.MinT, Percetile_F.MinT)


ggplot(Percentile_States_MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_MinT_NonPara

#OLS

Percetile_B.OLS <- Percetile_B.OLS %>% add_column(Series = "Victoria")
Percetile_C.OLS <- Percetile_C.OLS %>% add_column(Series = "Queensland")
Percetile_F.OLS <- Percetile_F.OLS %>% add_column(Series = "Tasmania")

Percentile_States_OLS <- rbind(Percetile_B.OLS, Percetile_C.OLS, Percetile_F.OLS)


ggplot(Percentile_States_OLS) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_OLS_NonPara

#Unrecon

Percetile_B.Unrecon <- Percetile_B.Unrecon %>% add_column(Series = "Victoria")
Percetile_C.Unrecon <- Percetile_C.Unrecon %>% add_column(Series = "Queensland")
Percetile_F.Unrecon <- Percetile_F.Unrecon %>% add_column(Series = "Tasmania")

Percentile_States_Unrecon <- rbind(Percetile_B.Unrecon, Percetile_C.Unrecon, Percetile_F.Unrecon)


ggplot(Percentile_States_Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_states_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_States_Unrecon_NonPara


grid.arrange( arrangeGrob(Plot_States_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_States_OLS_NonPara + theme(legend.position="none"),
                          Plot_States_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  heights=c(10, 0.5)) 


##--Zones--##

# Visualising only AD-South NSW, BD-North East VIC, CB-Central Coast QLD and 
# DA-Metro SA 

AllTS_Zones_df <- AllTS %>% 
  select(Time, AD, BD, CB, DA) %>% 
  dplyr::rename(Time = `Time`, 'South.NSW' = `AD`, 'North.East.VIC' = `BD`, 
                'Central.Coast.QLD' = `CB`, 'Metro.SA' = `DA`) %>%
  gather(key = Series, value = Observed, -Time)


#MinT

Percetile_AD.MinT <- Percetile_AD.MinT %>% add_column(Series = "South.NSW")
Percetile_BD.MinT <- Percetile_BD.MinT %>% add_column(Series = "North.East.VIC")
Percetile_CB.MinT <- Percetile_CB.MinT %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA.MinT <- Percetile_DA.MinT %>% add_column(Series = "Metro.SA")

Percentile_Zones_MinT <- rbind(Percetile_AD.MinT, Percetile_BD.MinT, 
                               Percetile_CB.MinT, Percetile_DA.MinT)


ggplot(Percentile_Zones_MinT) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_MinT_NonPara

#OLS

Percetile_AD.OLS <- Percetile_AD.OLS %>% add_column(Series = "South.NSW")
Percetile_BD.OLS <- Percetile_BD.OLS %>% add_column(Series = "North.East.VIC")
Percetile_CB.OLS <- Percetile_CB.OLS %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA.OLS <- Percetile_DA.OLS %>% add_column(Series = "Metro.SA")

Percentile_Zones_OLS <- rbind(Percetile_AD.OLS, Percetile_BD.OLS, 
                              Percetile_CB.OLS, Percetile_DA.OLS)


ggplot(Percentile_Zones_OLS) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_OLS_NonPara

#Unrecon

Percetile_AD.Unrecon <- Percetile_AD.Unrecon %>% add_column(Series = "South.NSW")
Percetile_BD.Unrecon <- Percetile_BD.Unrecon %>% add_column(Series = "North.East.VIC")
Percetile_CB.Unrecon <- Percetile_CB.Unrecon %>% add_column(Series = "Central.Coast.QLD")
Percetile_DA.Unrecon <- Percetile_DA.Unrecon %>% add_column(Series = "Metro.SA")

Percentile_Zones_Unrecon <- rbind(Percetile_AD.Unrecon, Percetile_BD.Unrecon, 
                                  Percetile_CB.Unrecon, Percetile_DA.Unrecon)


ggplot(Percentile_Zones_Unrecon) +
  geom_ribbon(aes(x = Time_fc, ymin = `L95`, ymax = `H95`, color = "lightsteelblue2"), fill = "lightsteelblue2") +
  geom_ribbon(aes(x = Time_fc, ymin = `L80`, ymax = `H80`, color = "royalblue3"), fill = "royalblue3") + 
  scale_color_manual("Interval", values = c(royalblue3 = "royalblue3", lightsteelblue2 = "lightsteelblue2"),
                     labels = c("95%", "80%")) +
  geom_line(data = AllTS_Zones_df, linetype = "dashed", aes(x=Time, y=Observed)) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Unrecon") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_Unrecon_NonPara



grid.arrange( arrangeGrob(Plot_Zones_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_Zones_OLS_NonPara + theme(legend.position="none"),
                          Plot_Zones_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  heights=c(10, 0.5)) 






rm(list=ls()[! ls() %in% c("Plot_total_OLS_NonPara", "Plot_total_MinT_NonPara", 
                           "Plot_total_Unrecon_NonPara", "Plot_states_Unrecon_NonPara",
                           "Plot_states_OLS_NonPara", "Plot_states_MinT_NonPara", 
                           "Plot_Zones_Unrecon_NonPara",
                           "Plot_Zones_OLS_NonPara", "Plot_Zones_MinT_NonPara",
                           "Plot_total_OLS_Gauss", "Plot_total_MinT_Gauss", 
                           "Plot_total_Unrecon_Gauss", "Plot_states_Unrecon_Gauss",
                           "Plot_states_OLS_Gauss", "Plot_states_MinT_Gauss", 
                           "Plot_Zones_Unrecon_Gauss",
                           "Plot_Zones_OLS_Gauss", "Plot_Zones_MinT_Gauss")])


# Combining plots for Total from Gaussian and Non-parametric approaches

g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_total_OLS_NonPara)

label = textGrob("Overnight Trips", rot = 90, hjust = 0.5)

grid.arrange(arrangeGrob(Plot_total_Unrecon_Gauss + theme(legend.position="none",
                                                          axis.title.y = element_blank()), 
                         Plot_total_OLS_Gauss + theme(legend.position="none",
                                                      axis.title.y = element_blank()),
                         Plot_total_MinT_Gauss + theme(legend.position="none", 
                                                       axis.title.y = element_blank()),
                         top="Gaussian Approach", ncol = 3, left = textGrob("Overnight Trips", rot = 90, vjust = 1)),
             ncol=1,  heights=c(10, 0.5)) -> Plot_Total_Gauss

grid.arrange( arrangeGrob(Plot_total_Unrecon_NonPara + theme(legend.position="none",
                                                             axis.title.y = element_blank()), 
                          Plot_total_OLS_NonPara + theme(legend.position="none",
                                                         axis.title.y = element_blank()),
                          Plot_total_MinT_NonPara + theme(legend.position="none",
                                                          axis.title.y = element_blank()),
                          top="Non-parametric Approach", ncol = 3, left = textGrob("Overnight Trips", rot = 90, vjust = 1)), 
              ncol=1,  heights=c(10, 0.5)) -> Plot_Total_NonPara

grid.arrange(arrangeGrob(Plot_Total_Gauss, Plot_Total_NonPara, ncol = 1), 
             ncol = 1, heights = c(20,1))
