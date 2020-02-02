require(tidyverse)
require(ggplot2)
require(ggfan)
library(gridExtra)
library(grid)
library(ggpubr)
library(forecast)

load("../NonPara_approach/Without-OptG/Forecasting_OvernightTrips_NonParaMethod_Fdistributions.RData")

###--Density plots for Total and state level--###

# Matrix to store simulated data from forecast densities for each aggregate level series

Density.forecast_total.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_A.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_D.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_E.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_G.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_A.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_D.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_E.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_G.OLS <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.OLS <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_A.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_D.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_E.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_G.BU <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_A.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_D.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_E.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_G.Unrecon <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Unrecon <- matrix(0, nrow = B, ncol = p)


for (i in 1:p) {
  
  Density.forecast_total.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,1]
  Density.forecast_A.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,2]
  Density.forecast_B.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,4]
  Density.forecast_D.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,5]
  Density.forecast_E.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,6]
  Density.forecast_F.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,7]
  Density.forecast_G.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,8]
  
  Density.forecast_AD.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,12]
  Density.forecast_BD.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,18]
  Density.forecast_CB.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,21]
  Density.forecast_DA.Mint.shr[,i] <- Recon_MinT_FP_ARIMA_h1[[i]][,24]
  
  
  Density.forecast_total.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,1]
  Density.forecast_A.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,2]
  Density.forecast_B.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,4]
  Density.forecast_D.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,5]
  Density.forecast_E.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,6]
  Density.forecast_F.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,7]
  Density.forecast_G.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,8]
  
  Density.forecast_AD.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,12]
  Density.forecast_BD.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,18]
  Density.forecast_CB.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,21]
  Density.forecast_DA.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,24]
  
  
  Density.forecast_total.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,1]
  Density.forecast_A.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,2]
  Density.forecast_B.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,4]
  Density.forecast_D.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,5]
  Density.forecast_E.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,6]
  Density.forecast_F.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,7]
  Density.forecast_G.BU[,i] <- Recon_BU_FP_ARIMA_h1[[i]][,8]
  
  Density.forecast_total.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,1]
  Density.forecast_A.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,2]
  Density.forecast_B.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,3]
  Density.forecast_C.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,4]
  Density.forecast_D.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,5]
  Density.forecast_E.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,6]
  Density.forecast_F.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,7]
  Density.forecast_G.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,8]
  
  Density.forecast_AD.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,12]
  Density.forecast_BD.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,18]
  Density.forecast_CB.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,21]
  Density.forecast_DA.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,24]
  
}

Time <- as.yearmon(1998 + seq(0, 12*21-1)/12)
Time_forecast <- Time[-(1:100)]

AllTS <- data.frame(Time = Time, AllTS)
AllTS_graph <- AllTS %>% filter(Time > "May 2006")

##--Total series--##

AllTS_total_df <- AllTS %>% 
  select(Time, Total) %>% 
  dplyr::rename(Time = `Time`, Australia = `Total`) %>%
  gather(key = Series, value = Observed, -Time)

DD_Total_OLS <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.OLS))
Density.forecast_total.OLS_df <- DD_Total_OLS %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.OLS_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_OLS_NonPara


DD_Total_Unrecon <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.Unrecon))
Density.forecast_total.Unrecon_df <- DD_Total_Unrecon %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.Unrecon_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_Unrecon_NonPara

DD_Total_BU <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.BU))
Density.forecast_total.BU_df <- DD_Total_BU %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.BU_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Reconciled-BU") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_BU_NonPara

DD_Total_MinT <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.Mint.shr))
Density.forecast_total.Mint.shr_df <- DD_Total_MinT %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.Mint.shr_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT(Shrink)") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_MinT_NonPara


##--States--##

# Visualising only Queensland, Tasmania and Vistoria

AllTS_states_df <- AllTS %>% 
  select(Time, B, C, `F`) %>% 
  dplyr::rename(Time = `Time`, Victoria = `B`, Queensland = `C`, Tasmania = `F`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

DD_A_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_A.Mint.shr))
DD_B_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.Mint.shr))
DD_C_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.Mint.shr))
DD_D_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_D.Mint.shr))
DD_E_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_E.Mint.shr))
DD_F_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.Mint.shr))
DD_G_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_G.Mint.shr))

Density.forecast_A.Mint.shr_df <- DD_A_MinT %>% gather(key=Sim, value="New South Wales", -Time_fc)
Density.forecast_B.Mint.shr_df <- DD_B_MinT %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.Mint.shr_df <- DD_C_MinT %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_D.Mint.shr_df <- DD_D_MinT %>% gather(key=Sim, value=South_Australia, -Time_fc)
Density.forecast_E.Mint.shr_df <- DD_E_MinT %>% gather(key=Sim, value=Western_Australia, -Time_fc)
Density.forecast_F.Mint.shr_df <- DD_F_MinT %>% gather(key=Sim, value=Tasmania, -Time_fc)
Density.forecast_G.Mint.shr_df <- DD_G_MinT %>% gather(key=Sim, value=Northern_Terrotary, -Time_fc)

Density.forecast_States_Mint.shr <- data.frame(Density.forecast_B.Mint.shr_df,
                                               Queensland = Density.forecast_C.Mint.shr_df[,'Queensland'], 
                                               Tasmania = Density.forecast_F.Mint.shr_df[,'Tasmania'])

Density.forecast_States_Mint.shr_df <- Density.forecast_States_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                        colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("MinT(Shrink)") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_MinT_NonPara


#OLS

DD_A_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_A.OLS))
DD_B_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.OLS))
DD_C_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.OLS))
DD_D_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_D.OLS))
DD_E_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_E.OLS))
DD_F_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.OLS))
DD_G_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_G.OLS))

Density.forecast_A.OLS_df <- DD_A_OLS %>% gather(key=Sim, value="New South Wales", -Time_fc)
Density.forecast_B.OLS_df <- DD_B_OLS %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.OLS_df <- DD_C_OLS %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_D.OLS_df <- DD_D_OLS %>% gather(key=Sim, value=South_Australia, -Time_fc)
Density.forecast_E.OLS_df <- DD_E_OLS %>% gather(key=Sim, value=Western_Australia, -Time_fc)
Density.forecast_F.OLS_df <- DD_F_OLS %>% gather(key=Sim, value=Tasmania, -Time_fc)
Density.forecast_G.OLS_df <- DD_G_OLS %>% gather(key=Sim, value=Northern_Terrotary, -Time_fc)

Density.forecast_States_OLS <- data.frame(Density.forecast_B.OLS_df,
                                          Queensland = Density.forecast_C.OLS_df[,'Queensland'], 
                                          Tasmania = Density.forecast_F.OLS_df[,'Tasmania'])

Density.forecast_States_OLS_df <- Density.forecast_States_OLS %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_OLS_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                        colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_OLS_NonPara

#BU

DD_A_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_A.BU))
DD_B_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.BU))
DD_C_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.BU))
DD_D_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_D.BU))
DD_E_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_E.BU))
DD_F_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.BU))
DD_G_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_G.BU))

Density.forecast_A.BU_df <- DD_A_BU %>% gather(key=Sim, value="New South Wales", -Time_fc)
Density.forecast_B.BU_df <- DD_B_BU %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.BU_df <- DD_C_BU %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_D.BU_df <- DD_D_BU %>% gather(key=Sim, value=South_Australia, -Time_fc)
Density.forecast_E.BU_df <- DD_E_BU %>% gather(key=Sim, value=Western_Australia, -Time_fc)
Density.forecast_F.BU_df <- DD_F_BU %>% gather(key=Sim, value=Tasmania, -Time_fc)
Density.forecast_G.BU_df <- DD_G_BU %>% gather(key=Sim, value=Northern_Terrotary, -Time_fc)

Density.forecast_States_BU <- data.frame(Density.forecast_B.BU_df,
                                         Queensland = Density.forecast_C.BU_df[,'Queensland'], 
                                         Tasmania = Density.forecast_F.BU_df[,'Tasmania'])

Density.forecast_States_BU_df <- Density.forecast_States_BU %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_BU_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                        colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Bottom up") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal")-> Plot_states_BU_NonPara

#Unrecon

DD_A_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_A.Unrecon))
DD_B_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.Unrecon))
DD_C_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.Unrecon))
DD_D_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_D.Unrecon))
DD_E_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_E.Unrecon))
DD_F_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.Unrecon))
DD_G_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_G.Unrecon))

Density.forecast_A.Unrecon_df <- DD_A_Unrecon %>% gather(key=Sim, value="New South Wales", -Time_fc)
Density.forecast_B.Unrecon_df <- DD_B_Unrecon %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.Unrecon_df <- DD_C_Unrecon %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_D.Unrecon_df <- DD_D_Unrecon %>% gather(key=Sim, value=South_Australia, -Time_fc)
Density.forecast_E.Unrecon_df <- DD_E_Unrecon %>% gather(key=Sim, value=Western_Australia, -Time_fc)
Density.forecast_F.Unrecon_df <- DD_F_Unrecon %>% gather(key=Sim, value=Tasmania, -Time_fc)
Density.forecast_G.Unrecon_df <- DD_G_Unrecon %>% gather(key=Sim, value=Northern_Terrotary, -Time_fc)

Density.forecast_States_Unrecon <- data.frame(Density.forecast_B.Unrecon_df,
                                              Queensland = Density.forecast_C.Unrecon_df[,'Queensland'], 
                                              Tasmania = Density.forecast_F.Unrecon_df[,'Tasmania'])

Density.forecast_States_Unrecon_df <- Density.forecast_States_Unrecon %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Unrecon_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                        colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_Unrecon_NonPara

# A function to get the legend from a plot

g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_states_Unrecon_NonPara)

grid.arrange( arrangeGrob(Plot_states_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_states_OLS_NonPara + theme(legend.position="none"),
                          Plot_states_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 


##--Zones--##

# Visualising only AD-South NSW, BD-North East VIC, CB-Central Coast QLD and 
# DA-Metro SA 

AllTS_Zones_df <- AllTS %>% 
  select(Time, AD, BD, CB, DA) %>% 
  dplyr::rename(Time = `Time`, 'South.NSW' = `AD`, 'North.East.VIC' = `BD`, 
                'Central.Coast.QLD' = `CB`, 'Metro.SA' = `DA`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

DD_AD_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.Mint.shr))
DD_BD_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.Mint.shr))
DD_CB_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.Mint.shr))
DD_DA_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.Mint.shr))

Density.forecast_AD.Mint.shr_df <- DD_AD_MinT %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.Mint.shr_df <- DD_BD_MinT %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.Mint.shr_df <- DD_CB_MinT %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.Mint.shr_df <- DD_DA_MinT %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_Mint.shr <- data.frame(Density.forecast_AD.Mint.shr_df,
                                               'North.East.VIC' = Density.forecast_BD.Mint.shr_df[,'North.East.VIC'], 
                                               'Central.Coast.QLD' = Density.forecast_CB.Mint.shr_df[,'Central.Coast.QLD'], 
                                               'Metro.SA' = Density.forecast_DA.Mint.shr_df[,'Metro.SA'])

Density.forecast_States_Mint.shr_df <- Density.forecast_States_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                              colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("MinT(Shrink)") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_MinT_NonPara


#OLS

DD_AD_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.OLS))
DD_BD_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.OLS))
DD_CB_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.OLS))
DD_DA_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.OLS))

Density.forecast_AD.OLS_df <- DD_AD_OLS %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.OLS_df <- DD_BD_OLS %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.OLS_df <- DD_CB_OLS %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.OLS_df <- DD_DA_OLS %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_OLS <- data.frame(Density.forecast_AD.OLS_df,
                                          'North.East.VIC' = Density.forecast_BD.OLS_df[,'North.East.VIC'], 
                                          'Central.Coast.QLD' = Density.forecast_CB.OLS_df[,'Central.Coast.QLD'], 
                                          'Metro.SA' = Density.forecast_DA.OLS_df[,'Metro.SA'])

Density.forecast_States_OLS_df <- Density.forecast_States_OLS %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_OLS_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("OLS") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_OLS_NonPara


# Incoherent 

DD_AD_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.Unrecon))
DD_BD_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.Unrecon))
DD_CB_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.Unrecon))
DD_DA_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.Unrecon))

Density.forecast_AD.Unrecon_df <- DD_AD_Unrecon %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.Unrecon_df <- DD_BD_Unrecon %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.Unrecon_df <- DD_CB_Unrecon %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.Unrecon_df <- DD_DA_Unrecon %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_Unrecon <- data.frame(Density.forecast_AD.Unrecon_df,
                                              'North.East.VIC' = Density.forecast_BD.Unrecon_df[,'North.East.VIC'], 
                                              'Central.Coast.QLD' = Density.forecast_CB.Unrecon_df[,'Central.Coast.QLD'], 
                                              'Metro.SA' = Density.forecast_DA.Unrecon_df[,'Metro.SA'])

Density.forecast_States_Unrecon_df <- Density.forecast_States_Unrecon %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Unrecon_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_Unrecon_NonPara


g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_Zones_Unrecon_NonPara)

grid.arrange( arrangeGrob(Plot_Zones_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_Zones_OLS_NonPara + theme(legend.position="none"),
                          Plot_Zones_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 

rm(list=ls()[! ls() %in% c("Plot_total_OLS_NonPara", "Plot_total_MinT_NonPara", 
                           "Plot_total_Unrecon_NonPara", "Plot_states_Unrecon_NonPara",
                           "Plot_states_OLS_NonPara", "Plot_states_MinT_NonPara", 
                           "Plot_Zones_Unrecon_NonPara",
                           "Plot_Zones_OLS_NonPara", "Plot_Zones_MinT_NonPara")])

####################################
    ##--Gaussian Approach--##
####################################

load("../Gaussian_approach/Without-OptG/Forecasting_OvernightTrips_GaussMethod_Fdistributions.RData")


Density.forecast_total.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Mint.shr <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Mint.shr <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.OLS <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.OLS <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.OLS <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.BU <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.BU <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.BU <- matrix(0, nrow = B, ncol = p)

Density.forecast_total.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_B.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_C.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_F.Unrecon <- matrix(0, nrow = B, ncol = p)

Density.forecast_AD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_BD.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_CB.Unrecon <- matrix(0, nrow = B, ncol = p)
Density.forecast_DA.Unrecon <- matrix(0, nrow = B, ncol = p)


for (i in 1:p) {
  
  Density.Mint.shr <- rmvnorm(n = B, mean = Recon_MinT_mean_ARIMA[[i]][1,],
                              sigma = Recon_MinT_Cov_ARIMA[[i]])
  Density.forecast_total.Mint.shr[,i] <- Density.Mint.shr[,1]
  Density.forecast_B.Mint.shr[,i] <- Density.Mint.shr[,3]
  Density.forecast_C.Mint.shr[,i] <- Density.Mint.shr[,4]
  Density.forecast_F.Mint.shr[,i] <- Density.Mint.shr[,7]
  
  Density.forecast_AD.Mint.shr[,i] <- Density.Mint.shr[,12]
  Density.forecast_BD.Mint.shr[,i] <- Density.Mint.shr[,18]
  Density.forecast_CB.Mint.shr[,i] <- Density.Mint.shr[,21]
  Density.forecast_DA.Mint.shr[,i] <- Density.Mint.shr[,24]
  
  Density.OLS <- rmvnorm(n = B, mean = Recon_OLS_mean_ARIMA[[i]][1,],
                         sigma = Recon_OLS_Cov_ARIMA[[i]])
  Density.forecast_total.OLS[,i] <- Density.OLS[,1]
  Density.forecast_B.OLS[,i] <- Density.OLS[,3]
  Density.forecast_C.OLS[,i] <- Density.OLS[,4]
  Density.forecast_F.OLS[,i] <- Density.OLS[,7]
  
  Density.forecast_AD.OLS[,i] <- Density.OLS[,12]
  Density.forecast_BD.OLS[,i] <- Density.OLS[,18]
  Density.forecast_CB.OLS[,i] <- Density.OLS[,21]
  Density.forecast_DA.OLS[,i] <- Density.OLS[,24]
  
  Density.BU <- rmvnorm(n = B, mean = Recon_BU_mean_ARIMA[[i]][1,],
                        sigma = Recon_BU_Cov_ARIMA[[i]])
  Density.forecast_total.BU[,i] <- Density.BU[,1]
  Density.forecast_B.BU[,i] <- Density.BU[,3]
  Density.forecast_C.BU[,i] <- Density.BU[,4]
  Density.forecast_F.BU[,i] <- Density.BU[,7]
  
  Density.forecast_AD.BU[,i] <- Density.BU[,12]
  Density.forecast_BD.BU[,i] <- Density.BU[,18]
  Density.forecast_CB.BU[,i] <- Density.BU[,21]
  Density.forecast_DA.BU[,i] <- Density.BU[,24]
  
  Density.Unrecon <- rmvnorm(n = B, mean = Unrecon_mean_ARIMA[[i]][1,],
                             sigma = Unrecon_Cov_ARIMA[[i]])
  Density.forecast_total.Unrecon[,i] <- Density.Unrecon[,1]
  Density.forecast_B.Unrecon[,i] <- Density.Unrecon[,3]
  Density.forecast_C.Unrecon[,i] <- Density.Unrecon[,4]
  Density.forecast_F.Unrecon[,i] <- Density.Unrecon[,7]
  
  Density.forecast_AD.Unrecon[,i] <- Density.Unrecon[,12]
  Density.forecast_BD.Unrecon[,i] <- Density.Unrecon[,18]
  Density.forecast_CB.Unrecon[,i] <- Density.Unrecon[,21]
  Density.forecast_DA.Unrecon[,i] <- Density.Unrecon[,24]
  
}


Time <- as.yearmon(1998 + seq(0, 12*21-1)/12)
Time_forecast <- Time[-(1:100)]

AllTS <- data.frame(Time = Time, AllTS)
AllTS_graph <- AllTS %>% filter(Time > "May 2006")

##--Total series--##

AllTS_total_df <- AllTS %>% 
  select(Time, Total) %>% 
  dplyr::rename(Time = `Time`, Australia = `Total`) %>%
  gather(key = Series, value = Observed, -Time)

DD_Total_OLS <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.OLS))
Density.forecast_total.OLS_df <- DD_Total_OLS %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.OLS_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_OLS_Gauss


DD_Total_Unrecon <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.Unrecon))
Density.forecast_total.Unrecon_df <- DD_Total_Unrecon %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.Unrecon_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_Unrecon_Gauss

DD_Total_BU <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.BU))
Density.forecast_total.BU_df <- DD_Total_BU %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.BU_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("Reconciled-BU") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_BU_Gauss

DD_Total_MinT <- data.frame(Time_fc=Time_forecast, t(Density.forecast_total.Mint.shr))
Density.forecast_total.Mint.shr_df <- DD_Total_MinT %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_total.Mint.shr_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_total_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Total")) +
  scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("Overnight Trips") + 
  xlab("Time") + 
  ggtitle("MinT(Shrink)") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_total_MinT_Gauss


##--States--##

# Visualising only Queensland, Tasmania and Vistoria

AllTS_states_df <- AllTS %>% 
  select(Time, B, C, `F`) %>% 
  dplyr::rename(Time = `Time`, Victoria = `B`, Queensland = `C`, Tasmania = `F`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

DD_B_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.Mint.shr))
DD_C_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.Mint.shr))
DD_F_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.Mint.shr))

Density.forecast_B.Mint.shr_df <- DD_B_MinT %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.Mint.shr_df <- DD_C_MinT %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_F.Mint.shr_df <- DD_F_MinT %>% gather(key=Sim, value=Tasmania, -Time_fc)

Density.forecast_States_Mint.shr <- data.frame(Density.forecast_B.Mint.shr_df,
                                               Queensland = Density.forecast_C.Mint.shr_df[,'Queensland'], 
                                               Tasmania = Density.forecast_F.Mint.shr_df[,'Tasmania'])

Density.forecast_States_Mint.shr_df <- Density.forecast_States_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                              colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("MinT(Shrink)") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_MinT_Gauss


#OLS

DD_B_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.OLS))
DD_C_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.OLS))
DD_F_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.OLS))

Density.forecast_B.OLS_df <- DD_B_OLS %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.OLS_df <- DD_C_OLS %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_F.OLS_df <- DD_F_OLS %>% gather(key=Sim, value=Tasmania, -Time_fc)

Density.forecast_States_OLS <- data.frame(Density.forecast_B.OLS_df,
                                          Queensland = Density.forecast_C.OLS_df[,'Queensland'], 
                                          Tasmania = Density.forecast_F.OLS_df[,'Tasmania'])

Density.forecast_States_OLS_df <- Density.forecast_States_OLS %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_OLS_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_OLS_Gauss

#BU

DD_B_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.BU))
DD_C_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.BU))
DD_F_BU <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.BU))

Density.forecast_B.BU_df <- DD_B_BU %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.BU_df <- DD_C_BU %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_F.BU_df <- DD_F_BU %>% gather(key=Sim, value=Tasmania, -Time_fc)

Density.forecast_States_BU <- data.frame(Density.forecast_B.BU_df,
                                         Queensland = Density.forecast_C.BU_df[,'Queensland'], 
                                         Tasmania = Density.forecast_F.BU_df[,'Tasmania'])

Density.forecast_States_BU_df <- Density.forecast_States_BU %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_BU_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Bottom up") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal")-> Plot_states_BU_Gauss

#Unrecon

DD_B_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_B.Unrecon))
DD_C_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_C.Unrecon))
DD_F_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_F.Unrecon))

Density.forecast_B.Unrecon_df <- DD_B_Unrecon %>% gather(key=Sim, value=Victoria, -Time_fc)
Density.forecast_C.Unrecon_df <- DD_C_Unrecon %>% gather(key=Sim, value=Queensland, -Time_fc)
Density.forecast_F.Unrecon_df <- DD_F_Unrecon %>% gather(key=Sim, value=Tasmania, -Time_fc)

Density.forecast_States_Unrecon <- data.frame(Density.forecast_B.Unrecon_df,
                                              Queensland = Density.forecast_C.Unrecon_df[,'Queensland'], 
                                              Tasmania = Density.forecast_F.Unrecon_df[,'Tasmania'])

Density.forecast_States_Unrecon_df <- Density.forecast_States_Unrecon %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Unrecon_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_states_df, linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_states_Unrecon_Gauss

# A function to get the legend from a plot

g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_states_Unrecon_Gauss)

grid.arrange( arrangeGrob(Plot_states_Unrecon_Gauss + theme(legend.position="none"),
                          Plot_states_OLS_Gauss + theme(legend.position="none"),
                          Plot_states_MinT_Gauss + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 


##--Zones--##

# Visualising only AD-South NSW, BD-North East VIC, CB-Central Coast QLD and 
# DA-Metro SA 

AllTS_Zones_df <- AllTS %>% 
  select(Time, AD, BD, CB, DA) %>% 
  dplyr::rename(Time = `Time`, 'South.NSW' = `AD`, 'North.East.VIC' = `BD`, 
                'Central.Coast.QLD' = `CB`, 'Metro.SA' = `DA`) %>%
  gather(key = Series, value = Observed, -Time)

#MinT

DD_AD_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.Mint.shr))
DD_BD_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.Mint.shr))
DD_CB_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.Mint.shr))
DD_DA_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.Mint.shr))

Density.forecast_AD.Mint.shr_df <- DD_AD_MinT %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.Mint.shr_df <- DD_BD_MinT %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.Mint.shr_df <- DD_CB_MinT %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.Mint.shr_df <- DD_DA_MinT %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_Mint.shr <- data.frame(Density.forecast_AD.Mint.shr_df,
                                               'North.East.VIC' = Density.forecast_BD.Mint.shr_df[,'North.East.VIC'], 
                                               'Central.Coast.QLD' = Density.forecast_CB.Mint.shr_df[,'Central.Coast.QLD'], 
                                               'Metro.SA' = Density.forecast_DA.Mint.shr_df[,'Metro.SA'])

Density.forecast_States_Mint.shr_df <- Density.forecast_States_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("MinT(Shrink)") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_MinT_Gauss


#OLS

DD_AD_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.OLS))
DD_BD_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.OLS))
DD_CB_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.OLS))
DD_DA_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.OLS))

Density.forecast_AD.OLS_df <- DD_AD_OLS %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.OLS_df <- DD_BD_OLS %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.OLS_df <- DD_CB_OLS %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.OLS_df <- DD_DA_OLS %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_OLS <- data.frame(Density.forecast_AD.OLS_df,
                                          'North.East.VIC' = Density.forecast_BD.OLS_df[,'North.East.VIC'], 
                                          'Central.Coast.QLD' = Density.forecast_CB.OLS_df[,'Central.Coast.QLD'], 
                                          'Metro.SA' = Density.forecast_DA.OLS_df[,'Metro.SA'])

Density.forecast_States_OLS_df <- Density.forecast_States_OLS %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_OLS_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("OLS") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_OLS_Gauss


# Incoherent 

DD_AD_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_AD.Unrecon))
DD_BD_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_BD.Unrecon))
DD_CB_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_CB.Unrecon))
DD_DA_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_DA.Unrecon))

Density.forecast_AD.Unrecon_df <- DD_AD_Unrecon %>% gather(key=Sim, value='South.NSW', -Time_fc)
Density.forecast_BD.Unrecon_df <- DD_BD_Unrecon %>% gather(key=Sim, value='North.East.VIC', -Time_fc)
Density.forecast_CB.Unrecon_df <- DD_CB_Unrecon %>% gather(key=Sim, value='Central.Coast.QLD', -Time_fc)
Density.forecast_DA.Unrecon_df <- DD_DA_Unrecon %>% gather(key=Sim, value='Metro.SA', -Time_fc)

Density.forecast_States_Unrecon <- data.frame(Density.forecast_AD.Unrecon_df,
                                              'North.East.VIC' = Density.forecast_BD.Unrecon_df[,'North.East.VIC'], 
                                              'Central.Coast.QLD' = Density.forecast_CB.Unrecon_df[,'Central.Coast.QLD'], 
                                              'Metro.SA' = Density.forecast_DA.Unrecon_df[,'Metro.SA'])

Density.forecast_States_Unrecon_df <- Density.forecast_States_Unrecon %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_States_Unrecon_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1998, 2019), 
                  breaks = c(1998, 2002, 2006, 2010, 2014, 2018)) + 
  geom_line(data = AllTS_Zones_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                             colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("Overnight Trips") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Zones_Unrecon_Gauss


g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_Zones_Unrecon_Gauss)

grid.arrange( arrangeGrob(Plot_Zones_Unrecon_Gauss + theme(legend.position="none"),
                          Plot_Zones_OLS_Gauss + theme(legend.position="none"),
                          Plot_Zones_MinT_Gauss + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 

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
             ncol = 1, mylegend, heights = c(20,1))
