#This script selects and fits base models for each window. 
#The forecast mean, forecast standard deviation, two estimates of the covariance matrix and residuals are retained.
library(magrittr)
library(tidyverse)
library(tsibble)
library(fable)
library(BigVAR)

#Clear workspace
rm(list=ls())

#j<- 36 #If running within R uncomment this.  This will only run one window
j<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this should go from 1 to J-L-N+1


#Function for Shaefer Strimmer Shrinkage

shrink.estim <- function(res)
{
  n<-nrow(res)
  covm <- cov(res)
  tar <- diag(diag(covm))
  corm <- stats::cov2cor(covm)
  xs <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  xs <- xs[stats::complete.cases(xs),]
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- stats::cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  W <- lambda * tar + (1 - lambda) * covm
  return(W)
}

# Order of variables

order<-c("Total","non-Renewable","Renewable",
         "Coal","Gas","Solar",
         'Hydro (inc. Pumps)',"Battery",
         "Black Coal","Brown Coal",
         "Gas (CCGT)", "Gas (OCGT)", "Gas (Reciprocating)", "Gas (Steam)",
         "Solar (Rooftop)","Solar (Utility)",
         "Hydro", "Pumps",
         "Battery (Charging)", "Battery (Discharging)",
         "Distillate","Biomass","Wind")


#Define S Matrix
S<-matrix(c(rep(1,15),#All
            c(rep(1,6),rep(0,6),1,rep(0,2)), #non-Renewables
            c(rep(0,6),rep(1,6),0,rep(1,2)), # Renewables
            c(rep(1,2),rep(0,13)), # Coal
            c(rep(0,2),rep(1,4),rep(0,9)), #Gas
            c(rep(0,6),rep(1,2),rep(0,7)), #Solar
            c(rep(0,8),rep(1,2),rep(0,5)), # Hydro
            c(rep(0,10),rep(1,2),rep(0,3)) # Battery
),8,15,byrow = T)

S<-rbind(S,diag(1,15))



#Read in data
data<-readRDS('./Data/nem_generation_by_source.rds')
data <- data %>% 
  as_tibble() %>% 
  mutate(Source = factor(Source, levels = order)) %>% 
  spread(key = Source, value = Generation) 
  
# arrange(data,match(Source,order))->datlong

#Sample sizes
N<-140 # Size of window
L<-0 # Lags to leave at beginning of window

startdate<-min(data$date)
enddate<-max(data$date)
alldate<-seq.Date(startdate,enddate,by=1)

J<-length(alldate)
range_j <- 1:226

#Function to forecast window j
forecast_j<-function(j){
  dat_j<-data %>% 
    filter((date>=alldate[j])&(date<=alldate[N+j-1])) %>% 
    select(-date) %>% 
    as.matrix()
  
  Model <- constructModel(dat_j, p = 7, struct = "Basic",
                           gran = c(50, 10))
  results <- cv.BigVAR(Model)
  
  #Residuals (as matrix)
  results@resids -> r

  # #Data (as matrix)
  # datlong%>%  
  #   pivot_wider(id_cols = date,names_from = Source,values_from = Generation)%>%
  #   select(-date)%>%
  #   as.matrix()->y
  # 
  # y<-y[(L+1):(L+N),] #remove NA rows
  
  #Forecast mean as a vector
  fc_mean <- predict(results, n.ahead = 1) %>% 
    as.vector()
  
  #Find Sigma (sample and shrink)
  Sigma_sam<-cov(r)
  Sigma_shr<-shrink.estim(r)
  fc_sd<-diag(Sigma_sam)
  
  return(list(fc_mean=fc_mean,
              fc_sd=fc_sd,
              fc_Sigma_sam=Sigma_sam,
              fc_Sigma_shr=Sigma_shr,
              resid=t(r)))
  
  
}

out<-forecast_j(j)

#Save output
saveRDS(out,paste0('../Base_Results/base_',N+L+j-1,'.rds'))



# #Save output
# saveRDS(mable,paste0('../Base_Results/mable_',N+L+j-1,'.rds'))


# autoplot(filter(f,Source%in%c('Total','Renewable','non-Renewable')),
#                 filter(data,Source%in%c('Total','Renewable','non-Renewable')))
# 
# autoplot(filter(f,Source%in%c('Coal','Gas','Distillate','Battery','Hydro (inc. Pumps)','Solar','Wind','Biomass')),
#          filter(data,Source%in%c('Coal','Gas','Distillate','Battery','Hydro (inc. Pumps)','Solar','Wind','Biomass')))
# 
# autoplot(filter(f,Source%in%c('Solar (Rooftop)','Solar (Utility)')),
#          filter(data,Source%in%c('Solar (Rooftop)','Solar (Utility)')))
# 
# autoplot(filter(f,Source%in%c('Battery (Charging)','Battery (Discharging)')),
#          filter(data,Source%in%c('Battery (Charging)','Battery (Discharging)')))
# 
# autoplot(filter(f,Source%in%c('Black Coal','Brown Coal')),
#          filter(data,Source%in%c('Black Coal','Brown Coal')))
# 
# autoplot(filter(f,Source%in%c('Hydro','Pumps')),
#          filter(data,Source%in%c('Hydro','Pumps')))
# 
# autoplot(filter(f,Source%in%c('Gas (CCGT)','Gas (OCGT)','Gas (Reciprocating)','Gas (Steam)')),
#          filter(data,Source%in%c('Gas (CCGT)','Gas (OCGT)','Gas (Reciprocating)','Gas (Steam)')))
# 

# left_join(datlong,rlong)%>%
#   mutate(Predicted=Generation-.resid)%>%
#   rename(Realised=Generation)%>%
#   select(-.model,-.resid)%>%
#   pivot_longer(cols = c(-date,-Source),names_to = 'Value',values_to='Generation')%>%
#   ggplot(aes(x=date,y=Generation,col=Value))+geom_line()+
#   facet_wrap(~Source,nrow = 6,ncol = 4,scales = 'free_y')


