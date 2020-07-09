# This code generates bottom level time series for the 
# simulation study from a Gaussian stationary DGP
require(portes)
require(MASS)
require(tidyr)
require(tsibble)



set.seed(1989) #Set seed

m <- 4 # Number of bottom level

init<- 500 #Number of initial values to be removed
train<-500 # Size of training sample
H <- 3 #Maximum forecast horizon
L <- 4 #Pre-sample
R <- 1000 #Number of reps

N <- train+R+init+H+L-1 # Sample size

#Randomly generating errors from a Gaussian distribution 

Bottom_pop_cov<-matrix(c(5,3.1,0.6,0.4,3.1,4,0.9,1.4,0.6,
                         0.9,2,1.8,0.4,1.4,1.8,3), nrow = m, 
                       ncol = m)  #Covariance matrix

E <- mvrnorm(n = N+5, mu = rep(0, m), Sigma = Bottom_pop_cov) #Generate MVN disturbances

#Generating the bottom level series. Each series were generated from 
#ARMA(p,d,q) model where the parameters were randomly selected from the
#defined parameter space

order_p <- sample(c(1,2), size = m, replace = TRUE)
order_d <- sample(c(0), size = m, replace = TRUE)
order_q <- sample(c(1,2), size = m, replace = TRUE)


Bottom_level <- matrix(0, nrow = N,  ncol = m)

AR_coef_store<-matrix(0,m,2)
MA_coef_store<-matrix(0,m,2)

statinv<-1 #flag for stationarity and invertibility

for (i in 1:m)
{
  
  if (order_p[i]==0) {
    AR_coef <- 0
  } else {
    while(!is.null(statinv)){
      AR_coef <- runif(n=order_p[i], min = 0.3, max = 0.5)
      statinv<-tryCatch(InvertQ(AR_coef)) #This will produce a warning if AR non-stationary
    }
    statinv<-1
  }
  
  
  AR_coef_store[i,1:length(AR_coef)]<-AR_coef #Keep AR coefficients
  
  if (order_q[i]==0) {
    MA_coef <- 0
  } else {
    while(!is.null(statinv)){
      MA_coef <- runif(n=order_q[i], min = 0.3, max = 0.7)
      statinv<-tryCatch(InvertQ(MA_coef)) #This will produce a warning if AR non-stationary
    }
    statinv<-1
  }
  MA_coef_store[i,1:length(MA_coef)]<-MA_coef #Keep AR coefficients
  
  
  Bottom_level[,i] <- arima.sim(list(order=c(order_p[i],order_d[i],order_q[i]),
                                     ar=AR_coef, ma=MA_coef), n = (N+5), 
                                innov = E[,i])[2:(N+1)]
  
}

# Eliminate a proportion of initial values

Bottom_level <- Bottom_level[-(1:init),] 

Ut <- rnorm(n = N-init, mean = 0, sd = sqrt(28)) #u_t
Vt <- rnorm(n = N-init, mean = 0, sd = sqrt(22)) #v_t

##Inequality Check

VTot<-sum(Bottom_pop_cov)
VA<-sum(Bottom_pop_cov[1:2,1:2])+22
VB<-sum(Bottom_pop_cov[3:4,3:4])+22
VAA<-Bottom_pop_cov[1,1]+22/4+28
VAB<-Bottom_pop_cov[2,2]+22/4+28
VBA<-Bottom_pop_cov[3,3]+22/4+28
VBB<-Bottom_pop_cov[4,4]+22/4+28

if(VTot>VA){print('VTot larger than VA')}
if(VTot>VB){print('VTot larger than VB')}
if(VA>VAA){print('VA larger than VA')}
if(VA>VAB){print('VA larger than VAB')}
if(VB>VBA){print('VB larger than VBA')}
if(VB>VBB){print('VB larger than VBB')}

AA <- Bottom_level[,1]+Ut-0.5*Vt
AB <- Bottom_level[,2]-Ut-0.5*Vt
BA <- Bottom_level[,3]+Ut+0.5*Vt
BB <- Bottom_level[,4]-Ut+0.5*Vt

#Find aggregates

A=AA+AB
B=BA+BB

Tot=A+B


#Put into a wide format (for export to csv)

wide<-tibble(Time=1:(N-init),Tot,A,B,AA,AB,BA,BB)
write.csv(wide, "../Data/gaussian_stationary.csv",row.names = F)


save(AR_coef_store,MA_coef_store,file = 'model_coefficients_gs.RData')
