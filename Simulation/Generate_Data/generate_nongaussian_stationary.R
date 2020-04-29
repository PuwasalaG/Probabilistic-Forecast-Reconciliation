# This code generates bottom level time series for the 
# simulation study from a non-Gaussian DGP
require(portes)
require(MASS)
require(copula)
require(tidyr)
require(tsibble)

set.seed(1989)

m <- 4 # Number of bottom level

init<- 500 #Number of initial values to be removed
train<-500 # Size of training sample
H <- 3 #Maximum forecast horizon
L <- 4 #Pre-sample
R <- 1000 #Number of reps

N <- train+R+init+H+L-1 # Sample size


#Randomly generating errors from a Gumbel copula with Beta margins

Gumbel.copula1 <- gumbelCopula(param = 10)
Gumbel.copula2 <- gumbelCopula(param = 8)

Z1 <- rCopula(N, Gumbel.copula1)
Z2 <- rCopula(N, Gumbel.copula2)

E <- matrix(NA, N, m)
E[,1] <- qbeta(Z1[,1], shape1 = 1, shape2 = 3)
E[,2] <- qbeta(Z1[,2], shape1 = 1, shape2 = 3)

E[,3] <- qbeta(Z2[,1], shape1 = 1, shape2 = 3)
E[,4] <- qbeta(Z2[,2], shape1 = 1, shape2 = 3)

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
      statinv<-tryCatch(InvertQ(MA_coef)) #This will produce a warning if MA non-stationary-invertible
    }
    statinv<-1
  }
  MA_coef_store[i,1:length(MA_coef)]<-MA_coef #Keep MA coefficients
  
  Bottom_level[,i] <- arima.sim(list(order=c(order_p[i],order_d[i],order_q[i]),
                                     ar=AR_coef, ma=MA_coef), n = (N+5), 
                                innov = E[,i])[2:(N+1)]
  
}

# Eliminate a proportion of initial values

Bottom_level <- Bottom_level[-(1:init),] 

#Generate noise to add to series to ensure bottom levels are noisier than top levels

Vt<-rnorm(n = N-init, mean = 0, sd = sqrt(10))
Wt<-rnorm(n = N-init, mean = 0, sd = sqrt(7))


AA<-Bottom_level[,1]+Vt-0.5*Wt
AB<-Bottom_level[,2]-Vt-0.5*Wt
BA<-Bottom_level[,3]+Vt+0.5*Wt
BB<-Bottom_level[,4]-Vt+0.5*Wt

#Find aggregates


A=AA+AB
B=BA+BB

Tot=A+B

#Put into a wide format (for export to csv)
wide<-tibble(Time=1:(N-init),Tot,A,B,AA,AB,BA,BB)

write.csv(wide, "../Data/nongaussian_stationary.csv",row.names = F)

save(AR_coef_store,MA_coef_store,order_d,file = 'model_coefficients_ns.RData')


