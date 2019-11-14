# This code will generate bottom level time series from Guassian DGP with some noise imposed

library("forecast")
require("Matrix")
require("hts")
require(miscTools)
require("MASS")
require("hydroGOF")
require("numDeriv")
require("psych")
require(Rsolnp)
require(VineCopula)
require(copula)

set.seed(1989)

N <- 2102
m <- 4


#Randomly generating errors from a Gaussian distribution 
Bottom_pop_cov<-matrix(c(5,3.1,0.6,0.4,3.1,4,0.9,1.4,0.6,
                         0.9,2,1.8,0.4,1.4,1.8,3), nrow = m, 
                       ncol = m)


E <- mvrnorm(n = N, mu = rep(0, m), Sigma = Bottom_pop_cov)

#Generating the bottom level series. Each series were generated from 
#ARMA(p,d,q) model where the parameters were randomly selected from the
#defined parameter space
order_p <- sample(c(1,2), size = m, replace = TRUE)
order_d <- sample(c(0,1), size = m, replace = TRUE)
order_q <- sample(c(1,2), size = m, replace = TRUE)


Bottom_level <- matrix(0, nrow = N,  ncol = m)

for (i in 1:m)
{
  
  if (order_p[i]==0) {
    AR_coef <- 0
  } else {
    AR_coef <- runif(n=order_p[i], min = 0.3, max = 0.5)
  }
  
  if (order_q[i]==0) {
    MA_coef <- 0
  } else {
    MA_coef <- runif(n=order_q[i], min = 0.3, max = 0.7)
  }
  
  
  Bottom_level[,i] <- arima.sim(list(order=c(order_p[i],order_d[i],order_q[i]),
                                     ar=AR_coef, ma=MA_coef), n = (N+5), 
                                innov = E[,i])[2:(N+1)]
  
}


Ut <- rnorm(n = N, mean = 0, sd = sqrt(28)) #u_t
Vt <- rnorm(n = N, mean = 0, sd = sqrt(22)) #v_t

Bottom_level_noisy <- matrix(0, nrow = N, ncol = m)

Bottom_level_noisy[,1] <- Bottom_level[,1]+Ut-0.5*Vt
Bottom_level_noisy[,2] <- Bottom_level[,2]-Ut-0.5*Vt
Bottom_level_noisy[,3] <- Bottom_level[,3]+Ut+0.5*Vt
Bottom_level_noisy[,4] <- Bottom_level[,4]-Ut+0.5*Vt

Bottom_level_noisy <- Bottom_level_noisy[-(1:500),] # To avoid impact from initial values

write.csv(Bottom_level_noisy, "Bottom_level.csv")
