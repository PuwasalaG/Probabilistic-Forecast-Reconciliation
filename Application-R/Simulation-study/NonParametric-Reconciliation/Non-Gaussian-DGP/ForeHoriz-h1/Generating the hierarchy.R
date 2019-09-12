
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

N <- 2500
L <- 500
r <- 500
m <- 4
B <- 5000
H <- 1


#Randomly generating errors from a Gumbel copula 
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
    MA_coef<-0
  } else {
    MA_coef <- runif(n=order_q[i], min = 0.3, max = 0.7)
  }
  
  
  Bottom_level[,i] <- arima.sim(list(order=c(order_p[i],order_d[i],order_q[i]),
                                     ar=AR_coef, ma=MA_coef), n = (N+5), 
                                innov = E[,i])[2:(N+1)]
  
}


Vt<-rnorm(n = N, mean = 0, sd = sqrt(10))
Wt<-rnorm(n = N, mean = 0, sd = sqrt(7))

Bottom_level_noisy<-matrix(0, nrow = N, ncol = m)

Bottom_level_noisy[,1]<-Bottom_level[,1]+Vt-0.5*Wt
Bottom_level_noisy[,2]<-Bottom_level[,2]-Vt-0.5*Wt
Bottom_level_noisy[,3]<-Bottom_level[,3]+Vt+0.5*Wt
Bottom_level_noisy[,4]<-Bottom_level[,4]-Vt+0.5*Wt


#Generating the hierarchy

Hierarchy<-suppressMessages(hts(Bottom_level_noisy, list(2, c(2,2))))
AllTS<-allts(Hierarchy)
n<-ncol(AllTS)

write.csv(Bottom_level_noisy, "Bottom_level.csv")
