##Generating random numbers from a degenerate Gaussian distribution


mu <- c(5, 5, 5)
Sigma <- matrix(c(1, 2, 1,
                  2, 3, 1,
                  1, 1, 0), 3)

# Analyze the covariance.

n <- dim(Sigma)[1]
s <- svd((Sigma + t(Sigma))/2) # Guarantee symmetry
s$d <- abs(zapsmall(s$d))
m <- sum(s$d > 0)
#$
# Generate a standard Normal `x` in R^m.
#
n.sample <- 1e3 # Number of points to generate
x <- matrix(rnorm(m*n.sample), nrow=m)
#
# Embed `x` in R^n and apply the square root of Sigma obtained from its SVD.
#
x <- rbind(x, matrix(0, nrow=n-m, ncol=n.sample))
y <- s$u %*% diag(sqrt(s$d)) %*% x + mu
#


#Function to generate random numbers from a degenerate Gaussian distribution
n<-3
k<-1000

rnorm_degenerate <- function(mu, Sigma)
{
  Sigma <- matrix(Sigma, n, n)
  SVD <- svd((Sigma + t(Sigma))/2)
  
  SVD$d <- abs(zapsmall(SVD$d))
  m1 <- sum(s$d > 0)
  
  x <- matrix(rnorm(m1*k), nrow=m1)
  
  x <- rbind(x, matrix(0, nrow=n-m1, ncol=k))
  y <- SVD$u %*% diag(sqrt(SVD$d)) %*% x + mu
  
}

XX<-rnorm_degenerate(mu,Sigma)


#Generating random numbers from the denerate predictive Gaussian distribution

rnorm_degenerate <- function(mu, Sigma)
{
  Sigma <- matrix(Sigma, n, n)
  SVD <- svd((Sigma + t(Sigma))/2)
  
  SVD$d <- abs(zapsmall(SVD$d))
  m1 <- sum(s$d > 0)
  
  x <- matrix(rnorm(m1*k), nrow=m1)
  
  x <- rbind(x, matrix(0, nrow=n-m1, ncol=k))
  y <- SVD$u %*% diag(sqrt(SVD$d)) %*% x + mu
  
}

X_BU<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_BU, 
              Sigma = Sigma.tilde_BU)

X_OLS<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_OLS, 
               Sigma = Sigma.tilde_OLS)

X_MinT.shr<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.shr, 
                    Sigma = Sigma.tilde_MinT.shr)

X_MinT.sam<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.sam, 
                    Sigma = Sigma.tilde_MinT.sam)

X_MinT.wls<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.wls, 
                    Sigma = Sigma.tilde_MinT.wls)


