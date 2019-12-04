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

rnorm_degenerate <- function(mu, Sigma, k, n)
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

rnorm_degenerate <- function(mu, Sigma, n, k)
{
  Sigma <- matrix(Sigma, n, n)
  SVD <- svd((Sigma + t(Sigma))/2)
  
  SVD$d <- abs(zapsmall(SVD$d))
  U <- SVD$u
  D <- diag(sqrt(SVD$d))
  m1 <- sum(SVD$d > 0)
  
  X <- rmvnorm(k, mean = rep(0, m1), diag(1,m1,m1))
  X <- cbind(X, matrix(0, nrow = k, ncol = n-m1))
  
  Mu <- matrix(rep(mu, k), k, n, byrow = TRUE)
  
  Y <- t(U %*% D %*% t(X)) + Mu
  
  return(Y)
}

X_BU<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_BU, 
              Sigma = Sigma.tilde_BU, k = k, n = n)

X_OLS<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_OLS, 
               Sigma = Sigma.tilde_OLS, k = k, n = n)

X_MinT.shr<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.shr, 
                    Sigma = Sigma.tilde_MinT.shr, k = k, n = n)

X_MinT.sam<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.sam, 
                    Sigma = Sigma.tilde_MinT.sam, k = k, n = n)

X_MinT.wls<-rnorm_degenerate(n=k, mu=Reconciled_point.forecasts_Mint.wls, 
                    Sigma = Sigma.tilde_MinT.wls, k = k, n = n)


