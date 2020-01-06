#This file contains optimisation functions written for:
  #Method 1: Estimating W
  #Method 2: Estimating through the reparameterisation using cholesky decomposition
  #Method 3: Estimating P directly, imposing the constraint PS=I



#Method 1: Estimating W

Energy_score_method1 <- function(Vec_W, Future_paths, Test, n1, B1, r1, S1)
{
  Inv_W <- miscTools::symMatrix(Vec_W, nrow = n1, byrow = FALSE)
  a <- t(S1)%*%Inv_W%*%S1
  b <- t(S1)%*%Inv_W
  P <- solve(a, b)
  
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
  
  ES_Reconciled <- numeric(r1)
  
  for (q in 1:r1)
  {
    
    d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
    
    ES_1 <- sqrt(rowSums(d1^2))
    
    d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
    
    ES_2 <- sqrt(rowSums(d2^2))
    
    ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
    
  }
  
  mean(ES_Reconciled)
  
}

  #Function to return the gradient

Grad_method1 <- function(x, Future_paths, Test, n1, B1, r1, S1)
{
  Inv_W <- miscTools::symMatrix(x, nrow = n1, byrow = FALSE)
  
  a <- t(S1)%*%Inv_W%*%S1
  b <- t(S1)%*%Inv_W
  P <- solve(a, b)
  Inv <- solve(a)
  
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
  
  ES_grad <- list(r1)
  
  for (q in 1:r1)
  {
    d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
    es_1 <- sqrt(rowSums(d1^2))
    
    d1 <- d1/es_1
    
    D1 <- S1 %*% Inv %*% t(S1) %*% t(d1) %*% Future_paths[[q]] %*% (diag(1,n1,n1) - t(Proj.mat))
    
    b2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
    es_2 <- sqrt(rowSums(na.omit(b2)^2))
    d2 <- Future_paths[[q]][1:(B1-1),] - Future_paths[[q]][2:B1,]
    
    d21 <- (d2/es_2)
    d21[is.na(d21)] <- 0
    
    D2 <- S1 %*% Inv %*% t(S1) %*% Proj.mat %*% t(d21) %*% d2 %*% (diag(1,n1,n1) - t(Proj.mat))
    
    ES_grad[[q]] <- D1/nrow(d1) - D2/(2*nrow(d21))
    
  }
  
  mean.W <- popbio::mean.list(ES_grad)
  
  return(gdata::upperTriangle(mean.W, diag = TRUE, byrow = TRUE))
  
}


#Method 2: Estimating through the reparameterisation using cholesky decomposition

Energy_score_method2 <- function(Vec_cholesky,  Future_paths, Test, n1, B1, r1, S1)
{
  R <- matrix(0, n1, n1)
  gdata::upperTriangle(R, diag = TRUE, byrow=TRUE) <- Vec_cholesky
  
  Inv_W <- t(R)%*%R
  a <- t(S1)%*%Inv_W%*%S1
  b <- t(S1)%*%Inv_W
  P <- solve(a, b)
  
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
  
  ES_Reconciled <- numeric(r1)
  
  for (q in 1:r1)
  {
    
    d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
    
    ES_1 <- sqrt(rowSums(d1^2))
    
    d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
    
    ES_2 <- sqrt(rowSums(d2^2))
    
    ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
    
  }
  
  
  mean(ES_Reconciled)
  
}


#Method 2b: Estimating through the reparameterisation using cholesky decomposition - restricted for rotations

Energy_score_method2b <- function(Vec_cholesky, Future_paths, Test, n1, B1, r1, S1)
{
  Vec_cholesky <- c(1, Vec_cholesky)
  R <- matrix(0, n1, n1)
  gdata::upperTriangle(R, diag = TRUE, byrow=TRUE) <- Vec_cholesky
  #diag(R) <- c(rep(1, n1))
  
  Inv_W <- t(R)%*%R
  a <- t(S1)%*%Inv_W%*%S1
  b <- t(S1)%*%Inv_W
  P <- solve(a, b)
  
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
  
  ES_Reconciled <- numeric(r1)
  
  for (q in 1:r1)
  {
    
    d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
    
    ES_1 <- sqrt(rowSums(d1^2))
    
    d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
    
    ES_2 <- sqrt(rowSums(d2^2))
    
    ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
    
  }
  
  mean(ES_Reconciled)
  
}


#Method 3: Estimating P directly, imposing the constraint PS=I

#Following function calculates the Energy score of coherent future paths
#where the future paths are reconciled by a suitable P matrix 

Energy_score_method3 <- function(Vec_P, Future_paths, Test, m1, n1, B1, r1, S1)
{
  P <- matrix(Vec_P, m1, n1, byrow = TRUE)
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- lapply(Future_paths, function(x) t(Proj.mat%*%t(x)))
  
  ES_Reconciled <- numeric(r1)
  
  for (q in 1:r1)
  {
    
    d1 <- Reconciled_future_paths[[q]] - matrix(rep(Test[q,],B1),B1,n1,byrow = TRUE)
    ES_1 <- sqrt(rowSums(d1^2))
    
    d2 <- Reconciled_future_paths[[q]][1:(B1-1),] - Reconciled_future_paths[[q]][2:B1,]
    
    ES_2 <- sqrt(rowSums(d2^2))
    ES_Reconciled[q] <- mean(ES_1) - mean(ES_2)/2
    
  }
  
  
  mean(ES_Reconciled)
  
}

  #Function that imposes the constraint PS=I into the optimization.

eqn1 <- function(Vec_P, Future_paths, Test, m1, n1, B1, r1, S1)
{
  List <- lapply(1:m1, function(x) t(S1))
  D <- bdiag(List)
  A <-  D %*% Vec_P 
  
  return(rep(t(A)))
}

