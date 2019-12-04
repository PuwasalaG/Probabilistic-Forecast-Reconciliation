#This file contains optimisation functions written for:
  #Method 1: Estimating W
  #Method 2: Estimating through the reparameterisation using cholesky decomposition
  #Method 3: Estimating P directly, imposing the constraint PS=I


#Method 1: Estimating W

Obj_func_LS <- function(Vec_W, Sigma_hat, Mu_hat, Test, m1, n1, r1, S1){
  #Sigma_hat - a list of r1 covariance matrices
  #Mu_hat - a matrix of (r1xn) 
  #Test - a matrix of (r1xn)
  
  Inv_W <- miscTools::symMatrix(Vec_W, nrow = n1, byrow = FALSE)
  x <- t(S1)%*%Inv_W%*%S1
  y <- t(S1)%*%Inv_W
  G <- solve(x, y)
  
  LS_Reconciled <- numeric(r1)
  
  for (q in r1) {
    
    Omega_tilda <- G %*% Sigma_hat[[q]] %*% t(G)
    mu_tilda <- G %*% Mu_hat[q,]
    b <- Test[q,(n1-m1+1):n1]
    
    Eigen <- eigen(Omega_tilda)$values
    
    log_det <- log(prod(Eigen))
    Inv_cov <- solve(Omega_tilda)
    Mean_shift <- c(b - mu_tilda)
    
    LS_Reconciled[q] <- (1/2)*(log_det + t(Mean_shift)%*%Inv_cov%*%Mean_shift)
    
  }
  
  mean(LS_Reconciled)
  
}


