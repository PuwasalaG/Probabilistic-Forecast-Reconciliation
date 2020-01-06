#This file contains optimisation functions written for:
  #Method 1: Estimating W


Variogram_score_method1 <- function(Vec_W, Future_paths, Test, n1, B1, r1, S1)
{
  Inv_W <- miscTools::symMatrix(Vec_W, nrow = n1, byrow = FALSE)
  a <- t(S1)%*%Inv_W%*%S1
  b <- t(S1)%*%Inv_W
  P <- solve(a, b)
  
  Proj.mat <- S1%*%P
  
  Reconciled_future_paths <- future_map(Future_paths, function(x) as.data.frame(t(Proj.mat%*%t(x))))
  
  VS_Reconciled <- numeric(r1)
  
  for (q in 1:r)
  {
    
    VS_Reconciled[q] <- (1/2) * vs_sample(y = Test[q,], dat = t(Reconciled_future_paths[[q]]), 
                                  p=0.5)
    
  }
  
  mean(VS_Reconciled)
  
}

# Diff <- function(dat)
# {
#   dat <- as.data.frame(dat)
#   combs <- combn(names(dat),2)
#   Y_tilde_diff <- abs(apply(combs,2,function(x) dat[[x[1]]]-dat[[x[2]]]))^(0.5)
# }
# 
# Variogram_score_method1 <- function(Vec_W, Future_paths, Test, n1, B1, r1, S1)
# {
#   Inv_W <- miscTools::symMatrix(Vec_W, nrow = n1, byrow = FALSE)
#   a <- t(S1)%*%Inv_W%*%S1
#   b <- t(S1)%*%Inv_W
#   P <- solve(a, b)
#   
#   Proj.mat <- S1%*%P
#   
#   Reconciled_future_paths <- lapply(Future_paths, function(x) as.data.frame(t(Proj.mat%*%t(x))))
#   
#   Y_tilde_diff2 <- lapply(Reconciled_future_paths, Diff)
#   
#   VS_Reconciled <- numeric(r1)
#   
#   for (q in 1:r)
#   {
#     Y_tilde_diff_mean <- colSums(Y_tilde_diff2[[q]])/B1
#     
#     z <- abs(outer(Test[q,],Test[q,],'-'))
#     y_diff <- z[lower.tri(z)]
#     y_diff <- y_diff^(0.5)
#     
#     C <- (y_diff - Y_tilde_diff_mean)^2
#     
#     VS_Reconciled[q] <- sum(C)
#     
#   }
#   
#   mean(VS_Reconciled)
#   
# }


