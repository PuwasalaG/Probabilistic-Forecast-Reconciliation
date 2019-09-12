
# 
# for(k in 1:B)
# {
#   Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
#   
#   for(i in 1:n)
#   {
#     Future_paths[,i] <- simulate(fit[[i]], nsim = H, future = TRUE,
#                                  innov = Residuals_all[Index_seq[k,],i])
#   }
#   Unrecon_future_paths_h1[k,]<-Future_paths[1,]
#   Unrecon_future_paths_h2[k,]<-Future_paths[2,]
#   Unrecon_future_paths_h3[k,]<-Future_paths[3,]
#   Unrecon_future_paths_h4[k,]<-Future_paths[4,]
#   Unrecon_future_paths_h5[k,]<-Future_paths[5,]
#   
# }

for (k in 1:B) {
  
  Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
  
}

Future_paths<-matrix(0, nrow = H, ncol = n)

for(i in 1:n)
{
  Future_paths[,i] <- simulate(fit_training[[i]], nsim = H, future = TRUE,
                               innov = Residuals_all_training[Index_seq[k,],i])
}


Innov <- as.list(as.data.frame(Residuals_all_training[Index_seq[k,],]))

FP <- mapply(simulate, fit_training, future = TRUE, nsim = H, innov = Innov)

mylist <- list(object = fit_training, innov = Innov)



FP_train_func1 <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the
  #k^th future path for H forecast horizons for all n series. In the returning matrix,
  #rows represents forecast horizons columns represents the series
  
  fit_training <- fit
  Residuals_all_training <- Resid
  Index <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(Residuals_all_training[Index_seq[k,],]))
  
  return(mapply(simulate, fit_training, future = TRUE, nsim = H, innov = Innov))
  
}

Start_FP_lapply <- Sys.time()
future_paths1 <-  lapply(c(1:B), FP_train_func1, fit = fit_training, 
                         Resid = Residuals_all_training, Index = Index, 
                         Index_seq = Index_seq, H=H, n=n)
End_FP_lapply <- Sys.time()


FP_train_func2 <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the
  #k^th future path for H forecast horizons for all n series. In the returning matrix,
  #rows represents forecast horizons columns represents the series
  
  fit_training <- fit
  Residuals_all_training <- Resid
  Index <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(Residuals_all_training[Index_seq[k,],]))
  mylist <- list(object = fit_training, innov = Innov)
  
  pmap_dfc(mylist, simulate, future = TRUE, nsim = H) %>% as.matrix() %>% return()
  
}

Start_FP_lapply_2 <- Sys.time()
future_paths2 <-  purrr::map(c(1:B), FP_train_func2, fit = fit_training, 
                             Resid = Residuals_all_training, Index = Index, 
                             Index_seq = Index_seq, H=H, n=n)
End_FP_lapply_2 <- Sys.time()

FP_train_func3 <- function(k, fit, Resid, Index, Index_seq, H, n) { #This function will return the
  #k^th future path for H forecast horizons for all n series. In the returning matrix,
  #rows represents forecast horizons columns represents the series
  
  fit_training <- fit
  Residuals_all_training <- Resid
  Index <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(Residuals_all_training[Index_seq[k,],]))
  mylist <- list(object = fit_training, innov = Innov)
  
  furrr::future_pmap_dfc(mylist, simulate, future = TRUE, nsim = H) %>% as.matrix() %>% return()
  
}

Start_FP_lapply_3 <- Sys.time()
future_paths3 <-  furrr::future_map(c(1:B), FP_train_func3, fit = fit_training, 
                                    Resid = Residuals_all_training, Index = Index, 
                                    Index_seq = Index_seq, H=H, n=n)
End_FP_lapply_3 <- Sys.time()

