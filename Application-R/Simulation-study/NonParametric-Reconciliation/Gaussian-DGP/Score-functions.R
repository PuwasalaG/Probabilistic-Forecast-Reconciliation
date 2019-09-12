#Calculating Enery score

Energy_score <- function(Data, Rel)
{
  Rel <- as.numeric(Rel)
  n <- ncol(Data)
  B <- nrow(Data)
  
  d1_eval <- Data - matrix(rep(Rel,B),B,n,byrow = TRUE)
  ES_1_eval <- apply(d1_eval, 1, function(x) sqrt(sum(x^2)))
  
  d2_eval <- Data[1:(B-1),] - Data[2:B,]
  
  ES_2_eval <- apply(d2_eval, 1, function(x) sqrt(sum(x^2)))
  ES_eval <- mean(ES_1_eval)-mean(ES_2_eval)/2
  
  return(ES_eval)
  
}


#Calculating Variogram score

Variogram_score<-function(Data, rel)
{
  
  Y_tilde_diff <- t(apply(Data, 1, function(x)
    (abs(outer(x,x,"-")[lower.tri(outer(x,x,"-"))]))^(0.5)))
  
  Y_tilde_diff_mean <- apply(Y_tilde_diff, 2, mean)
  
  z <- abs(outer(rel,rel,'-'))
  
  y_diff <- z[lower.tri(z)]
  y_diff <- y_diff^0.5
  
  C<-(y_diff - Y_tilde_diff_mean)^2
  
  sum(C)
}