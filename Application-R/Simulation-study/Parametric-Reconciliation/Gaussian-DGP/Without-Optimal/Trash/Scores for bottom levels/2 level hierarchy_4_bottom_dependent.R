require("knitr")
require("hydroGOF")
require("forecast")
require("MASS")
require("hts")
require("numDeriv")
require("miscTools")
require("matrixcalc")
require("clusterGeneration")
require("psych")
require("mvtnorm")
require("scoringRules")


start<-Sys.time()

#Let m be the number of bottom level series
#Let N be the size of the series
N<-510
L<-500
m<-4
C<-1000
k<-10000


#Generating bottom level covariance matrix

Bottom_pop_cov<-matrix(c(5,3.1,0.6,0.4,3.1,4,0.9,1.4,0.6,0.9,2,1.8,0.4,1.4,1.8,3), 
                       nrow = m, ncol = m)


#Initializing the variables. 
#P_BU : I_m
#P_OLS :  (S'S)^(-1)S'
#P_MinT.shr : (S'Inv_W S)^(-1)S'Inv_W
#P_MinT.sam : (S'Inv_sig.sam S)^(-1)S'nv_sig.sam


ES_Bottum.up<-numeric(0)
ES_OLS<-numeric(0)
ES_MinT.shr<-numeric(0)
ES_MinT.sam<-numeric(0)
ES_MinT.wls<-numeric(0)
ES_unreconciled<-numeric(0)

LS_Bottum.up<-numeric(0)
LS_OLS<-numeric(0)
LS_MinT.shr<-numeric(0)
LS_MinT.sam<-numeric(0)
LS_MinT.wls<-numeric(0)
LS_Unreconciled<-numeric(0)

VS_Bottum.up<-numeric(0)
VS_OLS<-numeric(0)
VS_MinT.shr<-numeric(0)
VS_MinT.sam<-numeric(0)
VS_MinT.wls<-numeric(0)
VS_Unreconciled<-numeric(0)

CRPS_Unrecon_Tot <- numeric(0)
CRPS_Unrecon_A <- numeric(0)
CRPS_Unrecon_B <- numeric(0)
CRPS_Unrecon_AA <- numeric(0)
CRPS_Unrecon_AB <- numeric(0)
CRPS_Unrecon_BA <- numeric(0)
CRPS_Unrecon_BB <- numeric(0)

CRPS_MinT_shr_Tot <- numeric(0)
CRPS_MinT_shr_A <- numeric(0)
CRPS_MinT_shr_B <- numeric(0)
CRPS_MinT_shr_AA <- numeric(0)
CRPS_MinT_shr_AB <- numeric(0)
CRPS_MinT_shr_BA <- numeric(0)
CRPS_MinT_shr_BB <- numeric(0)

CRPS_MinT_sam_Tot <- numeric(0)
CRPS_MinT_sam_A <- numeric(0)
CRPS_MinT_sam_B <- numeric(0)
CRPS_MinT_sam_AA <- numeric(0)
CRPS_MinT_sam_AB <- numeric(0)
CRPS_MinT_sam_BA <- numeric(0)
CRPS_MinT_sam_BB <- numeric(0)

CRPS_MinT_wls_Tot <- numeric(0)
CRPS_MinT_wls_A <- numeric(0)
CRPS_MinT_wls_B <- numeric(0)
CRPS_MinT_wls_AA <- numeric(0)
CRPS_MinT_wls_AB <- numeric(0)
CRPS_MinT_wls_BA <- numeric(0)
CRPS_MinT_wls_BB <- numeric(0)

CRPS_OLS_Tot <- numeric(0)
CRPS_OLS_A <- numeric(0)
CRPS_OLS_B <- numeric(0)
CRPS_OLS_AA <- numeric(0)
CRPS_OLS_AB <- numeric(0)
CRPS_OLS_BA <- numeric(0)
CRPS_OLS_BB <- numeric(0)

CRPS_BU_Tot <- numeric(0)
CRPS_BU_A <- numeric(0)
CRPS_BU_B <- numeric(0)
CRPS_BU_AA <- numeric(0)
CRPS_BU_AB <- numeric(0)
CRPS_BU_BA <- numeric(0)
CRPS_BU_BB <- numeric(0)


Log_Unrecon_Tot <- numeric(0)
Log_Unrecon_A <- numeric(0)
Log_Unrecon_B <- numeric(0)
Log_Unrecon_AA <- numeric(0)
Log_Unrecon_AB <- numeric(0)
Log_Unrecon_BA <- numeric(0)
Log_Unrecon_BB <- numeric(0)

Log_MinT_shr_Tot <- numeric(0)
Log_MinT_shr_A <- numeric(0)
Log_MinT_shr_B <- numeric(0)
Log_MinT_shr_AA <- numeric(0)
Log_MinT_shr_AB <- numeric(0)
Log_MinT_shr_BA <- numeric(0)
Log_MinT_shr_BB <- numeric(0)

Log_MinT_sam_Tot <- numeric(0)
Log_MinT_sam_A <- numeric(0)
Log_MinT_sam_B <- numeric(0)
Log_MinT_sam_AA <- numeric(0)
Log_MinT_sam_AB <- numeric(0)
Log_MinT_sam_BA <- numeric(0)
Log_MinT_sam_BB <- numeric(0)

Log_MinT_wls_Tot <- numeric(0)
Log_MinT_wls_A <- numeric(0)
Log_MinT_wls_B <- numeric(0)
Log_MinT_wls_AA <- numeric(0)
Log_MinT_wls_AB <- numeric(0)
Log_MinT_wls_BA <- numeric(0)
Log_MinT_wls_BB <- numeric(0)

Log_OLS_Tot <- numeric(0)
Log_OLS_A <- numeric(0)
Log_OLS_B <- numeric(0)
Log_OLS_AA <- numeric(0)
Log_OLS_AB <- numeric(0)
Log_OLS_BA <- numeric(0)
Log_OLS_BB <- numeric(0)

Log_BU_Tot <- numeric(0)
Log_BU_A <- numeric(0)
Log_BU_B <- numeric(0)
Log_BU_AA <- numeric(0)
Log_BU_AB <- numeric(0)
Log_BU_BA <- numeric(0)
Log_BU_BB <- numeric(0)


for(j in  1:C)
{
  
  #Randomly generating errors
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
  
  
  Vt <- rnorm(n = N, mean = 0, sd = sqrt(19)) #u_t
  Wt <- rnorm(n = N, mean = 0, sd = sqrt(18)) #v_t
  
  Bottom_level_noisy <- matrix(0, nrow = N, ncol = m)
  
  Bottom_level_noisy[,1] <- Bottom_level[,1]+Vt-0.5*Wt
  Bottom_level_noisy[,2] <- Bottom_level[,2]-Vt-0.5*Wt
  Bottom_level_noisy[,3] <- Bottom_level[,3]+Vt+0.5*Wt
  Bottom_level_noisy[,4] <- Bottom_level[,4]-Vt+0.5*Wt
  

#Generating the hierarchy

  Hierarchy <- suppressMessages(hts(Bottom_level_noisy, list(2, c(2,2))))
  AllTS <- allts(Hierarchy)
  n <- ncol(AllTS)
  
#Generating the summing matrix
  S <- smatrix(Hierarchy)

  Training <- AllTS[1:L,]
  Testing <- AllTS[(L+1):N,]
  
#Model fitting, forecasting and obtaining residuals

  h <- 1
  Residuals_all <- matrix(NA, nrow = nrow(Training), ncol = n)
  Base_forecasts <- matrix(NA, nrow = h, ncol = n)
  
  for(i in 1:n)
  {
    fit <- auto.arima(Training[,i])
    Base_forecasts[,i] <- forecast(fit, h=1)$mean
    Residuals_all[,i] <- Training[,i] - fitted(fit)
  }

  
  #Obtaining shrinkage estimator for var-cov matrix of in-sample errors 
  #(From MinT package)
  
  lowerD <- function(x)
  {
    n2 <- nrow(x)
    return(diag(apply(x, 2, crossprod) / n2))
  }
  
  shrink.estim <- function(x, tar)
  {
    if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
      stop("The data matrix must be numeric!")
    p <- ncol(x)
    n2 <- nrow(x)
    covm <- crossprod(x) / n2
    corm <- cov2cor(covm)
    xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
    v <- (1/(n2 * (n2 - 1))) * (crossprod(xs^2) - 1/n2 * (crossprod(xs))^2)
    diag(v) <- 0
    corapn <- cov2cor(tar)
    d <- (corm - corapn)^2
    lambda <- sum(v)/sum(d)
    lambda <- max(min(lambda, 1), 0)
    shrink.cov <- lambda * tar + (1 - lambda) * covm
    return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                              round(lambda, digits = 4))))
  }
  
  targ <- lowerD(Residuals_all)
  shrink <- shrink.estim(Residuals_all,targ)
  W.h <- shrink[[1]]
  Unreconsiled_shrinkage_cov_mat <- W.h
  
  #Calculating in-sample variance covariance matrix
  
  n1 <- nrow(Residuals_all)
  Sigma_sample <- crossprod(Residuals_all)/n1
  
  #Calculating WLS covariance matrix
  
  Sigma_WLS <- diag(diag(Sigma_sample),n,n)
  
  Inv_W.h <- solve(W.h)
  Inv_sig.sam <- solve(Sigma_sample)
  Inv_WLS <- solve(Sigma_WLS)
  
  #Calculating different P matrices
  
  Null.ma <- matrix(0,m,(n-m))
  P_BU <- cbind(Null.ma, diag(1,m,m))
  
  P_OLS <- solve(t(S)%*%S)%*%t(S)
  
  P_MinT.shr <- solve(t(S)%*%Inv_W.h%*%S)%*%t(S)%*%Inv_W.h
  
  P_MinT.sam <- solve(t(S)%*%Inv_sig.sam%*%S)%*%t(S)%*%Inv_sig.sam
  
  P_MinT.wls <- solve(t(S)%*%Inv_WLS%*%S)%*%t(S)%*%Inv_WLS
  
  
  #Obtainig reconciled bottom level point forecasts with different reconciliation
  #methods
  Reconciled_bottom_point.forecasts_BU <- P_BU%*%t(Base_forecasts)
  
  Reconciled_bottom_point.forecasts_Mint.shr <- P_MinT.shr%*%t(Base_forecasts)
    
  Reconciled_bottom_point.forecasts_Mint.sam <- P_MinT.sam%*%t(Base_forecasts)
  
  Reconciled_bottom_point.forecasts_Mint.wls <- P_MinT.wls%*%t(Base_forecasts)
  
  Reconciled_bottom_point.forecasts_OLS <- P_OLS%*%t(Base_forecasts)
  
  #Obtainig reconciled bottom level variance covariance matrices with different reconciliation
  #methods
  
  Sigma.tilde_bottom_BU <- P_BU%*%W.h%*%t(P_BU)
  Sigma.tilde_bottom_OLS <- P_OLS%*%diag(1,n,n)%*%t(P_OLS)
  Sigma.tilde_bottom_MinT.shr <- P_MinT.shr%*%W.h%*%t(P_MinT.shr)
  Sigma.tilde_bottom_MinT.sam <- P_MinT.sam%*%Sigma_sample%*%t(P_MinT.sam)
  Sigma.tilde_bottom_MinT.wls <- P_MinT.wls%*%Sigma_WLS%*%t(P_MinT.wls)
  
  #Obtainig reconciled point forecasts with different reconciliation
  #methods for the whole hierarchy
  Reconciled_point.forecasts_BU <- S%*%P_BU%*%t(Base_forecasts)
  
  Reconciled_point.forecasts_Mint.shr <- S%*%P_MinT.shr%*%t(Base_forecasts)
  
  Reconciled_point.forecasts_Mint.sam <- S%*%P_MinT.sam%*%t(Base_forecasts)
  
  Reconciled_point.forecasts_Mint.wls <- S%*%P_MinT.wls%*%t(Base_forecasts)
  
  Reconciled_point.forecasts_OLS <- S%*%P_OLS%*%t(Base_forecasts)
  
  #Obtainig reconciled variance covariance matrices with different reconciliation
  #methods for the whole hierarchy
  
  Sigma.tilde_BU <- S%*%P_BU%*%W.h%*%t(P_BU)%*%t(S)
  Sigma.tilde_OLS <- S%*%P_OLS%*%diag(1,n,n)%*%t(P_OLS)%*%t(S)
  Sigma.tilde_MinT.shr <- S%*%P_MinT.shr%*%W.h%*%t(P_MinT.shr)%*%t(S)
  Sigma.tilde_MinT.sam <- S%*%P_MinT.sam%*%Sigma_sample%*%t(P_MinT.sam)%*%t(S)
  Sigma.tilde_MinT.wls <- S%*%P_MinT.wls%*%Sigma_WLS%*%t(P_MinT.wls)%*%t(S)
  
  
  
  #Obtaining a random samples from the possible forecast Gaussian densities of the 
  #bottom level
  
  X_bottom_BU <- mvrnorm(n=k, mu=Reconciled_bottom_point.forecasts_BU, 
                Sigma = Sigma.tilde_bottom_BU)
  
  X_bottom_OLS <- mvrnorm(n=k, mu=Reconciled_bottom_point.forecasts_OLS, 
                 Sigma = Sigma.tilde_bottom_OLS)
  
  X_bottom_MinT.shr <- mvrnorm(n=k, mu=Reconciled_bottom_point.forecasts_Mint.shr, 
                      Sigma = Sigma.tilde_bottom_MinT.shr)
  
  X_bottom_MinT.sam <- mvrnorm(n=k, mu=Reconciled_bottom_point.forecasts_Mint.sam, 
                      Sigma = Sigma.tilde_bottom_MinT.sam)
  
  X_bottom_MinT.wls <- mvrnorm(n=k, mu=Reconciled_bottom_point.forecasts_Mint.wls, 
                                 Sigma = Sigma.tilde_bottom_MinT.wls)
  
###Evaluating the reconciled forecasts from different forecasting methods
  #(only the bottom levels are evaluated)
  
  #Evaluating the density forecasts of the bottom level using Energy score 

  #Calculating Enery score to evaluate
  
  Energy_score <- function(Data)
  {
    ES_1_eval <- numeric(0)
    ES_2_eval <- numeric(0)
    
    d1_eval <- (Data)- matrix(rep(Testing[1,(n-m+1):n],k),k,m,byrow = TRUE)
    ES_1_eval <- apply(d1_eval, 1, function(x) sqrt(sum(x^2)))
    
    d2_eval <- (Data)[1:k-1,] - (Data)[2:k,]
    ES_2_eval <- apply(d2_eval, 1, function(x) sqrt(sum(x^2)))
    ES_eval <- mean(ES_1_eval)-mean(ES_2_eval)/2
    
    return(ES_eval)
    
  }
  
   


  #Calculating Energy score for predicive densities
  
  ES_Bottum.up[j] <- Energy_score(X_bottom_BU)
  ES_OLS[j] <- Energy_score(X_bottom_OLS)
  ES_MinT.shr[j] <- Energy_score(X_bottom_MinT.shr)
  ES_MinT.sam[j] <- Energy_score(X_bottom_MinT.sam)
  ES_MinT.wls[j] <- Energy_score(X_bottom_MinT.wls)

  #Evaluating the density forecasts using Log score
  
  Log_score_eval <- function(P, Sigma.hat)
  {
    
   Bottom_mean <- P %*% t(Base_forecasts)
   Bottom_cov <- P %*% Sigma.hat %*% t(P)
   Eigen <- eigen(Bottom_cov)$values
    
   log_det <- log(prod(Eigen))
   Inv_bottom_cov <- solve(Bottom_cov)
   Mean_shift <- c(Testing[1,(n-m+1):n]-Bottom_mean)
   
   (1/2)*((m*log(2*pi)) + log_det + 
            t(Mean_shift)%*%Inv_bottom_cov%*%Mean_shift)
    
  }
  
  LS_MinT.shr[j] <- Log_score_eval(P_MinT.shr, Unreconsiled_shrinkage_cov_mat)
  
  LS_MinT.sam[j] <- Log_score_eval(P_MinT.sam, Sigma_sample)
  
  LS_MinT.wls[j] <- Log_score_eval(P_MinT.wls, Sigma_WLS)
  
  LS_OLS[j] <- Log_score_eval(P_OLS, diag(1,n,n))
  
  LS_Bottum.up[j] <- Log_score_eval(P_BU, Unreconsiled_shrinkage_cov_mat)
  

  #Evaluating the density forecasts using Variogram based score
  
  Variogram_score <- function(XX)
  {
    Y_tilde_diff <- t(apply(XX, 1, function(x)
      (abs(outer(x,x,"-")[lower.tri(outer(x,x,"-"))]))^(0.5)))
    
    Y_tilde_diff_mean <- apply(Y_tilde_diff, 2, mean)
    z <- abs(outer(Testing[1,(n-m+1):n],Testing[1,(n-m+1):n],'-'))
    y_diff <- z[lower.tri(z)]
    y_diff <- y_diff^0.5
    C <- (y_diff - Y_tilde_diff_mean)^2
    
    sum(C)
  }
  
  VS_Bottum.up[j] <- Variogram_score(X_bottom_BU)
  VS_OLS[j] <- Variogram_score(X_bottom_OLS)
  VS_MinT.shr[j] <- Variogram_score(X_bottom_MinT.shr)
  VS_MinT.sam[j] <- Variogram_score(X_bottom_MinT.sam)
  VS_MinT.wls[j] <- Variogram_score(X_bottom_MinT.wls)

###Comparing reconciled vs unreconciled forecasts using CRPS
  

  CRPS_Unrecon_Tot[j] <- crps_norm(Testing[1,1], mean = Base_forecasts[1], 
                           sd = sqrt(Unreconsiled_shrinkage_cov_mat[1,1]))
  CRPS_Unrecon_A[j] <- crps_norm(Testing[1,2], mean = Base_forecasts[2], 
                         sd = sqrt(Unreconsiled_shrinkage_cov_mat[2,2]))
  CRPS_Unrecon_B[j] <- crps_norm(Testing[1,3], mean = Base_forecasts[3], 
                         sd = sqrt(Unreconsiled_shrinkage_cov_mat[3,3]))
  CRPS_Unrecon_AA[j] <- crps_norm(Testing[1,4], mean = Base_forecasts[4], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[4,4]))
  CRPS_Unrecon_AB[j] <- crps_norm(Testing[1,5], mean = Base_forecasts[5], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[5,5]))
  CRPS_Unrecon_BA[j] <- crps_norm(Testing[1,6], mean = Base_forecasts[6], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[6,6]))
  CRPS_Unrecon_BB[j] <- crps_norm(Testing[1,7], mean = Base_forecasts[7], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[7,7]))
  
  CRPS_MinT_shr_Tot[j] <- crps_norm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.shr[1], 
                            sd = sqrt(Sigma.tilde_MinT.shr[1,1]))
  CRPS_MinT_shr_A[j] <- crps_norm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.shr[2], 
                          sd = sqrt(Sigma.tilde_MinT.shr[2,2]))
  CRPS_MinT_shr_B[j] <- crps_norm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.shr[3], 
                          sd = sqrt(Sigma.tilde_MinT.shr[3,3]))
  CRPS_MinT_shr_AA[j] <- crps_norm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.shr[4], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[4,4]))
  CRPS_MinT_shr_AB[j] <- crps_norm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.shr[5], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[5,5]))
  CRPS_MinT_shr_BA[j] <- crps_norm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.shr[6], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[6,6]))
  CRPS_MinT_shr_BB[j] <- crps_norm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.shr[7], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[7,7]))
  
  CRPS_MinT_sam_Tot[j] <- crps_norm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.sam[1], 
                            sd = sqrt(Sigma.tilde_MinT.sam[1,1]))
  CRPS_MinT_sam_A[j] <- crps_norm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.sam[2], 
                          sd = sqrt(Sigma.tilde_MinT.sam[2,2]))
  CRPS_MinT_sam_B[j] <- crps_norm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.sam[3], 
                          sd = sqrt(Sigma.tilde_MinT.sam[3,3]))
  CRPS_MinT_sam_AA[j] <- crps_norm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.sam[4], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[4,4]))
  CRPS_MinT_sam_AB[j] <- crps_norm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.sam[5], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[5,5]))
  CRPS_MinT_sam_BA[j] <- crps_norm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.sam[6], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[6,6]))
  CRPS_MinT_sam_BB[j] <- crps_norm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.sam[7], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[7,7]))
  
  CRPS_MinT_wls_Tot[j] <- crps_norm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.wls[1], 
                            sd = sqrt(Sigma.tilde_MinT.wls[1,1]))
  CRPS_MinT_wls_A[j] <- crps_norm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.wls[2], 
                          sd = sqrt(Sigma.tilde_MinT.wls[2,2]))
  CRPS_MinT_wls_B[j] <- crps_norm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.wls[3], 
                          sd = sqrt(Sigma.tilde_MinT.wls[3,3]))
  CRPS_MinT_wls_AA[j] <- crps_norm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.wls[4], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[4,4]))
  CRPS_MinT_wls_AB[j] <- crps_norm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.wls[5], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[5,5]))
  CRPS_MinT_wls_BA[j] <- crps_norm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.wls[6], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[6,6]))
  CRPS_MinT_wls_BB[j] <- crps_norm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.wls[7], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[7,7]))
  
  CRPS_OLS_Tot[j] <- crps_norm(Testing[1,1], mean = Reconciled_point.forecasts_OLS[1], 
                       sd = sqrt(Sigma.tilde_OLS[1,1]))
  CRPS_OLS_A[j] <- crps_norm(Testing[1,2], mean = Reconciled_point.forecasts_OLS[2], 
                     sd = sqrt(Sigma.tilde_OLS[2,2]))
  CRPS_OLS_B[j] <- crps_norm(Testing[1,3], mean = Reconciled_point.forecasts_OLS[3], 
                     sd = sqrt(Sigma.tilde_OLS[3,3]))
  CRPS_OLS_AA[j] <- crps_norm(Testing[1,4], mean = Reconciled_point.forecasts_OLS[4], 
                             sd = sqrt(Sigma.tilde_OLS[4,4]))
  CRPS_OLS_AB[j] <- crps_norm(Testing[1,5], mean = Reconciled_point.forecasts_OLS[5], 
                             sd = sqrt(Sigma.tilde_OLS[5,5]))
  CRPS_OLS_BA[j] <- crps_norm(Testing[1,6], mean = Reconciled_point.forecasts_OLS[6], 
                             sd = sqrt(Sigma.tilde_OLS[6,6]))
  CRPS_OLS_BB[j] <- crps_norm(Testing[1,7], mean = Reconciled_point.forecasts_OLS[7], 
                             sd = sqrt(Sigma.tilde_OLS[7,7]))
  
  CRPS_BU_Tot[j] <- crps_norm(Testing[1,1], mean = Reconciled_point.forecasts_BU[1], 
                      sd = sqrt(Sigma.tilde_BU[1,1]))
  CRPS_BU_A[j] <- crps_norm(Testing[1,2], mean = Reconciled_point.forecasts_BU[2], 
                    sd = sqrt(Sigma.tilde_BU[2,2]))
  CRPS_BU_B[j] <- crps_norm(Testing[1,3], mean = Reconciled_point.forecasts_BU[3], 
                    sd = sqrt(Sigma.tilde_BU[3,3]))
  CRPS_BU_AA[j] <- crps_norm(Testing[1,4], mean = Reconciled_point.forecasts_BU[4], 
                            sd = sqrt(Sigma.tilde_BU[4,4]))
  CRPS_BU_AB[j] <- crps_norm(Testing[1,5], mean = Reconciled_point.forecasts_BU[5], 
                            sd = sqrt(Sigma.tilde_BU[5,5]))
  CRPS_BU_BA[j] <- crps_norm(Testing[1,6], mean = Reconciled_point.forecasts_BU[6], 
                            sd = sqrt(Sigma.tilde_BU[6,6]))
  CRPS_BU_BB[j] <- crps_norm(Testing[1,7], mean = Reconciled_point.forecasts_BU[7], 
                            sd = sqrt(Sigma.tilde_BU[7,7]))
  

      
  Log_Unrecon_Tot[j] <- -dnorm(Testing[1,1], mean = Base_forecasts[1], 
                                   sd = sqrt(Unreconsiled_shrinkage_cov_mat[1,1]),log = TRUE)
  Log_Unrecon_A[j] <- -dnorm(Testing[1,2], mean = Base_forecasts[2], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[2,2]),log = TRUE)
  Log_Unrecon_B[j] <- -dnorm(Testing[1,3], mean = Base_forecasts[3], 
                                 sd = sqrt(Unreconsiled_shrinkage_cov_mat[3,3]),log = TRUE)
  Log_Unrecon_AA[j] <- -dnorm(Testing[1,4], mean = Base_forecasts[4], 
                             sd = sqrt(Unreconsiled_shrinkage_cov_mat[4,4]),log = TRUE)
  Log_Unrecon_AB[j] <- -dnorm(Testing[1,5], mean = Base_forecasts[5], 
                             sd = sqrt(Unreconsiled_shrinkage_cov_mat[5,5]),log = TRUE)
  Log_Unrecon_BA[j] <- -dnorm(Testing[1,6], mean = Base_forecasts[6], 
                             sd = sqrt(Unreconsiled_shrinkage_cov_mat[6,6]),log = TRUE)
  Log_Unrecon_BB[j] <- -dnorm(Testing[1,7], mean = Base_forecasts[7], 
                             sd = sqrt(Unreconsiled_shrinkage_cov_mat[7,7]),log = TRUE)
  
  Log_MinT_shr_Tot[j] <- -dnorm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.shr[1], 
                                    sd = sqrt(Sigma.tilde_MinT.shr[1,1]),log = TRUE)
  Log_MinT_shr_A[j] <- -dnorm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.shr[2], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[2,2]),log = TRUE)
  Log_MinT_shr_B[j] <- -dnorm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.shr[3], 
                                  sd = sqrt(Sigma.tilde_MinT.shr[3,3]),log = TRUE)
  Log_MinT_shr_AA[j] <- -dnorm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.shr[4], 
                              sd = sqrt(Sigma.tilde_MinT.shr[4,4]),log = TRUE)
  Log_MinT_shr_AB[j] <- -dnorm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.shr[5], 
                              sd = sqrt(Sigma.tilde_MinT.shr[5,5]),log = TRUE)
  Log_MinT_shr_BA[j] <- -dnorm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.shr[6], 
                              sd = sqrt(Sigma.tilde_MinT.shr[6,6]),log = TRUE)
  Log_MinT_shr_BB[j] <- -dnorm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.shr[7], 
                              sd = sqrt(Sigma.tilde_MinT.shr[7,7]),log = TRUE)
  
  Log_MinT_sam_Tot[j] <- -dnorm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.sam[1], 
                                    sd = sqrt(Sigma.tilde_MinT.sam[1,1]),log = TRUE)
  Log_MinT_sam_A[j] <- -dnorm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.sam[2], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[2,2]),log = TRUE)
  Log_MinT_sam_B[j] <- -dnorm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.sam[3], 
                                  sd = sqrt(Sigma.tilde_MinT.sam[3,3]),log = TRUE)
  Log_MinT_sam_AA[j] <- -dnorm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.sam[4], 
                              sd = sqrt(Sigma.tilde_MinT.sam[4,4]),log = TRUE)
  Log_MinT_sam_AB[j] <- -dnorm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.sam[5], 
                              sd = sqrt(Sigma.tilde_MinT.sam[5,5]),log = TRUE)
  Log_MinT_sam_BA[j] <- -dnorm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.sam[6], 
                              sd = sqrt(Sigma.tilde_MinT.sam[6,6]),log = TRUE)
  Log_MinT_sam_BB[j] <- -dnorm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.sam[7], 
                              sd = sqrt(Sigma.tilde_MinT.sam[7,7]),log = TRUE)
  
  Log_MinT_wls_Tot[j] <- -dnorm(Testing[1,1], mean = Reconciled_point.forecasts_Mint.wls[1], 
                                    sd = sqrt(Sigma.tilde_MinT.wls[1,1]),log = TRUE)
  Log_MinT_wls_A[j] <- -dnorm(Testing[1,2], mean = Reconciled_point.forecasts_Mint.wls[2], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[2,2]),log = TRUE)
  Log_MinT_wls_B[j] <- -dnorm(Testing[1,3], mean = Reconciled_point.forecasts_Mint.wls[3], 
                                  sd = sqrt(Sigma.tilde_MinT.wls[3,3]),log = TRUE)
  Log_MinT_wls_AA[j] <- -dnorm(Testing[1,4], mean = Reconciled_point.forecasts_Mint.wls[4], 
                              sd = sqrt(Sigma.tilde_MinT.wls[4,4]),log = TRUE)
  Log_MinT_wls_AB[j] <- -dnorm(Testing[1,5], mean = Reconciled_point.forecasts_Mint.wls[5], 
                              sd = sqrt(Sigma.tilde_MinT.wls[5,5]),log = TRUE)
  Log_MinT_wls_BA[j] <- -dnorm(Testing[1,6], mean = Reconciled_point.forecasts_Mint.wls[6], 
                              sd = sqrt(Sigma.tilde_MinT.wls[6,6]),log = TRUE)
  Log_MinT_wls_BB[j] <- -dnorm(Testing[1,7], mean = Reconciled_point.forecasts_Mint.wls[7], 
                              sd = sqrt(Sigma.tilde_MinT.wls[7,7]),log = TRUE)
  
  Log_OLS_Tot[j] <- -dnorm(Testing[1,1], mean = Reconciled_point.forecasts_OLS[1], 
                               sd = sqrt(Sigma.tilde_OLS[1,1]),log = TRUE)
  Log_OLS_A[j] <- -dnorm(Testing[1,2], mean = Reconciled_point.forecasts_OLS[2], 
                             sd = sqrt(Sigma.tilde_OLS[2,2]),log = TRUE)
  Log_OLS_B[j] <- -dnorm(Testing[1,3], mean = Reconciled_point.forecasts_OLS[3], 
                             sd = sqrt(Sigma.tilde_OLS[3,3]),log = TRUE)
  Log_OLS_AA[j] <- -dnorm(Testing[1,4], mean = Reconciled_point.forecasts_OLS[4], 
                         sd = sqrt(Sigma.tilde_OLS[4,4]),log = TRUE)
  Log_OLS_AB[j] <- -dnorm(Testing[1,5], mean = Reconciled_point.forecasts_OLS[5], 
                         sd = sqrt(Sigma.tilde_OLS[5,5]),log = TRUE)
  Log_OLS_BA[j] <- -dnorm(Testing[1,6], mean = Reconciled_point.forecasts_OLS[6], 
                         sd = sqrt(Sigma.tilde_OLS[6,6]),log = TRUE)
  Log_OLS_BB[j] <- -dnorm(Testing[1,7], mean = Reconciled_point.forecasts_OLS[7], 
                         sd = sqrt(Sigma.tilde_OLS[7,7]),log = TRUE)
  
  Log_BU_Tot[j] <- -dnorm(Testing[1,1], mean = Reconciled_point.forecasts_BU[1], 
                              sd = sqrt(Sigma.tilde_BU[1,1]),log = TRUE)
  Log_BU_A[j] <- -dnorm(Testing[1,2], mean = Reconciled_point.forecasts_BU[2], 
                            sd = sqrt(Sigma.tilde_BU[2,2]),log = TRUE)
  Log_BU_B[j] <- -dnorm(Testing[1,3], mean = Reconciled_point.forecasts_BU[3], 
                            sd = sqrt(Sigma.tilde_BU[3,3]),log = TRUE)
  Log_BU_AA[j] <- -dnorm(Testing[1,4], mean = Reconciled_point.forecasts_BU[4], 
                        sd = sqrt(Sigma.tilde_BU[4,4]),log = TRUE)
  Log_BU_AB[j] <- -dnorm(Testing[1,5], mean = Reconciled_point.forecasts_BU[5], 
                        sd = sqrt(Sigma.tilde_BU[5,5]),log = TRUE)
  Log_BU_BA[j] <- -dnorm(Testing[1,6], mean = Reconciled_point.forecasts_BU[6], 
                        sd = sqrt(Sigma.tilde_BU[6,6]),log = TRUE)
  Log_BU_BB[j] <- -dnorm(Testing[1,7], mean = Reconciled_point.forecasts_BU[7], 
                        sd = sqrt(Sigma.tilde_BU[7,7]),log = TRUE)
  
  
}



Mean_ES_Bottum.up <- round(mean(ES_Bottum.up), digits = 2)
Mean_ES_OLS <- round(mean(ES_OLS), digits = 2)
Mean_ES_MinT.shr <- round(mean(ES_MinT.shr), digits = 2)
Mean_ES_MinT.sam <- round(mean(ES_MinT.sam), digits = 2)
Mean_ES_MinT.wls <- round(mean(ES_MinT.wls), digits = 2)

Mean_LS_Bottum.up <- round(mean(LS_Bottum.up), digits = 2)
Mean_LS_OLS <- round(mean(LS_OLS), digits = 2)
Mean_LS_MinT.shr <- round(mean(LS_MinT.shr), digits = 2)
Mean_LS_MinT.sam <- round(mean(LS_MinT.sam), digits = 2)
Mean_LS_MinT.wls <- round(mean(LS_MinT.wls), digits = 2)

Mean_VS_Bottum.up <- round(mean(VS_Bottum.up), digits = 2)
Mean_VS_OLS <- round(mean(VS_OLS), digits = 2)
Mean_VS_MinT.shr <- round(mean(VS_MinT.shr), digits = 2)
Mean_VS_MinT.sam <- round(mean(VS_MinT.sam), digits = 2)
Mean_VS_MinT.wls <- round(mean(VS_MinT.wls), digits = 2)

ES_Skill.score_MinT.shr <- 100*(Mean_ES_Bottum.up - Mean_ES_MinT.shr)/Mean_ES_Bottum.up
ES_Skill.score_MinT.sam <- 100*(Mean_ES_Bottum.up - Mean_ES_MinT.sam)/Mean_ES_Bottum.up
ES_Skill.score_MinT.wls <- 100*(Mean_ES_Bottum.up - Mean_ES_MinT.wls)/Mean_ES_Bottum.up
ES_Skill.score_OLS <- 100*(Mean_ES_Bottum.up - Mean_ES_OLS)/Mean_ES_Bottum.up

VS_Skill.score_MinT.shr <- 100*(Mean_VS_Bottum.up - Mean_VS_MinT.shr)/Mean_VS_Bottum.up
VS_Skill.score_MinT.sam <- 100*(Mean_VS_Bottum.up - Mean_VS_MinT.sam)/Mean_VS_Bottum.up
VS_Skill.score_MinT.wls <- 100*(Mean_VS_Bottum.up - Mean_VS_MinT.wls)/Mean_VS_Bottum.up
VS_Skill.score_OLS <- 100*(Mean_VS_Bottum.up - Mean_VS_OLS)/Mean_VS_Bottum.up

LS_Skill.score_MinT.shr <- 100*(Mean_LS_Bottum.up - Mean_LS_MinT.shr)/Mean_LS_Bottum.up
LS_Skill.score_MinT.sam <- 100*(Mean_LS_Bottum.up - Mean_LS_MinT.sam)/Mean_LS_Bottum.up
LS_Skill.score_MinT.wls <- 100*(Mean_LS_Bottum.up - Mean_LS_MinT.wls)/Mean_LS_Bottum.up
LS_Skill.score_OLS <- 100*(Mean_LS_Bottum.up - Mean_LS_OLS)/Mean_LS_Bottum.up


CRPS_Unrecon <- cbind(CRPS_Unrecon_Tot, CRPS_Unrecon_A, CRPS_Unrecon_B, CRPS_Unrecon_AA, 
                      CRPS_Unrecon_AB, CRPS_Unrecon_BA, CRPS_Unrecon_BB)
CRPS_MinT_shr <- cbind(CRPS_MinT_shr_Tot, CRPS_MinT_shr_A, CRPS_MinT_shr_B, CRPS_MinT_shr_AA, 
                       CRPS_MinT_shr_AB, CRPS_MinT_shr_BA, CRPS_MinT_shr_BB)
CRPS_MinT_sam <- cbind(CRPS_MinT_sam_Tot, CRPS_MinT_sam_A, CRPS_MinT_sam_B, CRPS_MinT_sam_AA, 
                       CRPS_MinT_sam_AB, CRPS_MinT_sam_BA, CRPS_MinT_sam_BB)
CRPS_MinT_wls <- cbind(CRPS_MinT_wls_Tot, CRPS_MinT_wls_A, CRPS_MinT_wls_B, CRPS_MinT_wls_AA, 
                       CRPS_MinT_wls_AB, CRPS_MinT_wls_BA, CRPS_MinT_wls_BB)
CRPS_OLS <- cbind(CRPS_OLS_Tot, CRPS_OLS_A, CRPS_OLS_B, CRPS_OLS_AA, CRPS_OLS_AB, 
                  CRPS_OLS_BA, CRPS_OLS_BB)
CRPS_BU <- cbind(CRPS_BU_Tot, CRPS_BU_A, CRPS_BU_B, CRPS_BU_AA, CRPS_BU_AB, 
                 CRPS_BU_BA, CRPS_BU_BB)

Mean_CRPS_Unrecon <- round(apply(CRPS_Unrecon, 2, mean), digits = 2)
Mean_CRPS_MinT_shr <- round(apply(CRPS_MinT_shr, 2, mean), digits = 2)
Mean_CRPS_MinT_sam <- round(apply(CRPS_MinT_sam, 2, mean), digits = 2)
Mean_CRPS_MinT_wls <- round(apply(CRPS_MinT_wls, 2, mean), digits = 2)
Mean_CRPS_OLS <- round(apply(CRPS_OLS, 2, mean), digits = 2)
Mean_CRPS_BU <- round(apply(CRPS_BU, 2, mean), digits = 2)

CRPS_Skill_score_MinT.shr_Tot <- 100*(Mean_CRPS_Unrecon[1] - Mean_CRPS_MinT_shr[1])/Mean_CRPS_Unrecon[1]
CRPS_Skill_score_MinT.sam_Tot <- 100*(Mean_CRPS_Unrecon[1] - Mean_CRPS_MinT_sam[1])/Mean_CRPS_Unrecon[1]
CRPS_Skill_score_MinT.wls_Tot <- 100*(Mean_CRPS_Unrecon[1] - Mean_CRPS_MinT_wls[1])/Mean_CRPS_Unrecon[1]
CRPS_Skill_score_OLS_Tot <- 100*(Mean_CRPS_Unrecon[1] - Mean_CRPS_OLS[1])/Mean_CRPS_Unrecon[1]
CRPS_Skill_score_BU_Tot <- 100*(Mean_CRPS_Unrecon[1] - Mean_CRPS_BU[1])/Mean_CRPS_Unrecon[1]

CRPS_Skill_score_MinT.shr_A <- 100*(Mean_CRPS_Unrecon[2] - Mean_CRPS_MinT_shr[2])/Mean_CRPS_Unrecon[2]
CRPS_Skill_score_MinT.sam_A <- 100*(Mean_CRPS_Unrecon[2] - Mean_CRPS_MinT_sam[2])/Mean_CRPS_Unrecon[2]
CRPS_Skill_score_MinT.wls_A <- 100*(Mean_CRPS_Unrecon[2] - Mean_CRPS_MinT_wls[2])/Mean_CRPS_Unrecon[2]
CRPS_Skill_score_OLS_A <- 100*(Mean_CRPS_Unrecon[2] - Mean_CRPS_OLS[2])/Mean_CRPS_Unrecon[2]
CRPS_Skill_score_BU_A <- 100*(Mean_CRPS_Unrecon[2] - Mean_CRPS_BU[2])/Mean_CRPS_Unrecon[2]

CRPS_Skill_score_MinT.shr_B <- 100*(Mean_CRPS_Unrecon[3] - Mean_CRPS_MinT_shr[3])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_MinT.sam_B <- 100*(Mean_CRPS_Unrecon[3] - Mean_CRPS_MinT_sam[3])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_MinT.wls_B <- 100*(Mean_CRPS_Unrecon[3] - Mean_CRPS_MinT_wls[3])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_OLS_B <- 100*(Mean_CRPS_Unrecon[3] - Mean_CRPS_OLS[3])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_BU_B <- 100*(Mean_CRPS_Unrecon[3] - Mean_CRPS_BU[3])/Mean_CRPS_Unrecon[3]

CRPS_Skill_score_MinT.shr_AA <- 100*(Mean_CRPS_Unrecon[4] - Mean_CRPS_MinT_shr[4])/Mean_CRPS_Unrecon[4]
CRPS_Skill_score_MinT.sam_AA <- 100*(Mean_CRPS_Unrecon[4] - Mean_CRPS_MinT_sam[4])/Mean_CRPS_Unrecon[4]
CRPS_Skill_score_MinT.wls_AA <- 100*(Mean_CRPS_Unrecon[4] - Mean_CRPS_MinT_wls[4])/Mean_CRPS_Unrecon[4]
CRPS_Skill_score_OLS_AA <- 100*(Mean_CRPS_Unrecon[4] - Mean_CRPS_OLS[4])/Mean_CRPS_Unrecon[4]
CRPS_Skill_score_BU_AA <- 100*(Mean_CRPS_Unrecon[4] - Mean_CRPS_BU[4])/Mean_CRPS_Unrecon[4]

CRPS_Skill_score_MinT.shr_AB <- 100*(Mean_CRPS_Unrecon[5] - Mean_CRPS_MinT_shr[5])/Mean_CRPS_Unrecon[5]
CRPS_Skill_score_MinT.sam_AB <- 100*(Mean_CRPS_Unrecon[5] - Mean_CRPS_MinT_sam[5])/Mean_CRPS_Unrecon[5]
CRPS_Skill_score_MinT.wls_AB <- 100*(Mean_CRPS_Unrecon[5] - Mean_CRPS_MinT_wls[5])/Mean_CRPS_Unrecon[5]
CRPS_Skill_score_OLS_AB <- 100*(Mean_CRPS_Unrecon[5] - Mean_CRPS_OLS[5])/Mean_CRPS_Unrecon[5]
CRPS_Skill_score_BU_AB <- 100*(Mean_CRPS_Unrecon[5] - Mean_CRPS_BU[5])/Mean_CRPS_Unrecon[5]

CRPS_Skill_score_MinT.shr_BA <- 100*(Mean_CRPS_Unrecon[6] - Mean_CRPS_MinT_shr[6])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_MinT.sam_BA <- 100*(Mean_CRPS_Unrecon[6] - Mean_CRPS_MinT_sam[6])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_MinT.wls_BA <- 100*(Mean_CRPS_Unrecon[6] - Mean_CRPS_MinT_wls[6])/Mean_CRPS_Unrecon[3]
CRPS_Skill_score_OLS_BA <- 100*(Mean_CRPS_Unrecon[6] - Mean_CRPS_OLS[6])/Mean_CRPS_Unrecon[6]
CRPS_Skill_score_BU_BA <- 100*(Mean_CRPS_Unrecon[6] - Mean_CRPS_BU[6])/Mean_CRPS_Unrecon[6]

CRPS_Skill_score_MinT.shr_BB <- 100*(Mean_CRPS_Unrecon[7] - Mean_CRPS_MinT_shr[7])/Mean_CRPS_Unrecon[7]
CRPS_Skill_score_MinT.sam_BB <- 100*(Mean_CRPS_Unrecon[7] - Mean_CRPS_MinT_sam[7])/Mean_CRPS_Unrecon[7]
CRPS_Skill_score_MinT.wls_BB <- 100*(Mean_CRPS_Unrecon[7] - Mean_CRPS_MinT_wls[7])/Mean_CRPS_Unrecon[7]
CRPS_Skill_score_OLS_BB <- 100*(Mean_CRPS_Unrecon[7] - Mean_CRPS_OLS[7])/Mean_CRPS_Unrecon[7]
CRPS_Skill_score_BU_BB <- 100*(Mean_CRPS_Unrecon[7] - Mean_CRPS_BU[7])/Mean_CRPS_Unrecon[7]

Log_Unrecon <- cbind(Log_Unrecon_Tot, Log_Unrecon_A, Log_Unrecon_B, Log_Unrecon_AA, 
                     Log_Unrecon_AB, Log_Unrecon_BA, Log_Unrecon_BB)
Log_MinT_shr <- cbind(Log_MinT_shr_Tot, Log_MinT_shr_A, Log_MinT_shr_B, Log_MinT_shr_AA, 
                      Log_MinT_shr_AB, Log_MinT_shr_BA, Log_MinT_shr_BB)
Log_MinT_sam <- cbind(Log_MinT_sam_Tot, Log_MinT_sam_A, Log_MinT_sam_B, Log_MinT_sam_AA, 
                      Log_MinT_sam_AB, Log_MinT_sam_BA, Log_MinT_sam_BB)
Log_MinT_wls <- cbind(Log_MinT_wls_Tot, Log_MinT_wls_A, Log_MinT_wls_B, Log_MinT_wls_AA, 
                      Log_MinT_wls_AB, Log_MinT_wls_BA, Log_MinT_wls_BB)
Log_OLS <- cbind(Log_OLS_Tot, Log_OLS_A, Log_OLS_B, Log_OLS_AA, Log_OLS_AB, 
                 Log_OLS_BA, Log_OLS_BB)
Log_BU <- cbind(Log_BU_Tot, Log_BU_A, Log_BU_B, Log_BU_AA, Log_BU_AB, Log_BU_BA, Log_BU_BB)

Mean_Log_Unrecon <- round(apply(Log_Unrecon, 2, mean), digits = 2)
Mean_Log_MinT_shr <- round(apply(Log_MinT_shr, 2, mean), digits = 2)
Mean_Log_MinT_sam <- round(apply(Log_MinT_sam, 2, mean), digits = 2)
Mean_Log_MinT_wls <- round(apply(Log_MinT_wls, 2, mean), digits = 2)
Mean_Log_OLS <- round(apply(Log_OLS, 2, mean), digits = 2)
Mean_Log_BU <- round(apply(Log_BU, 2, mean), digits = 2)

Log_Skill_score_MinT.shr_Tot <- 100*(Mean_Log_Unrecon[1] - Mean_Log_MinT_shr[1])/Mean_Log_Unrecon[1]
Log_Skill_score_MinT.sam_Tot <- 100*(Mean_Log_Unrecon[1] - Mean_Log_MinT_sam[1])/Mean_Log_Unrecon[1]
Log_Skill_score_MinT.wls_Tot <- 100*(Mean_Log_Unrecon[1] - Mean_Log_MinT_wls[1])/Mean_Log_Unrecon[1]
Log_Skill_score_OLS_Tot <- 100*(Mean_Log_Unrecon[1] - Mean_Log_OLS[1])/Mean_Log_Unrecon[1]
Log_Skill_score_BU_Tot <- 100*(Mean_Log_Unrecon[1] - Mean_Log_BU[1])/Mean_Log_Unrecon[1]

Log_Skill_score_MinT.shr_A <- 100*(Mean_Log_Unrecon[2] - Mean_Log_MinT_shr[2])/Mean_Log_Unrecon[2]
Log_Skill_score_MinT.sam_A <- 100*(Mean_Log_Unrecon[2] - Mean_Log_MinT_sam[2])/Mean_Log_Unrecon[2]
Log_Skill_score_MinT.wls_A <- 100*(Mean_Log_Unrecon[2] - Mean_Log_MinT_wls[2])/Mean_Log_Unrecon[2]
Log_Skill_score_OLS_A <- 100*(Mean_Log_Unrecon[2] - Mean_Log_OLS[2])/Mean_Log_Unrecon[2]
Log_Skill_score_BU_A <- 100*(Mean_Log_Unrecon[2] - Mean_Log_BU[2])/Mean_Log_Unrecon[2]

Log_Skill_score_MinT.shr_B <- 100*(Mean_Log_Unrecon[3] - Mean_Log_MinT_shr[3])/Mean_Log_Unrecon[3]
Log_Skill_score_MinT.sam_B <- 100*(Mean_Log_Unrecon[3] - Mean_Log_MinT_sam[3])/Mean_Log_Unrecon[3]
Log_Skill_score_MinT.wls_B <- 100*(Mean_Log_Unrecon[3] - Mean_Log_MinT_wls[3])/Mean_Log_Unrecon[3]
Log_Skill_score_OLS_B <- 100*(Mean_Log_Unrecon[3] - Mean_Log_OLS[3])/Mean_Log_Unrecon[3]
Log_Skill_score_BU_B <- 100*(Mean_Log_Unrecon[3] - Mean_Log_BU[3])/Mean_Log_Unrecon[3]

Log_Skill_score_MinT.shr_AA <- 100*(Mean_Log_Unrecon[4] - Mean_Log_MinT_shr[4])/Mean_Log_Unrecon[4]
Log_Skill_score_MinT.sam_AA <- 100*(Mean_Log_Unrecon[4] - Mean_Log_MinT_sam[4])/Mean_Log_Unrecon[4]
Log_Skill_score_MinT.wls_AA <- 100*(Mean_Log_Unrecon[4] - Mean_Log_MinT_wls[4])/Mean_Log_Unrecon[4]
Log_Skill_score_OLS_AA <- 100*(Mean_Log_Unrecon[4] - Mean_Log_OLS[4])/Mean_Log_Unrecon[4]
Log_Skill_score_BU_AA <- 100*(Mean_Log_Unrecon[4] - Mean_Log_BU[4])/Mean_Log_Unrecon[4]

Log_Skill_score_MinT.shr_AB <- 100*(Mean_Log_Unrecon[5] - Mean_Log_MinT_shr[5])/Mean_Log_Unrecon[5]
Log_Skill_score_MinT.sam_AB <- 100*(Mean_Log_Unrecon[5] - Mean_Log_MinT_sam[5])/Mean_Log_Unrecon[5]
Log_Skill_score_MinT.wls_AB <- 100*(Mean_Log_Unrecon[5] - Mean_Log_MinT_wls[5])/Mean_Log_Unrecon[5]
Log_Skill_score_OLS_AB <- 100*(Mean_Log_Unrecon[5] - Mean_Log_OLS[5])/Mean_Log_Unrecon[5]
Log_Skill_score_BU_AB <- 100*(Mean_Log_Unrecon[5] - Mean_Log_BU[5])/Mean_Log_Unrecon[5]

Log_Skill_score_MinT.shr_BA <- 100*(Mean_Log_Unrecon[6] - Mean_Log_MinT_shr[6])/Mean_Log_Unrecon[6]
Log_Skill_score_MinT.sam_BA <- 100*(Mean_Log_Unrecon[6] - Mean_Log_MinT_sam[6])/Mean_Log_Unrecon[6]
Log_Skill_score_MinT.wls_BA <- 100*(Mean_Log_Unrecon[6] - Mean_Log_MinT_wls[6])/Mean_Log_Unrecon[6]
Log_Skill_score_OLS_BA <- 100*(Mean_Log_Unrecon[6] - Mean_Log_OLS[6])/Mean_Log_Unrecon[6]
Log_Skill_score_BU_BA <- 100*(Mean_Log_Unrecon[6] - Mean_Log_BU[6])/Mean_Log_Unrecon[6]

Log_Skill_score_MinT.shr_BB <- 100*(Mean_Log_Unrecon[7] - Mean_Log_MinT_shr[7])/Mean_Log_Unrecon[7]
Log_Skill_score_MinT.sam_BB <- 100*(Mean_Log_Unrecon[7] - Mean_Log_MinT_sam[7])/Mean_Log_Unrecon[7]
Log_Skill_score_MinT.wls_BB <- 100*(Mean_Log_Unrecon[7] - Mean_Log_MinT_wls[7])/Mean_Log_Unrecon[7]
Log_Skill_score_OLS_BB <- 100*(Mean_Log_Unrecon[7] - Mean_Log_OLS[7])/Mean_Log_Unrecon[7]
Log_Skill_score_BU_BB <- 100*(Mean_Log_Unrecon[7] - Mean_Log_BU[7])/Mean_Log_Unrecon[7]


Method < -c("MinT.shr", "MinT.sam", "MinT.wls", "OLS","Bottom.up")

Mean_Energy_score <- c(Mean_ES_MinT.shr, Mean_ES_MinT.sam, Mean_ES_MinT.wls, 
                     Mean_ES_OLS, Mean_ES_Bottum.up)

Mean_Log_score <- c(Mean_LS_MinT.shr, Mean_LS_MinT.sam, Mean_LS_MinT.wls, 
                  Mean_LS_OLS, Mean_LS_Bottum.up)

Mean_Variogram_score <- c(Mean_VS_MinT.shr, Mean_VS_MinT.sam, Mean_VS_MinT.wls, 
                        Mean_VS_OLS, Mean_VS_Bottum.up)

Eval_Prob_forecasts <- data.frame(Method, Mean_Energy_score, Mean_Log_score,Mean_Variogram_score)

METHOD <- c("MinT.shr", "MinT.sam", "MinT.wls", "OLS")

ES_Skill_score <- round(c(ES_Skill.score_MinT.shr, ES_Skill.score_MinT.sam, ES_Skill.score_MinT.wls, 
                    ES_Skill.score_OLS), digits = 2)
LS_Skill_score <- round(c(LS_Skill.score_MinT.shr, LS_Skill.score_MinT.sam, LS_Skill.score_MinT.wls, 
                          LS_Skill.score_OLS), digits = 2)
VS_Skill_score <- round(c(VS_Skill.score_MinT.shr, VS_Skill.score_MinT.sam, VS_Skill.score_MinT.wls, 
                          VS_Skill.score_OLS), digits = 2)

Eval_Prob_forecasts_Skill.score <- data.frame(METHOD, ES_Skill_score, LS_Skill_score, VS_Skill_score)

#Comparison of reconciled vs unreconciled

Forecasting_Method <- c("Unreconciled","MinT.shr", "MinT.sam", "MinT.wls", "OLS","Bottom.up")
Tot_CRPS <- c(Mean_CRPS_Unrecon[1], Mean_CRPS_MinT_shr[1], Mean_CRPS_MinT_sam[1], Mean_CRPS_MinT_wls[1],
         Mean_CRPS_OLS[1], Mean_CRPS_BU[1])

A_CRPS <- c(Mean_CRPS_Unrecon[2], Mean_CRPS_MinT_shr[2], Mean_CRPS_MinT_sam[2], Mean_CRPS_MinT_wls[2],
         Mean_CRPS_OLS[2], Mean_CRPS_BU[2])
B_CRPS <- c(Mean_CRPS_Unrecon[3], Mean_CRPS_MinT_shr[3], Mean_CRPS_MinT_sam[3], Mean_CRPS_MinT_wls[3],
         Mean_CRPS_OLS[3], Mean_CRPS_BU[3])
AA_CRPS <- c(Mean_CRPS_Unrecon[4], Mean_CRPS_MinT_shr[4], Mean_CRPS_MinT_sam[4], Mean_CRPS_MinT_wls[4],
            Mean_CRPS_OLS[4], Mean_CRPS_BU[4])
AB_CRPS <- c(Mean_CRPS_Unrecon[5], Mean_CRPS_MinT_shr[5], Mean_CRPS_MinT_sam[5], Mean_CRPS_MinT_wls[5],
            Mean_CRPS_OLS[5], Mean_CRPS_BU[5])
BA_CRPS <- c(Mean_CRPS_Unrecon[6], Mean_CRPS_MinT_shr[6], Mean_CRPS_MinT_sam[6], Mean_CRPS_MinT_wls[6],
            Mean_CRPS_OLS[6], Mean_CRPS_BU[6])
BB_CRPS <- c(Mean_CRPS_Unrecon[7], Mean_CRPS_MinT_shr[7], Mean_CRPS_MinT_sam[7], Mean_CRPS_MinT_wls[7],
            Mean_CRPS_OLS[7], Mean_CRPS_BU[7])

Tot_Log <- c(Mean_Log_Unrecon[1], Mean_Log_MinT_shr[1], Mean_Log_MinT_sam[1], Mean_Log_MinT_wls[1],
              Mean_Log_OLS[1], Mean_Log_BU[1])

A_Log <- c(Mean_Log_Unrecon[2], Mean_Log_MinT_shr[2], Mean_Log_MinT_sam[2], Mean_Log_MinT_wls[2],
            Mean_Log_OLS[2], Mean_Log_BU[2])
B_Log <- c(Mean_Log_Unrecon[3], Mean_Log_MinT_shr[3], Mean_Log_MinT_sam[3], Mean_Log_MinT_wls[3],
            Mean_Log_OLS[3], Mean_Log_BU[3])

AA_Log <- c(Mean_Log_Unrecon[4], Mean_Log_MinT_shr[4], Mean_Log_MinT_sam[4], Mean_Log_MinT_wls[4],
           Mean_Log_OLS[4], Mean_Log_BU[4])
AB_Log <- c(Mean_Log_Unrecon[5], Mean_Log_MinT_shr[5], Mean_Log_MinT_sam[5], Mean_Log_MinT_wls[5],
           Mean_Log_OLS[5], Mean_Log_BU[5])
BA_Log <- c(Mean_Log_Unrecon[6], Mean_Log_MinT_shr[6], Mean_Log_MinT_sam[6], Mean_Log_MinT_wls[6],
           Mean_Log_OLS[6], Mean_Log_BU[6])
BB_Log <- c(Mean_Log_Unrecon[7], Mean_Log_MinT_shr[7], Mean_Log_MinT_sam[7], Mean_Log_MinT_wls[7],
           Mean_Log_OLS[7], Mean_Log_BU[7])

#Comparison of reconciled vs unreconciled - Skill score

Tot_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_Tot, CRPS_Skill_score_MinT.sam_Tot, 
         CRPS_Skill_score_MinT.wls_Tot, CRPS_Skill_score_OLS_Tot, CRPS_Skill_score_BU_Tot), digits = 2)

A_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_A, CRPS_Skill_score_MinT.sam_A, 
         CRPS_Skill_score_MinT.wls_A, CRPS_Skill_score_OLS_A, CRPS_Skill_score_BU_A), digits = 2)

B_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_B, CRPS_Skill_score_MinT.sam_B, 
       CRPS_Skill_score_MinT.wls_B, CRPS_Skill_score_OLS_B, CRPS_Skill_score_BU_B), digits = 2)

AA_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_AA, CRPS_Skill_score_MinT.sam_AA, 
                              CRPS_Skill_score_MinT.wls_AA, CRPS_Skill_score_OLS_AA, CRPS_Skill_score_BU_AA), digits = 2)

AB_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_AB, CRPS_Skill_score_MinT.sam_AB, 
                              CRPS_Skill_score_MinT.wls_AB, CRPS_Skill_score_OLS_AB, CRPS_Skill_score_BU_AB), digits = 2)

BA_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_BA, CRPS_Skill_score_MinT.sam_BA, 
                              CRPS_Skill_score_MinT.wls_BA, CRPS_Skill_score_OLS_BA, CRPS_Skill_score_BU_BA), digits = 2)

BB_Skill.score_CRPS <- round(c(CRPS_Skill_score_MinT.shr_BB, CRPS_Skill_score_MinT.sam_BB, 
                              CRPS_Skill_score_MinT.wls_BB, CRPS_Skill_score_OLS_BB, CRPS_Skill_score_BU_BB), digits = 2)

Tot_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_Tot, Log_Skill_score_MinT.sam_Tot, 
                         Log_Skill_score_MinT.wls_Tot, Log_Skill_score_OLS_Tot, Log_Skill_score_BU_Tot), digits = 2)

A_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_A, Log_Skill_score_MinT.sam_A, 
                       Log_Skill_score_MinT.wls_A, Log_Skill_score_OLS_A, Log_Skill_score_BU_A), digits = 2)

B_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_B, Log_Skill_score_MinT.sam_B, 
                       Log_Skill_score_MinT.wls_B, Log_Skill_score_OLS_B, Log_Skill_score_BU_B), digits = 2)

AA_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_AA, Log_Skill_score_MinT.sam_AA, 
                             Log_Skill_score_MinT.wls_AA, Log_Skill_score_OLS_AA, Log_Skill_score_BU_AA), digits = 2)

AB_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_AB, Log_Skill_score_MinT.sam_AB, 
                             Log_Skill_score_MinT.wls_AB, Log_Skill_score_OLS_AB, Log_Skill_score_BU_AB), digits = 2)

BA_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_BA, Log_Skill_score_MinT.sam_BA, 
                             Log_Skill_score_MinT.wls_BA, Log_Skill_score_OLS_BA, Log_Skill_score_BU_BA), digits = 2)

BB_Skill.score_Log <- round(c(Log_Skill_score_MinT.shr_BB, Log_Skill_score_MinT.sam_BB, 
                             Log_Skill_score_MinT.wls_BB, Log_Skill_score_OLS_BB, Log_Skill_score_BU_BB), digits = 2)

Eval_unrecon.vs.recon_CRPS <- data.frame(Forecasting_Method, Tot_CRPS, A_CRPS, B_CRPS, 
                                         AA_CRPS, AB_CRPS, BA_CRPS, BB_CRPS)
Eval_skill.score_CRPS <- data.frame(Method, Tot_Skill.score_CRPS, A_Skill.score_CRPS, 
                                    B_Skill.score_CRPS, AA_Skill.score_CRPS, 
                                    AB_Skill.score_CRPS, BA_Skill.score_CRPS, 
                                    BB_Skill.score_CRPS)

Eval_unrecon.vs.recon_Log <- data.frame(Forecasting_Method, Tot_Log, A_Log, B_Log, AA_Log, 
                                        AB_Log, BA_Log, BB_Log)
Eval_skill.score_Log <- data.frame(Method, Tot_Skill.score_Log, A_Skill.score_Log, 
                                    B_Skill.score_Log, AA_Skill.score_Log, 
                                   AB_Skill.score_Log, BA_Skill.score_Log, 
                                   BB_Skill.score_Log)


Compare_reconciled.forecasts <- kable(Eval_Prob_forecasts, format = "markdown", align = "c")
Compare_reconciled.forecasts_Skill.score <- kable(Eval_Prob_forecasts_Skill.score, format = "markdown", align = "c")

Compare_unrecon.vs.recon.forecasts_CRPS <- kable(Eval_unrecon.vs.recon_CRPS, format = "markdown", align = "c")
Comparison_Skill.score_CRPS <- kable(Eval_skill.score_CRPS, format = "markdown", align = "c")

Compare_unrecon.vs.recon.forecasts_Log <- kable(Eval_unrecon.vs.recon_Log, format = "markdown", align = "c")
Comparison_Skill.score_Log <- kable(Eval_skill.score_Log, format = "markdown", align = "c")

end <- Sys.time()

end-start

