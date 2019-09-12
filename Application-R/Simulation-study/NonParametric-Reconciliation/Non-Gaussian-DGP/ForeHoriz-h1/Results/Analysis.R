#Method 1: Estimating W
#Method 2: Estimating through the reparameterisation using cholesky decomposition
#Method 3: Estimating P directly, imposing the constraint PS=I


library(tidyverse)



ES_1_250 <- read.csv("ES_1-250.csv")[,-1]
ES_251_500 <- read.csv("ES_251-500.csv")[,-1]
ES_501_750 <- read.csv("ES_501-750.csv")[,-1]
ES_751_1000 <- read.csv("ES_751-1000.csv")[,-1]


ES <- ES_1_250 + ES_251_500 + ES_501_750 + ES_751_1000

VS_1_250 <- read.csv("VS_1-250.csv")[,-1]
VS_251_500 <- read.csv("VS_251-500.csv")[,-1]
VS_501_750 <- read.csv("VS_501-750.csv")[,-1]
VS_751_1000 <- read.csv("VS_751-1000.csv")[,-1]


VS <- VS_1_250 + VS_251_500 + VS_501_750 + VS_751_1000


colnames(ES) <- c("Method1_Opt.W", "Method3_Opt.P", "MinT.shr", "MinT.wls", "OLS", "Unreconciled")
colnames(VS) <- c("Method1_Opt.W", "Method3_Opt.P", "MinT.shr", "MinT.wls", "OLS", "Unreconciled")

ES_mean <- t(apply(ES, 2, mean))
ES_mean <- t(ES_mean)

VS_mean <- t(apply(VS, 2, mean))
VS_mean <- t(VS_mean)

Comparison <- round(cbind(ES_mean[,1], VS_mean[,1]), digits = 4)
colnames(Comparison) <- c("ES", "VS")
Comparison

ES %>% gather(key = "Method", value = "ES") -> ES_2
ggplot(ES_2, aes(x=Method, y=ES)) + geom_boxplot() + 
  scale_x_discrete(labels = c("Method1_Opt.W"="M1_Opt.W", "Method2_Opt.chol"="M2_chol", 
                              "Method2b_Opt.chol.Restricted"="M2b_chol", "Method3_Opt.P"="M3_Opt.P",
                              "MinT.shr"="MinT", "OLS"="OLS", "Unreconciled"="Unrecon"))

ggplot(ES) + geom_point(aes(x=MinT.shr, y=Method3_Opt.P))

Sigma_Optimal.vs.MinT.shr <- mean((ES[,"Method3_Opt.P"] - ES[,"MinT.shr"])^2)
t_Optimal.vs.MinT.shr <- sqrt(1000)*(ES_mean["Method3_Opt.P",] - ES_mean["MinT.shr",])/
  sqrt(Sigma_Optimal.vs.MinT.shr)

p.val_Optimal.vs.MinT.shr <- 2*pnorm(-abs(t_Optimal.vs.MinT.shr))

