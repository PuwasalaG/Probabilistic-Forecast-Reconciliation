rm(list=ls())
library(tidyverse)

fn<-dir('../Reconciled_Results/')[1:23]

converge<-function(x){
  a<-readRDS(paste0('../Reconciled_Results/',x))
  ar<-a[[sample(1:500,1)]]
  plot(1:length(ar$val_store),ar$val_store,'l',axes=FALSE,xlab='',ylab='')
  
}

par(mfrow=c(4,6),mar=c(0.1,0.1,0.1,0.1))
for (i in 1:23){
  converge(fn[i])
}
