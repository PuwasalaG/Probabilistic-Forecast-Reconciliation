# A script to make all reports as pdf files
library(tidyverse)
library(rmarkdown)

simtable<-read_csv('../Base_Forecasts/SimulationTable.csv')
for (scen in 1:7){
  simj<-simtable[scen,] #Extract row of table
  outname<-paste0('Reports/Results','_',simj$dist,'_',simj$trend,'_',simj$model,'.pdf')
  render('maketables.Rmd',output_file = outname,params=list(args=simj))
}
