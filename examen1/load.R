library(tidyr)
library(purrr)
library(ggplot2) 
library(dplyr)
library(readr)
library(grid)
library(vcd)



loads<-function(f){
  if(file.exists(f)==TRUE){
    f<-readRDS(f)
  }
  else{
    
   crime <- read_csv('incidencia_delictiva.csv',col_names=TRUE)
    
    f<-saveRDS(crime,file=f)
  }
  
  #return(f)
}

loads('crime')
crime<-readRDS('crime')

