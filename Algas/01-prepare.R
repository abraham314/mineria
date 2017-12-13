source('toolset.R')
source('utils.R')
source('metadata.R')
library(tidyr)
library(purrr)
library(ggplot2) 
library(dplyr)



#new<-german_data%>%separate(personal_status_and_sex,c("sex", "personal_status"), sep=':') 
#saveRDS(new,file="german-tidy")