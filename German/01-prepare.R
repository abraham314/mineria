source('utils.R')
source('metadata.R')
library(tidyr)
library(purrr)
library(ggplot2) 
library(dplyr)

german_data  <- german_data %>% mutate_all(funs(german_decode))
