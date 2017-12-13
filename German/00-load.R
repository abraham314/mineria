source('utils.R')
source('metadata.R')
german_data<-readRDS('german')
german_colnames

colnames(german_data) <- german_colnames

german_data$good_loan <- as.factor(
  ifelse(
    german_data$good_loan == 1, 
    'GoodLoan', 
    'BadLoan'
  )
)

