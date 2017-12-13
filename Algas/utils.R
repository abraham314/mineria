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
    algas_url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/coil-mld/analysis.data'
    
    algas <- read_csv(algas_url, 
                      col_names = algas_colnames,
                      na = 'XXXXXXX')
    f<-saveRDS(algas,file=f)
  }
  
  #return(f)
}

loads('algas')





german_decode <- function(columna){
  
  if(exists("german_codes")){
    source('metadata.R') # german_codes = ON
  }
  
  if( class(columna) %in% c('character', 'factor') ){
    
    # df(data.column)
    column_ok <- data.frame(col.id = as.character(columna),
                            stringsAsFactors = F)
    # query
    code_cat <- left_join(
      # left table
      column_ok,
      # crea cat, right table (al vuelo)
      german_codes %>%
        bind_rows() %>%
        gather(col.id, col.desc), # col_name[2] = col.desc
      by = "col.id" )
    # tratamiento NA's
    code_cat <- mutate(code_cat,
                       col.modif = ifelse(
                         is.na(col.desc),
                         col.id,
                         col.desc))
    column_ok <- code_cat$col.modif
  }
  else {
    column_ok <- columna
  }
  return(column_ok)
  rm(column_ok,code_cat)
}


gc2<-function(d,i){
  if((class(colnames(d[i])))=="character"){
    select(d,aux=noquote(colnames(d[i])),good_loan)%>%
      ggplot(aes(x=aux,fill=good_loan))+geom_bar()+ggtitle(noquote(colnames(d[i])))
  }
  else{
    select(d,aux=noquote(colnames(d[i])),good_loan)%>%
      ggplot(aes(x=aux,y=good_loan))+geom_boxplot()+ggtitle(noquote(colnames(d[i])))
    
  }
  
}


eda<-function(vars,d){
  nums <- sapply(d[vars], is.numeric)
  num<-colnames(d[vars][,nums])
  
  chars <- sapply(d[vars], is.character)
  char<-colnames(d[vars][,chars])
  
  if(length(num)!=0){
  pairs(~.,data=d[vars][,nums],main = "Variables numericas")
  }
  
  if(length(char)!=0){
  k<-structable(d[vars][,chars])
  pairs(k,highlighting=2,main = "Variables categoricas",diag_panel=pairs_diagonal_mosaic,mosaic_diag_panel_args=list(fill=grey.colors))
  }
 
  if(length(num)!=0 & length(char)!=0) {
  for (i in 1:length(vars)){
    
   for(j in 1:length(vars)){
      
      if(i!=j){
        md<-select(d[vars],vars[i],vars[j])
        if(colnames(md[1]) %in% num & colnames(md[2]) %in% char){
          print(ggplot(data=md,aes(y=md[1],x=md[2]))+geom_boxplot()+scale_x_discrete()+
            scale_y_continuous()+xlab(colnames(md[2]))+ylab(colnames(md[1])))
          
        }
 
      }
    }
  }
    
  }
  
}


