loads<-function(f){
  if(file.exists(f)==TRUE){
    f<-readRDS(f)
  }
  else{
    german_url <- paste0('http://archive.ics.uci.edu/ml',
                         '/machine-learning-databases/statlog',
                         '/german/german.data')
    german_data <- read_delim(german_url, 
                              col_names=F,
                              delim=" ")
    f<-saveRDS(german_data,file=f)
  }
  
  #return(f)
}

loads('german')





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
