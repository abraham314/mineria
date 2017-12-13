library(tidyr)
library(purrr)
library(ggplot2) 
library(dplyr)
library(readr)
library(grid)
library(vcd)
library(stringr)


clean_names<-function(l){
  
  #l<-str_replace_all(l,'.','_') 
  l<-str_replace_all(l,'-','_') 

}
#funcion que pide d.-dataframe, k.-numero de columna que es es target i.e. clase
geda<-function(d,k){
  
  for(i in 1:ncol(d)){
    
  if(i!=k){
    
  if((class(pull(d[,i])))=="character"){
    print(i)
    
    p<-select(d,aux=noquote(colnames(d[i])),target=noquote(colnames(d[k])))%>%
      ggplot(aes(y=..count../sum(..count..),x=aux,fill=target))+geom_bar()+
      ggtitle(paste('target=',noquote(colnames(d[k]))))+ylab('percentage')+xlab(noquote(colnames(d[i])))
  }
  else{
    p<-select(d,aux=noquote(colnames(d[i])),target=noquote(colnames(d[k])))%>%
      ggplot(aes(y=aux,x=target))+geom_boxplot()+ggtitle(noquote(colnames(d[i])))+
      ylab(noquote(colnames(d[i])))+xlab(noquote(colnames(d[k])))
    
  }
    print(p)
  }
    
  }
  
}




