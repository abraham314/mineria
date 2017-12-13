library(tidyr)
library(purrr)
library(ggplot2) 
library(dplyr)
library(readr)
library(grid)
library(vcd)
library(GGally) 

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

#eda(colnames(algas),algas)

#ggplot(data=md,aes(x=md[1],y=md[2]))+geom_point()+scale_x_continuous()+scale_y_continuous()+xlab(colnames(md[1]))+ylab(colnames(md[2]))

indices_con_NAs<-function (data, porcentaje = 0.2) 
{
  n <- if (porcentaje < 1) {
    as.integer(porcentaje * ncol(data))
  }
  else {
    stop("Debes de introducir el porcentaje de columnas con NAs.")
  }
  indices <- which(apply(data, 1, function(x) sum(is.na(x))) > 
                     n)
  if (!length(indices)) {
    warning("No hay observaciones con tantos NAs\\n            (la respuesta de la función es vacía),\\n            no se recomienda indexar el data.frame con esto")
  }
  indices
}



Mode<-function(x,na.rm){
  xtab<-table(x)
  xmode<-names(which(xtab==max(xtab)))
  if(length(xmode)>1){
    xmode<-">1mode"
  }
  return(xmode)
}

imputar_valor_central <- function(data, colnames) {
  
  #bn<-data%>%select(colnames)
  for(var in colnames){
    if(lapply(data[,var],class)=="numeric"){
      #print("entro")
      #print(data[is.na(data[,var]),var])
      data[is.na(data[,var]),var]<-sapply(data[complete.cases(data[,var]),var],FUN=median)
      #print(data[is.na(data[,var]),var])
      #median(data[,var],na.rm=TRUE)
    }
    else if(lapply(data[,var],class)=="character"){
      #print("char") 
      #print(data[is.na(data[,var]),var]) 
      #print(Mode(data[is.na(data[,var]),var],na.rm = TRUE))
      data[is.na(data[,var]),var]<-Mode(data[,var],na.rm = TRUE)
    }
  }
  return(data)
}

imputar_valor_lm <- function(data,vard,var_independiente, modelo) {
  if(lapply(data[,var_independiente],class)=="numeric"){
    data[is.na(data[,vard] & is.na(data[,var_independiente])==FALSE),vard]<-
      modelo$coefficients[1]+modelo$coefficients[2]*data[is.na(data[,vard]) & 
                                                           is.na(data[,var_independiente])==FALSE,var_independiente]
    
    return(data)
  }
}

#funcion de distancias nominales Jaccard
simnom<-function(v1,v2){
  mm<-0
  for(i in 1:length(v1)){
    if (v1[i]==v2[i]){
      mm<-mm+1
    }
  }
  return(1-(mm/length(v1)))
}


imputar_por_similitud <- function(data, num_vecinos) { 
  
  nums <- sapply(data, is.numeric)
  chars <- sapply(data, is.character)
  aux<-as_tibble(cbind(data[,chars],scale(data[,nums])))
  
  
  xm<-aux[!complete.cases(aux),]
  
  xm_n<-xm[,nums]
  xm_c<-xm[,chars]
  
  x<-aux[complete.cases(aux),]
  
  for(j in 1:nrow(xm)){
    xm_n[j , colSums(is.na(xm_n[j,])) == 0]
    ll<-colnames(xm_n[j , colSums(is.na(xm_n[j,])) == 0])
    var<-colnames(xm_n[j , colSums(is.na(xm_n[j,])) > 0])
    
    lc<-colnames(xm_c[j , colSums(is.na(xm_c[j,])) == 0])
    
    d<-rep(0,nrow(x))
    ind<-rep(0,nrow(x))
    
    for(i in 1:nrow(x)){
      ind[i]<-i
      d[i]<-dist(rbind(xm_n[j,ll],x[i,ll]))+simnom(xm_c[j,lc],x[i,lc])
    }
    new<-data.frame(ind,d)
    new<-new[order(d),]
    val<-colSums(data[complete.cases(data),][c(new[1:num_vecinos,]$ind),var])/num_vecinos
    #print(val)
    #print(data[!complete.cases(data),])
    data[!complete.cases(data),var][1,]<-val
  }
  
  return(data)
}

signedLog10 <- function(x) {
  ifelse(abs(x) <= 1.0, sign(x)*log10(abs(x)))
}