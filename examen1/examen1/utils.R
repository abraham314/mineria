
#funcion de normalizacion
data_normalization<-function(var,max,min){
  
  return((var-min)/(max-min))
  
}

#funcion de estandarizacion
data_scaling<-function(var,m,s){
  return((var-m)/s)
}