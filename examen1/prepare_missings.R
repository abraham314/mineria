source('utils.R')
library(onehot)

#Funcion imputar mssings con mediana, parametros dat es labase de datos y ves el vector de columnas que se
#necesita agrupar para obtener la mediana del segmento, ejemplo si queremos imputar por modalidad,
#tipo y subtipo entonces v<-c('modalidad','tipo','subtipo')
missing_imputations_centrality<-function(dat,v){
  
  g<-select(dat,v,valu)%>%group_by_at(vars(-(length(v)+1)))%>%
    dplyr::summarise(vals = median(valu,na.rm = TRUE))%>%arrange(desc(vals))
  
  mis<-dat[!complete.cases(dat),]
  
  limp<-merge(x=mis,y=g,all.x = TRUE)
  
  return(pull(limp,vals))
    
  
}


v<-c('modalidad','tipo','subtipo')
print(paste0('ejemplo de imputacion con la funcion missing_imputations_centrality usando las el vectorde variables:'
             ,v))
print("la salida es el vector de todos los valores(medianas) a imputar de la tabla crimet")
print(missing_imputations_centrality(crimet,v)[1:200])

#crime[is.na(jh[,'valu']),'valu']<-missing_imputations_centrality(crimet,c('modalidad','tipo','subtipo'))
#gh<-select(crimet,v,valu)%>%group_by(-(length(v)+1))%>%
#  dplyr::summarise(crimenes = median(valu,na.rm = TRUE))
#pull(g,crimenes)


#funcion que recibe como parametros los datos y el vector del valor de modalidad
#o si usamos mas campos poder ampliarlo, la idea es modelar el numero
#crimenes dependiendo de su modalidad solamente ya que el Geda nos mostró que es quien 
#concentra más crimenes en menpos categorías entonces usamos esta variable como grupo para
#modelar.

missing_imputations_lm<-function(dat,v){
  dat<-dat%>%filter(modalidad==v[1])
  #print('ok1')
  seg<-dat[complete.cases(dat),!names(dat) %in% c('modalidad')]#seleccionamos las variables y los casos completos
  #print('ok2')
  mod<-lm(valu~.,seg)#hacemos el modelo de regresion usando todas as entradas vector v aunque son 
  #categoricos los datos R entiende que hacer una regresion asi implica trabajar con dummys, es decir
  #da lo mismo poner la variable directa o hacer onehot encoder por cada una de ellas
  pr<-predict(mod,dat[!complete.cases(dat),])#predecimos los datos
  pr<-sapply(pr,function(x){if(x<0){return(0)} else{return(x)}})# si hubiera predicciones negativas 
  #las hacemos cero.
    

  
  return(pr)
  
}

v<-c('ROBO COMUN')
print(paste0('ejemplo de imputacion usando missing_imputations_lm con la categoria:',v))
print("la salida(se imprimen primeras 20 obs.) vector de predicciones por modalidad")
#este ejemplo lo hacemos sin las variables ano y clave dado que no son variables numericas y no funcionan
#mucho para imputar por regresion.

new<-crimet[,!names(crimet) %in% c('ano','clave_marco_geoestadistico_nal')]
print(missing_imputations_lm(new,v)[1:20])






v<-c("DELITOS PATRIMONIALES","ABUSO DE CONFIANZA","ABUSO DE CONFIANZA","DICIEMBRE")
#imputacion por knn, en este caso decidimos utilizar además de usar modalidad, tipo, subtipo, también
#el mes, aunque según el geda que hicimos la distribución del mes con respecto a las otras vaibles no
#tiene diferencia entre sus categorías, es decir, el mes se distribuye uniforme con respecto a
#los crímenes, en este caso lo usamos por conveniencia al tiempo de ejecución de la función.
missing_imputations_knn<-function(data,v,num_vecinos){

  data<-data%>%filter(modalidad==v[1],tipo==v[2],subtipo==v[3],mes==v[4])
  if(nrow(data[!complete.cases(data),])!=0){
  
  nums <- sapply(data, is.numeric)
  chars <- sapply(data, is.character)
  encoder<-onehot(data[,chars],stringsAsFactors = FALSE)#hacemos dummies las variables categóricas
  auxen<-predict(encoder,data[,chars])
  
  if(ncol(data[,nums])!=0){
  #usamos la función data_scaling que creamos en utils para estandarizar los datos numéricos
  anum<-sapply(data[,nums],function(x,a,b) data_scaling(x,mean(x,na.rm = TRUE),sd(x,na.rm = TRUE)))
  aux<-as_tibble(cbind(auxen,anum))
  
  #aux<-as_tibble(cbind(auxen,scale(data[,nums])))
  }
  
  else{
    aux<-auxen
  }
  
  xm<-aux[!complete.cases(aux),]
  
  #xm_n<-xm[,nums]
  #xm_c<-xm[,chars]
  
  x<-aux[complete.cases(aux),]
  
  for(j in 1:nrow(xm)){
    xm[j , colSums(is.na(xm[j,])) == 0]
    ll<-colnames(xm[j , colSums(is.na(xm[j,])) == 0])
    var<-colnames(xm[j , colSums(is.na(xm[j,])) > 0])
    
    #lc<-colnames(xm_c[j , colSums(is.na(xm_c[j,])) == 0])
    
    d<-rep(0,nrow(x))
    ind<-rep(0,nrow(x))
    
    for(i in 1:nrow(x)){
      ind[i]<-i
      d[i]<-dist(rbind(xm[j,ll],x[i,ll]))
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
}

print('Para la imputacion por knn usamos as categorias')
print(v)
print('la salida es el dataframe de la categoria seleccionada con los datos ya imputados con 3 vecinos')
print(missing_imputations_knn(crimet,v,3))