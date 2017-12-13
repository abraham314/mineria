
#hacemos el dataset en formato tidy, los datos no lo son porque las columnas enero-diciembre en realidad es 
#una variable y no cumple el pricipio de datos limpios, entonces usamos gather para poner las columnas de
#meses como 1 variable y sus valores correspondientes y dejamos fijaslas otras variables que estan en formato
#correcto.

#tenemos el dataset crimet que ya esta en formato tidy.
crimet<-gather(data=crime,mes,valu,-c(1:6))


#limpiamos las variables con la funion clean.R
crime_colname<-colnames(crimet)
colnames(crimet) <- limpia(crime_colname)

content_fixing_modality<-function(dat,var){
 
  v<-select(dat,aux=var)%>%mutate(aux=str_replace_all(aux,'[(]',""))%>%
    mutate(aux=str_replace_all(aux,'[)]',""))
  return(pull(v,aux))
  
}

content_fixing_type<-function(dat,var){
  v<-select(dat,aux=var)%>%mutate(aux=str_replace_all(aux,'[(]',""))%>%
           mutate(aux=str_replace_all(aux,'[)]',""))%>%mutate(aux=str_replace_all(aux,'\xd1',"ni"))
  return(pull(v,aux))
}

content_fixing_subtype<-function(dat,var){
  v<-select(dat,aux=var)%>%mutate(aux=str_replace_all(aux,'[(]',""))%>%
           mutate(aux=str_replace_all(aux,'[)]',""))%>%mutate(aux=str_replace_all(aux,'\xd1',"ni"))
  return(pull(v,aux))
}

crimet$modalidad<-content_fixing_modality(crimet,'modalidad')
crimet$tipo<-content_fixing_type(crimet,'tipo')
crimet$subtipo<-content_fixing_subtype(crimet,'subtipo')

