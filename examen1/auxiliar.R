library(onehot)
hh<-
ww<-crimet[,!names(crimet) %in% c('modalidad','tipo','subtipo','mes')]

nums <- sapply(ww, is.numeric)
chars <- sapply(ww, is.character)
encoder<-onehot(ww[,chars],stringsAsFactors = TRUE,max_levels = 50)
auxen<-predict(encoder,ww[,chars])
ww[,nums]<-sapply(ww[,nums],function(x,a,b) data_scaling(x,mean(x,na.rm = TRUE),sd(x,na.rm = TRUE)))
aux<-as_tibble(cbind(auxen,ww[,nums]))

xm<-aux[!complete.cases(aux),]

x<-aux[complete.cases(aux),]

xm[1 , colSums(is.na(xm[1,])) == 0]
ll<-colnames(xm[1 , colSums(is.na(xm[1,])) == 0])
var<-colnames(xm[1 , colSums(is.na(xm[1,])) > 0])


v<-c("DELITOS PATRIMONIALES","ABUSO DE CONFIANZA","ABUSO DE CONFIANZA","DICIEMBRE")

missing_imputations_knn<-function(dat,v,num_vecinos){

#hh<-crimet[!complete.cases(crimet),][1,]
#print(pull(hh,modalidad))
fil<-dat%>%filter(modalidad==v[1],tipo==v[2],subtipo==v[3],mes==v[4])

#fil<-as_tibble(rbind(hh,fil))
#fil<-fil[,!names(fil) %in% v]

return(imputar_por_similitud2(fil,3))
}

#[1] "DELITOS PATRIMONIALES"           "DELITOS SEXUALES VIOLACION"      "HOMICIDIOS"                     
#[4] "LESIONES"                        "OTROS DELITOS"                   "PRIV. DE LA LIBERTAD SECUESTRO" 
#[7] "ROBO COMUN"                      "ROBO DE GANADO ABIGEATO"         "ROBO EN INSTITUCIONES BANCARIAS"
#[10] "ROBO EN CARRETERAS"  

#[1] "ABUSO DE CONFIANZA"         "DAniO EN PROPIEDAD AJENA"   "EXTORSION"                 
#[4] "FRAUDE"                     "DESPOJO"                    "VIOLACION"                 
#[7] "DOLOSOS"                    "CULPOSOS"                   "CULPOSAS"                  
#[10] "AMENAZAS"                   "ESTUPRO"                    "OTROS SEXUALES"            
#[13] "RESTO DE LOS DELITOS OTROS" "SECUESTRO"                  "CON VIOLENCIA"             
#[16] "SIN VIOLENCIA" 

#[1] "ABUSO DE CONFIANZA"               "DAniO EN PROPIEDAD AJENA"         "EXTORSION"                       
#[4] "FRAUDE"                           "CON VIOLENCIA"                    "SIN VIOLENCIA"                   
#[7] "SIN DATOS"                        "VIOLACION"                        "CON ARMA DE FUEGO"               
#[10] "CON ARMA BLANCA"                  "OTROS"                            "AMENAZAS"                        
#[13] "ESTUPRO"                          "OTROS SEXUALES"                   "RESTO DE LOS DELITOS OTROS"      
#[16] "SECUESTRO"                        "A CASA HABITACION"                "A NEGOCIO"                       
#[19] "DE VEHICULOS"                     "A TRANSPORTISTAS"                 "A TRANSEUNTES"                   
#[22] "ABIGEATO"                         "A BANCOS"                         "A CASA DE BOLSA"                 
#[25] "A CASA DE CAMBIO"                 "A EMPRESA DE TRASLADO DE VALORES" "A CAMIONES DE CARGA"             
#[28] "A AUTOBUSES"                      "A VEHICULOS PARTICULARES"    
select(crimet,modalidad,tipo,subtipo,mes,valu)%>%group_by(modalidad,tipo,subtipo,mes)%>%
  dplyr::summarise(vals = sum(valu,na.rm = TRUE))%>%arrange(desc(vals))



hh<-crimet[!complete.cases(crimet),][1,]

missing_imputations_knn(hh,crimet,v,3)


ter<-crimet[!complete.cases(crimet),]

sapply(ter,function(x,d,u,k) missing_imputations_knn(x,crimet,v,3))

vk<-rep(0,nrow(ter))
for(i in 1:nrow(ter)){
 vk[i]<-missing_imputations_knn(ter[i,],crimet,v,3)
}
