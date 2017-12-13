library(ggplot2)
library(plotly)
library(GGally)
library(questionr)
library(knitr)
#--------------Funcion EDA------------------------------------------------
eda_analysis_single_variable<-function(dat){
  chars<-sapply(dat,is.character)
  nums<-sapply(dat,is.numeric)
  if(nrow(dat[,chars])!=0){
    print("EDA DE VARIABLES CATEGORICAS")
    for(var in colnames(dat[,chars])){
      print(paste0("frecuencias de variable: ",var))
      sal<-select(dat,aux=var,valu) %>% 
              group_by(aux) %>% 
              dplyr::summarise(crimenes = sum(valu,na.rm = TRUE)) %>% 
              arrange(desc(crimenes))%>%mutate(proportion=round((crimenes/sum(crimenes))*100,digits=1))
      colnames(sal)[1]<-var
      print(sal)
      
    }
  
  }
  
  if(nrow(dat[,nums])!=0){
    
    print("EDA DE VARIABLES NUMERICAS")
    print(summary(dat[,nums]))
    
  }
  
  
  
}


#Funcion GEDA1---------------------------------------------------------------------------------
geda_analysis_single_variable<-function(dat){
  chars<-sapply(dat,is.character)
  nums<-sapply(dat,is.numeric)
  if(nrow(dat[,chars])!=0){
    print("GEDA DE VARIABLES CATEGORICAS")
    for(var in colnames(dat[,chars])){

      p<-select(dat,valu,aux=var) %>% 
        group_by(aux) %>% 
        dplyr::summarise(crimenes = sum(valu,na.rm = TRUE)) %>% 
        arrange(desc(crimenes)) %>% 
        ggplot(.) + 
        geom_bar(aes(x=reorder(aux, crimenes), y = (crimenes/sum(crimenes))*100), stat="identity", fill="gray") + 
        coord_flip() + 
       # theme_hc() + 
        ylab('% de crimenes') + 
        xlab(var)
      print(p)
          
    }
    
  }
  
  if(nrow(dat[,nums])!=0){
    
    print("GEDA DE VARIABLES NUMERICAS")
    for(var in colnames(dat[,nums])){  
      if(var!='valu'){
      pk<-select(dat,aux=var,valu) %>% 
        group_by(aux) %>% 
        dplyr::summarise(crimenes = sum(valu,na.rm = TRUE)) %>% 
        arrange(desc(crimenes)) %>% 
        ggplot(.) + 
        geom_point(aes(x=aux,y=crimenes),fill='blue') +
        geom_smooth(aes(x=aux,y=crimenes),method = 'loess')+
        # theme_hc() + 
        ylab('crimenes') + 
        xlab(var)
      
    
   print(pk)
      }
    }  
  }
  
  
  
}

#Funcion GEDA2 pairwise---------------------------------------------------------------------------------
geda_analysis_pairwise_variables<-function(dat){
  chars<-sapply(dat,is.character)
  nums<-sapply(dat,is.numeric)
  if(nrow(dat[,chars])!=0){
    print("GEDA DE VARIABLES CATEGORICAS")
    for(var in colnames(dat[,chars])){
      for(var2 in colnames(dat[,chars])){
       if(var!=var2){
    pw<-select(dat,aux1=var,aux2=var2,valu) %>% 
      group_by(aux1,aux2) %>% 
      dplyr::summarise(crimenes = sum(valu,na.rm = TRUE)) %>% 
    arrange(desc(crimenes)) %>% 
     ggplot(.) + geom_bar(aes(x=reorder(aux1, crimenes),y=crimenes),stat='identity')+facet_grid(~aux2)+
      # theme_hc() + 
    ylab('crimenes') + 
     xlab(var)+ggtitle(paste0("GRÁFICO SEPARADO POR:",var2))
    #print(pw)
      
    print(pw)
       }
      }
    }
  }
  
  if(nrow(dat[,nums])!=0){
    
    print("EDA DE VARIABLES NUMERICAS")
    
  }
  
  
  
}



#para el eda se descartan las variables ano y clave, dado que a pesar de ser numericas no tiene sentido\
#ver estadisticosde una variale numerica para estas.
eda_analysis_single_variable(crimet[,!names(crimet) %in% c('ano','clave_marco_geoestadistico_nal')])

geda_analysis_single_variable(crimet)

#Sólo hacemos Geda para las categorías más relevantes de las variables tipo,subtipo,mdalidad y entidad
#porque son las que tienen categorías más diferenciadas.
geda_analysis_pairwise_variables(
  select(crimet,modalidad,tipo,subtipo,entidad,mes,valu)%>%filter(modalidad %in% c('ROBO COMUN','OTROS DELITOS',
                      'DELITOS PATRIMONIALES','LESIONES'),
                                                 tipo %in% c('SIN VIOLENCIA','RESTO DE LOS DELITOS OTROS',
'CON VIOLENCIA','DOLOSAS'),subtipo %in% c('RESTO DE LOS DELITOS OTROS','OTROS','DE VEHICULOS',
                                          'DAniO EN PROPIEDAD AJENA'),entidad %in% c('MEXICO',
                                                                                     'CIUDAD DE MEXICO',
                                                                                     'BAJA CALIFORNIA',
                                                                                     'JALISCO '))
                                 )


