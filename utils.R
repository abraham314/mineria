
#Función forward...
forward_filtering<-function(dat,targ,vars){
  ll<-list()
  rfinal<--1
  for(k in (1:length(vars))){
    score_max<--1
    for(var in vars){
      newll<-append(ll,var)
      f<-paste(targ,'~',paste(newll, collapse="+"),sep = "")
      mod<-lm(f,data=dat)
      score<-summary(mod)$fstatistic[1]
      if(score>score_max){
        rs<-round(summary(mod)$r.squared,2)
        score_max<-score
        var_i<-var
        rnewll<-newll
      }
      
      
    }
   if(rs>rfinal){
     print(rfinal)
     rfinal<-rs
     ll<-rnewll
     print(as.character(ll))
     print(score_max)
     print(rs)
     vars<-vars[vars!=var_i]
   }  
    
  }
  
 modelo_fin<-lm(paste(targ,'~',paste(ll, collapse="+"),sep = ""),data=dat)  
 print(summary(modelo_fin))
 return(as.character(ll))
}

#Función fast correlation...
FCB_filtering<-function(dat,umbral,target){
  kp<-list()
  features<-colnames(dat)
  features<-features[features!=target]
  numf<-1
  aux<-dat
  
  while(numf>0){
    dfc<-cor(dat[,features])
    dfc<-data.frame(dfc)
    #print(dfc)
    tabcor<-data.frame(features)
    colnames(tabcor)<-'variable'
    tabcor$correlacion<-0
    #print(tabcor)
    j<-1
    #print(tabcor$variable)
    #print(length(tabcor$variable))
    for(i in tabcor$variable){
      #print(tabcor$variable)
      
      tabcor$correlacion[j]<-cor(aux[i],aux[,target])
      j<-j+1
      
    }
    tabcor$variable<-as.character(tabcor$variable)
    tabcor$abscor<-abs(tabcor$correlacion)
    #print(tabcor)
    p<-tabcor%>%arrange(desc(correlacion))%>%
      ggplot(.) +
      geom_bar(stat = "identity", aes(x=variable,y=correlacion,fill = correlacion)) + 
      scale_y_continuous()
      #ggplot(.)+geom_bar(aes(x=correlacion,y=variable),stat="identity", fill="gray")+
      #scale_x_continuous()
      
    print(p)
    
    sal<-tabcor%>%arrange(desc(abscor))
    sal<-sal[sal$abscor>umbral,]
    #print(sal)
    
    numf<-nrow(sal)
    
    
    if(numf>0){
      #print(sal)
      #print(sal$variable[1])
      maxvar<-sal$variable[1]
      #print(dfc[,maxvar])
      kp<-append(kp,maxvar)
      #print(paste0('maxvar ',maxvar))
      #print(dfc[abs(dfc[,maxvar])>umbral,maxvar])
      ll<-row.names(dfc[abs(dfc[,maxvar])>umbral,])
      #print(paste0('vals ',ll))
      print(paste0('tira ',ll[ll!=maxvar]))
      #print(sal)
      lc<-c(ll)
      #print(lc)
      aux<-aux[,!(names(aux) %in% lc)]
      features<-colnames(aux)
      features<-features[features!=target]
      
    }
  }
  
  return(as.character(append(kp,colnames(aux))))
}

#Funcion corrrelation filtering
correlation_filtering<-function(dat,target,umbral){#esta funcion regresa la lista de variables a eliminar!
  elimina<-list()
  dfc<-cor(dat)
  dfc<-data.frame(dfc)
  dfc[dfc==1]<-NA#eliminamos las correlaciones entre mismas variables
  crf<-sapply(abs(dfc), max, na.rm = TRUE)
  p<-ggplot(data=NULL,aes(x='variable',y=as.vector(crf)))+geom_boxplot()+
    ggtitle('Distribución de correlaciones máximas por variable en valor absoluto')
  print(p)
  
  ll<-colnames(dat)
  for(var in ll[ll!=target]){
    if(max(abs(dfc[,var]),na.rm = TRUE)>umbral){
      corvar<-row.names(dfc[abs(dfc[,var])==max(abs(dfc[,var]),na.rm = TRUE),])
      corvar<-corvar[corvar!="NA"]
     
      print(paste0(var,corvar))
      #Cuál tiramos?... la que este menos correlacionada con el target
      if(abs(cor(dat[,var],dat[,target]))<abs(cor(dat[,corvar],dat[,target]))){
        el<-var
        print(paste0('elimina ',var))
        
      }
      else{
        el<-corvar
        print(paste0('elimina ',corvar))
      }
      
      if(!el %in% elimina){
        elimina<-append(elimina,el)
      }
      
    }
    
  }
  return(elimina)
}


