library(psych)

data_profiling<-function(dat){
  chars<-sapply(dat,is.character)
  nums<-sapply(dat,is.numeric)
  if(nrow(dat[,chars])!=0){
    for(var in colnames(dat[,chars])){
      
      num_val_distintos<-n_distinct(dat[,var])
      val_distintos<-list(c(unique(dat[,var])))
      uniqueness<-n_distinct(dat[,var])/nrow(dat[,var])
      n_missings<- sum(is.na(dat[,var]))
      f<-table(dat[,var])
      moda<-rownames(data.frame(f[which.max(f)]))
      
      aa<-data.frame(num_val_distintos,uniqueness,n_missings,moda)
      rownames(aa)<-var
      print(paste0("estadisticos de la variable: ",var,"------------------------------------------"))
      print(tbl_df(aa))
      print("")
      print(paste0("valores_distintos: ",val_distintos))
      print("--------------------------------------------------------------------------------------")
      
     
      
    }
  }
  
  if(nrow(dat[,nums])!=0){
    
    print("estadisticos de valores numericos:")
    #summary(dat[,var])
      df<-dat[,nums]
      tmp <- do.call(data.frame, 
                     list(max = apply(df[complete.cases(df),], 2, max),
                          min = apply(df[complete.cases(df),], 2, min),
                          mean = apply(df[complete.cases(df),], 2, mean),
                          median = apply(df[complete.cases(df),], 2, median),
                          sd = apply(df[complete.cases(df),], 2, sd),
                          Q1 = apply(df[complete.cases(df),],2,quantile)[2,],
                          Q3 = apply(df[complete.cases(df),],2,quantile)[4,],
                          n=apply(df,2,length),
                          n_miss=apply(is.na(df),2,sum),
                          skew=apply(df[complete.cases(df),],2,skew),
                          kurt=apply(df[complete.cases(df),],2,kurtosi),
                          n_unique=apply(df,2,n_distinct)
                          #uniqueness=apply(df,2,)
                          
                          ))
      tmp$uniqueness<-tmp$n_unique/tmp$n
      #tmp$Q1<-tmp$
      print(tmp)
      
    
  }
  
  
  
  
  
  
}


data_profiling(crimet)

