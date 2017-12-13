library(stringr)


limpia<-function(l){

l<-str_replace_all(l,' ','_') 
l<-str_replace_all(l,'/','_')
l<-str_to_lower(l)

}

