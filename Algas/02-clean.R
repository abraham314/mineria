
summary(algas)
problems(algas) 
algas[20,]
algas[problems(algas)$row,] 

problematic_rows <- problems(algas)$row

algas[problematic_rows,] <- algas %>% 
  slice(problematic_rows) %>% 
  unite(col="all", -seq(1:6), sep = "/", remove=TRUE) %>%
  extract(all, into=c("NO3", "NH4", "resto"),
          regex="([0-9]*.[0-9]{5})([0-9]*.[0-9]*)/(.*)/NA", remove=TRUE) %>%
  separate(resto, into=names(algas)[9:18], sep="/", remove=TRUE)    

algas[19:20,]

algas %>%
  slice(problematic_rows) %>% head() 

algas %>% 
  slice(problematic_rows) %>%
  unite(col="all", -seq(1:6), sep="/", remove=T)

algas %>% 
  slice(problematic_rows) %>%
  unite(col="all", -seq(1:6), sep="/", remove=T) %>%
  extract(all, into=c("NO3", "NH4", "resto"),
          regex="([0-9]*.[0-9]{5})([0-9]*.[0-9]*)/(.*)/NA", remove=T) 

algas <- readr::type_convert(algas) 

algas 

#algas <- algas %>%
#  mutate_all(funs(algas_clean))

algas

summary(algas)

glimpse(algas)


x <- as.data.frame(abs(is.na(algas))) # df es un data.frame

head(x)

# Extrae las variables que tienen algunas celdas con NAs
y <- x[which(sapply(x, sd) > 0)] #se usa sd ya que si sd>0 entonces sabemos que hay NA's

# Da la correación, un valor alto positivo significa que desaparecen juntas.
cor(y) 
#-------------------------------------------------------------------------------------------
library(corrplot)
#tarea 1 library(corplot) corplot(cor(y))
#reporte de Missings:
library(Amelia)
missmap(algas, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 
#podemos ver en esta grafica las variables con mas missings cla,cl y ahora calculamos el % de NAs que 
#tiene cada variable...
mis<-colSums(is.na(algas))/nrow(algas)*100
mis

#numero de observaciones que tienen al menos 1 missing en alguna columna
nrow(algas[!complete.cases(algas),])
#observaciones con missings:
algas[!complete.cases(algas),]

#matriz de correlacion de misings...
corrplot(cor(y))

#de esta grafica podemos interpretar que los variables no3,nh4 y opo4 son missings al parejo es decir 
#si una de ellas es missing las otras 2 tambien lo seran.
#tambien pasa un eefecto similar con las variables cl y chla

#-----------------------------------------------------------------------------------------------


summary(algas[-grep(colnames(algas),pattern = "^a[1-9]")]) # Nota el uso del grep


  algas%>%select(-starts_with("a")) %>%
  summary()

nrow(algas[!complete.cases(algas),])

algas_con_NAs <- algas[!complete.cases(algas),]

algas_con_NAs[c('max_ph', 'min_o2', 'cl', 'no3', 'nh4', 'opo4', 'po4', 'chla')]%>%print(n = 33)

# ¿Cuántos NAs hay por observación?
apply(algas, 1, function(x) sum(is.na(x)))

algas[apply(algas, 1, function(x) sum(is.na(x))) > 2,] 

indices_con_NAs(algas, 0.2)

indices_con_NAs(algas, 0.8) 

algas <- algas[-indices_con_NAs(algas, 0.2),]
dim(algas)

#dataset$cat_with_NAs_fix <- ifelse(is.na(dataset$cat_with_NAs),
#                                   "missing",
#                                   ifelse(dataset$ccat_with_NAs == TRUE,    
#                                          # o el valor que sea
#                                          "level_1",
#                                          "level_2"))

print(ggpairs(algas[complete.cases(algas),],lower = list(combo =wrap("facethist", binwidth=0.8))))

#----------------------------------------------------------------------------------------------------
#Ejer2
#imputar max_ph con la mediana porque su distribucion es quasi normal..
algas<-imputar_valor_central(algas,{"max_ph"})

#--------------------------------------------------------------------------------------------------

algas[,-c(1:3)] %>%
  cor(use="complete.obs") %>%
  symnum() 



print(ggplot(data=algas) + 
  aes(x=opo4, y=po4) + 
  geom_point(sape=1) + # Usamos una bolita para los puntos
  geom_smooth(method=lm, se=FALSE)#+theme_hc()
)
#algas <- algas[-indices_con_NAs(algas, 0.2),]
modelo <- lm(po4 ~ opo4, data=algas)
modelo
#------------------------------------------------------------------------------------------------
#Ejercicio 3

#hacemos una matriz de correlaciones para idetificar que variables pueden imputar otras con una regresion
#simple...
#nums <- sapply(algas, is.numeric)
#h<-algas[complete.cases(algas),]
#corrplot(cor(h[,nums]))
#dea cuerdo con la lista de variables podemos ver aqullas tienen missings y buscar las relaciones..
#colSums(is.na(algas[,nums]))
#observamos que necesitamos chla,cl,po4,min_o2,max_ph, entonces vamos a buscar las variables mas
#relacionadas y contruimos los modelos
#cl se relaciona con po4 entonces 
#modelocl <- lm(cl ~ po4, data=h)
#modelocl
#chla se relaciona mas con max_ph y viceversa
#modelochla <- lm(chla ~ max_ph, data=h)
#modelochla

#modelomax<- lm(max_ph~chla, data=h)
#modelomax
#min_o2 con po4
#modelomin <- lm(min_o2 ~ po4, data=h)
#modelomin

#usemos las el modelo po4 y opo4 para imputar los valores de la primera:
#nb<-data.frame(algas[,c("po4","opo4")])#hacemos un datframe de estas 2 variables
#imputando po4 con regresion...
#nb<-imputar_valor_lm(nb,"po4","opo4",modelo)
#verificando....
algas[28,]#=42.8970464+1.2930609*4=48.06929

#imputamos po4 con opo4 porque su relacion es casi lineal...
algas<-imputar_valor_lm(algas,"po4","opo4",modelo)
#------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#Ejercicio 4 
#imputamos cl, chla y min_o2 con 3-vecinos mas cercanos dado que sus distribuciones estan sesgadas
algas<-imputar_por_similitud(algas,3)


#------------------------------------------------------------------------------------------

#transformaciones de variables
#dado que max_ph se distribuye casi normal solo la vamos a estandarizar, min_o2 no tienen tanto sesgo
#tambien la estadirizaremos
algas$max_ph<-scale(algas$max_ph)
algas$min_o2<-scale(algas$min_o2)

#al resto de las variables numericas las transformaremos en logaritmo dado que solo tienen valores
#positivos y estan sesgadas  esta funcion abre valores pequeños y agrupa valores grandes..
vars<-c("cl","no3","nh4","opo4","po4","chla","a1","a2","a3","a4","a5","a6","a7")
#colnames(algas[,vars])
algas[,vars]<-log10(algas[,vars]+1) 
#ggpairs(log10(algas[,vars]+1))

print(ggpairs(algas[,vars]))
ggpairs(algas[,vars],lower = list(combo =wrap("facethist", binwidth=0.8)))




