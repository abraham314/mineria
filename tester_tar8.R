# Loading
data(mtcars)
# Print the first 6 rows
head(mtcars, 6)
mtcars[,'mpg']

model<-lm(mpg~+wt,data=mtcars)

model$terms

summary(model)$fstatistic[1]


a<-list('wt','cyl')
sm<-for(i in (2:length(a))){
  paste0()
}
f<-paste0('mpg','~',a)

wf<-paste('mpg','~',paste(a, collapse="+"),sep = "")

m<-lm(wf,data=mtcars)

sa


ld<-colnames(mtcars)
features<-colnames(mtcars)
features<-features[features!='mpg']

sal<-forward_filtering(mtcars,'mpg',features)

FCB_filtering(mtcars,.7,'mpg')

correlation_filtering(mtcars,'mpg',.875)


