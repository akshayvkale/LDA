library(MASS)

library(mvtnorm)

library(mda)

library(ggplot2)

?rmvnorm

n<-500

x11<-rmvnorm(n,mean = c(-4,-4))

x12<-rmvnorm(n,mean = c(0,4))

x13<-rmvnorm(n,mean = c(4,-4))

x21<-rmvnorm(n,mean=c(-4,4))

x22<-rmvnorm(n,mean = c(4,4))

x23<-rmvnorm(n,mean = c(0,0))

x31<-rmvnorm(n,mean = c(-4,0))

x32<-rmvnorm(n,mean = c(0,-4))

x33<-rmvnorm(n,mean = c(4,0))

x<-rbind(x11,x12,x13,x21,x22,x23,x31,x32,x33)

x

?gl()

train<-data.frame(x,y=gl(3,n*3))

View(train)

lda_out<-lda(y~.,train)

qda_out<-qda(y~.,train)

mda_out<-mda(y~.,train)

#Generates test data that will be used to generates desision boundaries

contour_data<-expand.grid(X1=seq(-8,8,length=300),X2=seq(-8,8,length=300))

contour_data

dim(contour_data)

dim(train)

#classifing test data

set.seed(123)

lda_prdict<- data.frame(contour_data,
                        y=as.numeric(predict(lda_out,contour_data)$class))
lda_prdict

qda_predict<-data.frame(contour_data,
                        y=as.numeric(predict(qda_out,contour_data)$class))

p <- ggplot(train,aes(x=X1,y=X2,color=y))+geom_point()

p

colnames(train)

p+stat_contour(aes(x=X1,y=X2,z=y),data=lda_prdict)

p+stat_contour(aes(x=X1,y=X2,z=y),data=qda_predict)

lda_prdict

mda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(mda_out, contour_data)))

p+stat_contour(aes(x=X1,y=X2,z=y),data=mda_predict)

ggplot(data=mda_predict,aes(x=X1,y=X2,color=y))+geom_point()+
  stat_contour(aes(x=X1,y=X2,z=y),data=mda_predict)

ggplot(data=qda_predict,aes(x=X1,y=X2,color=y))+geom_point()+
  stat_contour(aes(x=X1,y=X2,z=y),data=qda_predict)

ggplot(data=lda_prdict,aes(x=X1,y=X2,color=y))+geom_point()

View(lda_prdict)

#________________________________________________________________

