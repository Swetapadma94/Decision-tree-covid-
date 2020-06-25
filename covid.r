library(readr)
library(readxl)
library(caret)
library(C50)
covid<-read_xlsx("E:\\srinivas excelr\\data set\\covid1.xlsx")
View(covid)
summary(covid)
str(covid)
is.na(covid)
attach(covid)
str(covid$RedZone)
covid$RedZone<-as.factor(covid$RedZone)
covid$`District(odisha)`<-as.factor(covid$`District(odisha)`)
intrain<-createDataPartition(covid$RedZone,p=.70,list=F)
training<-covid[intrain,]
testing<-covid[-intrain,]
str(training$RedZone)

#Model Building
model<-C5.0(RedZone~.,data = training,)
plot(model)
summary(model)
##Predict for test data set
pred<-predict.C5.0(model,newdata=testing[,-6])
summary(pred)
x<-table(testing$RedZone,pred)
sum(diag(x)/sum(x))
plot(pred)
