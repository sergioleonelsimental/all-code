##IMPORTAR BASE DE DATOS
sitios<-read.table("C:/01_MGA/01_GEOSTADISTICA/AEE_2016/02_MATERIAL/01_DATA/06_NO_PARAM.txt", header=TRUE, sep="\t", dec=".") 

##vERIFICAR SI ESTA INSTALADA LA LIBRERIA

library(e1071)


#Código para dar rangos a los dos parámetros en svm
cost<-seq(from=1000, to=1, by=-200)
gamma<-seq(from=1, to=0.00001, by=-0.005)

#Código para seleccionar el mejor modelo usando los rangos antes definidos
svm.best<-tune.svm(VRTA~Band_1+Band_2+Band_3+Band_4+Band_5+Band_7+NDVI,data=sitios, cost=cost, gamma=gamma)
svm.best$performances
svm.best$best.performance
svm.best$best.model


#Código para calcular los predichos con el mejor modelo svm y para estimar el RMSE

svm.model<-svm(VRTA~Band_1+Band_2+Band_3+Band_4+Band_5+Band_7+NDVI,data=sitios, cost=1000, gamma=0.095)
svm.pred<-predict(svm.model, sitios[,])
rmse_svm<-(crossprod(svm.pred-sitios[,1])/length(sitios))^0.5
r2_svm<-1-(crossprod(svm.pred-sitios[,1]))/(crossprod(sitios[,1]-mean(sitios[,1])))
rmse_svm
r2_svm

summary(svm.model)
