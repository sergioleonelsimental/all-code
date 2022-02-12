##IMPORTAR BASE DE DATOS
sitios<-read.table("C:/00_DATA/04_Asesor/02_MGARFA/03_JoseLuis/BASE DE DATOS/BASE_DATOS_pmls_22_01_2018.txt", header=TRUE, sep="\t", dec=".") 


##vERIFICAR SI ESTA INSTALADA LA LIBRERIA
library(randomForest)



### Para determinar el numero de veces que se desea correr y cambiarlo cada vez que se corra**
set.seed(85)

##ESTABLECER EL MODELO LINEAL(MODELO REDUCIDO)
forest<-randomForest(VRTA~Band_1+Band_2+Band_3+Band_4+Band_5+Band_7+NDVI,data=sitios,mtry=7,importance=T)
summary(forest)
importance(forest)

### Muestra el error de acuerdo al num de arboles**
plot(forest)
### Muestra una grafica de la importancia de las variables***
varImpPlot(forest,type=1)
### valores predichos, grafica predvsobser***
plot(forest$predicted~sitios$VRTA)
forest$predicted
### Estadististicos de randomForest***
forest
