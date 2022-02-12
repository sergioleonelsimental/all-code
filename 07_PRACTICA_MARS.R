
##IMPORTAR BASE DE DATOS
sitios<-read.table("C:/00_DATA/12_DOCENCIA_UJED/01_MGA/01_GEOSTADISTICA/AEE_2017/02_MATERIAL/01_DATA/06_NO_PARAM.txt", header=TRUE, sep="\t", dec=".") 
sitios<-read.table("C:/00_DATA/22_Sentinel_2017/BBDD_SPIFYS_2017a.txt", header=TRUE, sep="\t", dec=".") 

##vERIFICAR SI ESTA INSTALADA LA LIBRERIA
library(earth)

##ESTABLECER EL MODELO LINEAL(MODELO REDUCIDO)
mars.model<-earth(Biomass2 ~ banda1+banda2+banda3+banda4+ndvi+curva+plancur+profcur+slope+terranshape+trans+wi,data=sitios)

#Grafico para visualizar la distribucion normal y la distribucion del r2 a diferentes terminos
plot(mars.model)

#Forma de calcular el r2 y coefficients**
mars.model$rsq
mars.model$coefficients

#Para poder visualizar estadisticos del modelo generado por MARS**
summary(mars.model)
mars.model

#Graficos de cada variable que interviene en el modelo**
plotmo(mars.model)

#Para visualizar los valores predichos y resiuales**
mars.model$fitted.values
mars.model$residuals

#Para calcular la importancia de las variables**
evimp(mars.model)

summary(mars.model)
summary(mars.model, style="max")
summary(mars.model, style="bf")
