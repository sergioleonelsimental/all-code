##IMPORTAR BASE DE DATOS
spifys<-read.table("C:/00_DATA/12_DOCENCIA_UJED/01_MGA/01_GEOSTADISTICA/AEE_2017/02_MATERIAL/01_DATA/04_REG_SPIFyS.txt", header=TRUE, sep="\t", dec=".") 

# permite aceder directamente a las variables
attach(spifys)

#Obtener la grafica de dispersion 

cor(spifys)

#Graficar dos variables
plot(VRTA,Band_3)


#Analisis de regresion
regresiom <-lm(VRTA ~ Band_3+Band_7+Band_1+NDVI, data = spifys)

coefficients(regresiom)
#Resumen del analisis de regresion
summary(regresiom)

#Analisis del modelo de regresion
residuos <- rstandard(regresiom)
predichos <- fitted(regresiom)
plot(predichos, residuos, main="Valores predichos vs resiuos")
plot(predichos, VRTA, main="Valores predichos vs observados")

rmse<-(crossprod(predichos-spifys[,1])/length(spifys))^0.5
rmse

head(predichos)




