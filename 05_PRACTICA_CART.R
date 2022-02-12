
##IMPORTAR BASE DE DATOS
sitios<-read.table("C:/00_DATA/12_DOCENCIA_UJED/01_MGA/01_GEOSTADISTICA/AEE_2017/02_MATERIAL/01_DATA/06_NO_PARAM.txt", header=TRUE, sep="\t", dec=".") 


library(rpart)
##ESTABLECER EL MODELO LINEAL(MODELO REDUCIDO)
volumen<-rpart(VRTA~Band_1+Band_2+Band_3+Band_4+Band_5+Band_7+NDVI, data = sitios)

##GENERAR LOS ESTADISTICOS DE VALIDACION DEL MODELO
vol.pred<-predict(volumen, sitios[,])
rmse_sitios<-(crossprod(vol.pred-sitios[,1])/length(sitios))^0.5
r2_sitios<-1-(crossprod(vol.pred-sitios[,1]))/(crossprod(sitios[,1]-mean(sitios[,1])))
rmse_sitios
r2_sitios

##Impresion del arbol en forma tabular
print(volumen)

##Estudio del arbol en forma grafica
plot(volumen)
#Personalizacion del grafico
text(volumen, use.n=TRUE, all=TRUE, cex=.6)
summary(volumen)

# Crear un grafico postscript del arbol 
post(volumen, file ="C:/01_MGA/01_GEOSTADISTICA/AEE_2016/02_MATERIAL/01_DATA/arbol.ps", title = "Arbol de Clasificacion")

##Grafico de la validación cruzada del modelo
par(mfrow=c(1,2))
par
# Dos graficos por pagina
rsq.rpart(volumen)

##Nos ofrece una representacion visual de los resultados de validacion cruzada.
plotcp(volumen)

##Valores predichos de la validacion cruzada del modelo
xpred.rpart(volumen)

##Grafico de residuos vs predichos
summary(residuals(volumen))
plot(predict(volumen),residuals(volumen))


##Realizacion de podas de las ramas de nuestro arbol con la funcion prune
podaarbol=prune(volumen,cp= 0.12) 

#Cp indica el parámetro de complejidad de poda del arbol; extraer de la tabla.
print(podaarbol)
plot(podaarbol)
text(podaarbol, use.n=TRUE, all=TRUE, cex=.6)
par(mfrow=c(1,2));rsq.rpart(podaarbol)
plotcp(podaarbol)
summary(residuals(podaarbol))
plot(predict(podaarbol),residuals(podaarbol))


