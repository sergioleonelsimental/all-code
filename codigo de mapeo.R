dato <- read.csv(file.choose())
j1<-read.table('clipboard') ##carga depuntos

library(raster)
library(rgdal)
library(dismo)
library(rjava)
library(jsonlite)
library(sp)
library(maptools)

##solo coordenadas
Especie_modela<- j1[,2:3]
head (Especie_modela)
data(wrld_simpl)

plot(wrld_simpl)## descarga mapa

## elige zona
plot(wrld_simpl, xlim=c(-120,-70), ylim=c(15,32), axes=TRUE, col="red") ## elige zona
## contorno del mapa
box()

## mapear puntos
points(Especie_modela$log, Especie_modela$lat, col="20", pch=20, cex=2)

points(Especie_modela$log, Especie_modela$lat, col="blue", cex=2)

## bariables ambiemtales
BClim =getData("worldclim", var ="bio", res=5, lon=-80, lat=10)

## res resolucion 1minutos 5 2.5 minut y 30 second
plot(BClim,1)
## recorta la exencion
YbrevRange = extent (-120, -80, 10,40)
plot(YbrevRange, add=TRUE, col ="red", lwd =2 )

## recorta datos de world clima
Bclim = crop(BClim, YbrevRange)
## recorta datos de world clima guardar recorte
writeRaster(Bclim, filename = "var_amb_recort.grd", overwrite=T)
##   cargar recorte
Bclim = brick ("var_amb_recort.grd")

## catalago raster
predictors <- stack (Bclim)
class(predictors)
predictors
names(predictors)
plot(predictors,1)

## estracion de valores
 
presvals <- extract(predictors, Especie_modela)

##  perfil bioclimatico 
head(presvals)

summary(presvals)

hist(presvals[,14] ,main="Perfil bioclimatico bio14", xlab="Precipitacion del mes mas seco", ylab="precencia")

## genera puntos aleatorios de ausencia
set.seed(0)
backgr <- randomPoints(predictors, 200)
colnames(backgr) = C("lon", "lat")
fix(backgr)
head(backgr)

## seracion de grupos de ajusnte y validacion
group <- kfold(backgr, 5)
backg_train <- backgr [group !=1,]
backg_test <- backgr [group ==1,]

##  extrae valore con puntos de ausencia
absvals <- extract(predictors, backgr)
##  varibles que indica si se trata de preceincia o punto de fonde
pb <- c(rep(1, nrow(presvals)),rep(0, nrow(absvals))) 

## creamos una matrix de prececi y ausencia
sdmdata <- data.frame(cbind(pb, rbind (presvals, absvals)))
head(sdmdata)
tail(sdmdata)
summary(sdmdata)
write.csv(sdmdata, file = "data ambp.csv")

## colinialide de variables
pairs(sdmdata [,2:6])

## entrenamiento con los datos de precencia
group <- kfold(Especie_modela, 4)
pres_train <- Especie_modela [group !=1,]
pres_test <- Especie_modela [group ==1,]

##  graficar puntos
r =raster(predictors,1)
plot(!is.na(r), col=c("white", "light grey"), legend= FALSE)
plot(wrld_simpl, and=TRUE, border= "dark grey")
plot(YbrevRange, and=TRUE, border= "red", lwd=2)
points(backg_train, pch= "_", cex=0.5, col= "red")
points(backg_test, pch= "_", cex=0.5, col= "black")
points(pres_train, pch= "+",  col= "green")
points(pres_test, pch= "+",  col= "blue")

bc <- bioclim(predictors, pres_train)
##  nicho bidimencional
plot (bc, a=1, b=3, p=0.85)
## evaluar modelo ausencia y precencia de fondo
e <- evaluate(pres_test,backg_test, bc, predictors)

e
## rster stack para variavle preictoras
pb <- predict(predictors, bc, ext=YbrevRange, progress="")
pb
## mostral ubral de clasificacion
tr <- threshold (e, "spec_sens")
density(e)
abline(v=tr)

##verificacion del mapa precencia y ausencia 
plot (pb, main= "Bioclim, raw values")
plot (wrld_simpl, add=TRUE, border= "dark grey")
plot (pb >-tr, main ="Bioblim_presence/absence")
plot (wrld_simpl, add=TRUE, border= "dark grey")
writeRaster (pb, "map.bil")

points (pres_train, pch="+")
xm <- maxent(predictors, pres_train)
