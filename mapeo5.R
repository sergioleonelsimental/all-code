dato <- read.csv(file.choose())
j1<-read.table('clipboard') ##carga depuntos

library(raster)##
library(rgdal)
library(dismo)##
library(rjava)
library(jsonlite)
library(sp)
library(maptools)##
library(ENMeval)##
library(virtualspecies)
library(removeCollinearity)
library(rgdal)
library(dismo)
library(rjava)
library(jsonlite)
library(sp)
library(maptools)

##solo coordenadas
Especie_modela<- j1[,1:2]
head (Especie_modela)
data(wrld_simpl)

plot(wrld_simpl)## descarga mapa

## elige zona
plot(wrld_simpl, xlim=c(-120,-70), ylim=c(15,32), axes=TRUE, col="red") ## elige zona
## contorno del mapa
box()

## mapear puntos
points(Especie_modela$long, Especie_modela$lat, col="20", pch=20, cex=2)

points(Especie_modela$log, Especie_modela$lat, col="blue", cex=2)

## bariables ambiemtales
BClim =getData("worldclim", var ="bio", res=0.5, lon=-100, lat=30)

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


projection(predictors)<- CRS("+proj=longlat +datum=WGS84")
raster.crop <- predictors



raster.crop.reduced <- removeCollinearity(raster.crop, multicollinearity.cutoff = 0.85,
                                         select.variables = TRUE, sample.points = FALSE, plot = TRUE, method = "spearman")
?removeCollinearity

raster.crop.reduced 
rasters.selected <- subset(raster.crop,c("bio1_22","bio2_22","bio3_22","bio10_22","bio11_22","bio7_22","bio8_22","bio16_22","bio14_22",
 "bio15_22","bio18_22","bio19_22"))

prevals <- raster::extract(rasters.selected, Especie_modela)

prevals

backgr <- randomPoints(rasters.selected,2000)
absvals<- raster::extract(rasters.selected, backgr)
pb <-c(rep(1,nrow(prevals)), rep(0, nrow(absvals)))
pb
sdmdata.present <- data.frame(cbind(pb, rbind (prevals, absvals)))
getwd()
setwd()
write.table(prevals, file = "F:/2021/POpulus_treumoidel/distribucuion y coliniaridad/Nueva carpeta/cuadriculas/variable.txt", quote =TRUE, sep ="\t", eol ="\n", na = "NA", dec
             =",", row.names = TRUE, qmethod = c("escape", "double"))

rasters.final <- subset(rasters.selected,c("bio1_22","bio2_22","bio3_22","bio10_22","bio11_22","bio7_22","bio8_22","bio16_22","bio14_22",
                                          "bio15_22","bio18_22","bio19_22"))

rasters.final<- omi

rasters.final<- na.omit(rasters.final) # listwise deletion of missing
model.maxent <- maxent(
  x=rasters.final,
  p=Especie_modela,
  a=backgr,
  args=c(
    ' randomtespoints=40',
    ' betamultipliet=1' ,
    ' linear=true' ,
    ' quadratic=true' ,
    ' product =true' ,
    ' threshold=true' ,
    ' hinge=true' ,
    ' threads=2',
    ' responsecurves= true' ,
    ' jackknife=true' ,
    ' askoverarite=false' 
    )
)

mydata <- rasters.final

mydata<- read.csv("de.csv", header=TRUE)
mydata<- na.omit(mydata) # listwise deletion of missing
require(caret)

mydata$pb[mydata$pb==1] <- "yes"
mydata$pb[mydata$pb==0] <- "no"

control <- trainControl(method="cv", number=5, classProbs= TRUE, summaryFunction=twoClassSummary)
set.seed(7)

fit <- train(pb ~ bio1_22+bio2_22+bio3_22+bio10_22+bio11_22+bio7_22+bio8_22+bio16_22+bio14_22+
+bio15_22+bio18_22+bio19_22, raster=mydata, method="rf", metric="Kappa", trControl=control)

print(fit)

model.maxent

plot(model.maxent)

map.fit <- predict(
  object=fit,
  x=rasters.crop,
  na.rm=TRUE,
  format='GTiff',
  filename= "F:/2021/POpulus_treumoidel/distribucuion y coliniaridad/Nueva carpeta/cuadriculas",
  overwrite=TRUE,
  progress='text'
)

plot(map.fit,main='Modela')

library(randomForest)
fit<- randomForest(presence ~ climate_anusplin_current_fday                +climate_anusplin_current_map+                climate_anusplin_current_pratio+           soils_bdricm_m_250m_ll+           soils_bldfie_m_sl5_250m_ll+                soils_crfvol_m_sl4_250m_ll+      soils_ocstha_m_sd1_250m_ll+  soils_phihox_m_sl1_250m_ll+                soils_sltppt_m_sl4_250m_ll+     soils_sndppt_m_sl2_250m_ll+   terrain_r90_curv,data=mydata)
importance(fit)


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








