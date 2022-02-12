library(raster)##
library(rgdal)
library(dismo)##
library(rjava)
library(jsonlite)
library(sp)
library(maptools)##
library(ENMeval)##


cordinates<-read.table('clipboard')

total <- nrow(cordinates) ## contar

dups <- duplicated (cordinates [,1:2]) ## quitar duplicados

cordinates <- unique(cordinates) ## quitar duplicados

x <- cordinates [,1]  ## area de mapeo
y <- cordinates [,2]

xmin=min(x)-5
xmax=max(x)+5
ymin=min(y)-8
ymax=max(y)+4

## mapear 
maps::map("world", xlim=c(xmin,xmax), ylim=c(ymin,ymax),
          col="gray60", border="gray60", fill=TRUE, resolution=0)

box(which = "plot", lty = "soild", lwd=100)
axis(side=1,cex.axis=0.4,lwd = 0.25)
axis(side=2,cex.axis=0.4, lwd = 0.25)


## consultar puntos
data(wrld_simpl)
specie.pts <- SpatialPoints(cordinates, proj4string = CRS(proj4string(wrld_simpl)))
min.distance <- c()
pts.on.land <- 

box()
points(specie.pts@coords$long, specie.pts@coords$lat, col="20", pch=20, cex=2)


list.raster <- (
  list.file (
   "C/w"
   full.names =T,
   pattern =".asc"))
  
projection(predictors)<- CRS("+proj=longlat +datum=WGS84")

raster.crop.reduced <- removeCollinearity(raster.crop, multicollinearity.cutoff = 0.85,
                                          select.variables = TRUE, sample.points = FALSE, plot = TRUE)

raster.crop.reduced

rasters.selected <- subset(raster.crop,c("bio1_22","bio2_22","bio3_22","bio10_22","bio11_22","bio7_22","bio8_22","bio16_22","bio14_22",
                                         "bio15_22","bio18_22","bio19_22"))

prevals <- raster::extract(rasters.selected, coordinates)

prevals

set.seed(20)


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
raster.final


rasters.final
model.maxent <- maxent(
  x=rasters.final,
  p=Especie_modela,
  a=backgr,
  args=c(
    ' randomtespoints=30',
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
model. maxent()

plot(model.maxent)

map.model.maxent <- predict(
  object.model.maxent,
  x=rasters.crop,
  na.rm=TRUE,
  format='GTiff',
  filename= "F:/2021/POpulus_treumoidel/distribucuion y coliniaridad/Nueva carpeta/cuadriculas",
  overwrite=TRUE,
  progress='text'
)

plot(map.fit,main='Modela')

