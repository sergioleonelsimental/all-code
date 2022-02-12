###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
##VARIABLES TOPOGRAFICAS USANDO MODELO DE ELEVACION (OBSERVA LA RESOLUCION)
# Load libraries
require(pacman)
#Carga las librerias
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, sf, RColorBrewer, ggpubr)
#crea una lista
rm(list = ls())

# Functions to use
make_map <- function(col, clr, nme ){
  
  # col <- 'srtm_90'
  # clr <- terrain.colors(10)
  # nme <- 'Modelo de Elevación Digital'
  
  dfm <- vls %>% 
    dplyr::select(x, y, col) %>% 
    setNames(c('x', 'y', 'value'))
  
  gg <- ggplot() +
    geom_tile(data = dfm, aes(x = x, y = y, fill = value)) +
    geom_polygon(data = shp, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
    geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
    scale_fill_gradientn(colours = clr) +
    ggtitle(label = nme) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
          legend.key.width = unit(3, 'line')) +
    labs(x = 'Longitud', y = 'Latitude', fill = '') +
    coord_equal(xlim = extent(shp)[1:2], ylim = extent(shp)[3:4]) +
    guides(shape = guide_legend(override.aes = list(size = 3)))
  
  print("¡Hecho el mapa!")
  return(gg)
  
}
# Load data
shp <- shapefile('../shp/cali.shp')
mps <- shapefile('../shp/mpios.shp')
trn <- raster('../raster/srtm_90.tif')
trn <- trn * 1

# Extract by mask 
trn <- raster::crop(trn, shp) 
trn <- raster::mask(trn, shp)

# Project the shapefile 
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
trn.prj <- raster::projectRaster(trn, crs = prj, method = 'bilinear')

# Get the terrain variables
vrs <- raster::terrain(x = trn.prj, opt = c('slope', 'aspect', 'TPI', 'TRI'))
vrs <- raster::projectRaster(vrs, crs = '+proj=longlat +datum=WGS84 +no_defs')

msk <- vrs[[1]] * 0 
msk <- rasterToPolygons(msk)
msk <- aggregate(msk)

trn <- trn %>% raster::crop(., msk) %>% raster::mask(., msk)
vrs <- vrs %>% raster::crop(., msk) %>% raster::mask(., msk)
trn <- raster::resample(trn, vrs[[1]])

stk <- addLayer(trn, vrs)

# Raster to table
vls <- rasterToPoints(stk, spatial = FALSE)
vls <- as_tibble(vls)
vls <- drop_na(vls)
vls <- vls %>% dplyr::select(-tpi)
extent(trn)
extent(vrs)

# To make the map
names(vls)
col <- names(vls)[3:6]
clr <- list(
  terrain.colors(n = 10),
  RColorBrewer::brewer.pal(n = 8, name = 'BrBG'),
  RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd'),
  RColorBrewer::brewer.pal(n = 8, name = 'PuOr')
)
nme <- c("Modelo de Elevación Digital", "Índice de Rugosidad Topográfica", "Pendiente", "Aspecto")

maps <- purrr::pmap(.l = list(col, clr, nme), .f = make_map)
ggall <- ggpubr::ggarrange(maps[[1]], maps[[2]], maps[[3]], maps[[4]], ncol = 2, nrow = 2)
ggall

ggsave(plot = ggall, filename = '../png/mapa_terrain.png', units = 'in', 
       width = 15, height = 12, dpi = 300)

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
# DESCARGA VARIABLES DE CLIMA
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

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#SELECION DE VARIABLES (COLINIARIDAD SPERMAN Pearson (checa vif FDR OTRO METODOS MAS EFECTIVOS no incluidos en este codigo))
#Leer, escribir, manipular, analizar y modelar datos espaciales
library(raster)
#Enlaces a formatos ráster compatibles
library(rgdal)
#Manejo de metadatos
library(ncdf4)
#Quitar la coliniaridad
library(virtualspecies)
#No es nesesario
library(removeCollinearity) 
#Modelo
library(randomForest)
#Generar ausencias random del stck predictorio
library(dismo)
#Leer lista de raster
listaVar  <- list.files(full.name =T)
#Crear lista de raster
listaVar
#Crear raster stack
pcp_stk <- stack(listaVar)
#Remover la coliniaridad entre variables 
raster.crop.reduced <- removeCollinearity(pcp_stk, multicollinearity.cutoff = 0.8,
                                          select.variables = TRUE, sample.points = FALSE, plot = TRUE, method = "spearman")
#Ver selection de variables
raster.crop.reduced 
#Seleccionar las variables
variables.predictoras <- subset(pcp_stk,c("apect","BDRICM_M_250m_ll","bio7","bio10","bio12","bio14",
                                          "bio15","bio19","bio2","bio4","BLDFIE_M_sl1_250m_ll",
                                          "CECSOL_M_sl1_250m_ll","CLYPPT_M_sl1_250m_ll","CRFVOL_M_sl1_250m_ll"
                                          ,"ORCDRC_M_sl1_250m_ll","PHIHOX_M_sl1_250m_ll","slope","SLTPPT_M_sl1_250m_ll",
                                          "SNDPPT_M_sl1_250m_ll"))

rasters.final <- subset(variables.predictoras,c("apect","BDRICM_M_250m_ll","bio7","bio10","bio12","bio14",
                                                "bio15","bio19","bio2","bio4","BLDFIE_M_sl1_250m_ll",
                                                "CECSOL_M_sl1_250m_ll","CLYPPT_M_sl1_250m_ll","CRFVOL_M_sl1_250m_ll"
                                                ,"ORCDRC_M_sl1_250m_ll","PHIHOX_M_sl1_250m_ll","slope","SLTPPT_M_sl1_250m_ll",
                                                "SNDPPT_M_sl1_250m_ll"))

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#CARGAR AUSENCIAS Y PRESENCIAS
#Cargar presencias corta y pega de exel=clipboard
prese<-read.table('clipboard')
#Cargamos ausencias
ausencias<-read.table('clipboard')
#Solo usar cuando tiene una data de ausencias
backgr<-ausencias
#Si no tienes ausesias puedes gerara semi--ausencias de el stack
## genera puntos aleatorios de ausencia
set.seed(0)v 
backgr <- randomPoints(rasters.final, 200)
colnames(backgr) = C("lon", "lat")
#editar titulos
#Cambiar los titulos x,y a lon y lat
fix(backgr)
#observar
head(backgr)
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
library(raster)
library(rgdal)
library(rjava)
#Importamos los datos a un dataframe desde Github en un archivo json
library(jsonlite)
library(sp)
#mapa continentes
library(maptools)
# MAPEAR PRESENCIAS
data(wrld_simpl)
## grafica
plot(wrld_simpl)

## elige zona
plot(wrld_simpl, xlim=c(-120,-70), ylim=c(15,32), axes=TRUE, col="red") ## elige zona MEXICO
## contorno del mapa
box()

## mapear puntos
points(prese$lon, prese$lat, col="20", pch=20, cex=2)
#simbolo distinto

points(prese$log, prese$lat, col="blue", cex=2) 
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#EXTRACCION DE VALORES
## estracion de valores prececnia
presvals <- extract(rasters.final,prese[,1:2])
presvals1<-cbind(prese,presvals)
## estracion de ausencia
absvals <- extract(rasters.final, backgr)
absvals1<-cbind(backgr,absvals)

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
##VALIDACION DEL MODELO
#seracion de grupos de ajusnte y validacion
##entrenamiento con los datos de ausencia
group <- kfold(backgr, 5)
backg_train <- backgr [group !=1,]
backg_test <- backgr [group ==1,]
##Entrenamiento con los datos de precencia
group <- kfold(prese, 5)
pres_train <- prese [group !=1,]
pres_test <- prese [group ==1,]
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#CREAR MATRIX
pb <- c(rep(1, nrow(presvals1)),rep(0, nrow(absvals1))) 

## creamos una matrix de prececi y ausencia
sdmdata <- data.frame(cbind(pb, rbind ( presvals1, absvals1)))
head(sdmdata)
tail(sdmdata)
summary(sdmdata)
write.csv(sdmdata, file = "smata2003.csv")
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
## GRAFICAR PUNTOS EVALUAR EL MODELO
r =raster(rasters.final,1)
plot(!is.na(r), col=c("white", "light grey"), legend= FALSE)
plot(wrld_simpl, and=TRUE, border= "dark grey")
plot(pcp_stk, and=TRUE, border= "red", lwd=2)
points(backg_train, pch= "_", cex=0.5, col= "red")
points(backg_test, pch= "_", cex=0.5, col= "black")
points(pres_train, pch= "+",  col= "green")
points(pres_test, pch= "+",  col= "blue")

bc <- bioclim(rasters.final, pres_train)
##  nicho bidimencional
plot (bc, a=1, b=3, p=0.85)
## evaluar modelo ausencia y precencia de fondo
e <- evaluate(pres_test,backg_test, bc, rasters.final)

e
## rster stack para variavle preictoras
pb <- predict(rasters.final, bc, ext=pcp_stk, progress="")
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
writeRaster (pb, "map2002.tif")
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#Evaluar el modelo con los siguiente algoritmos:
#rf= Random forest
#lm= linear regression
#nnet= Neural Network
#avNNet= Model Averaged Neural Network 
#brnn= Bayesian Regularized Neural Networks
#rrf= Regularized random forest
#cv= Validacion cruzada

mydata <- rasters.final

# Load libraries ----------------------------------------------------------
#Modelos
library(caret)
#diseño, una gramática y estructuras de datos na
library(tidyverse)
library(raster)
# ller excel
library(readxl)
# para leer, manipular, analizar datos
require(hablar)
#contiene paletas de colores adicionales a las disponibles en r 
require(RColorBrewer)
#Leer datos espaciales
library(terra)
# Load data ---------------------------------------------------------------
fls <- list.files('../raster', full.names = TRUE, pattern = '.tif$')
stk <- raster::stack(fls)

brk <- raster::brick(rasters.final)
mydata <- read_excel("../tbl/data.xlsx")
mydata1<- read.csv(file.choose())
mydata <- mydata1[ , -c(1)]
mydata <- drop_na(mydata)

str(mydata)

mydata$pb[mydata$pb == 1] <- "yes"
mydata$pb[mydata$pb == 0] <- "no"

mydata <- mydata[,-c(2, 3)] # Eliminamos la longitud y la latitud

# Character to numeric ---------------------------------------------------
mydata <- cbind(mydata[,1], mydata[,2:ncol(mydata)] %>% mutate_if(is.character, as.numeric))
mydata <- as_tibble(mydata)
mydata <- drop_na(mydata)

control <- trainControl(method = "cv", 
                        number = 5, 
                        classProbs= TRUE, 
                        summaryFunction = twoClassSummary)

set.seed(5)
fix(mydata)
fit <- train(pb ~ apect + BDRICM_M_250m_ll + bio7 + bio10 + bio12 + bio14 + bio15 + bio19 + bio2 + bio4 + BLDFIE_M_sl1_250m_ll 
             + CECSOL_M_sl1_250m_ll + CLYPPT_M_sl1_250m_ll + CRFVOL_M_sl1_250m_ll + ORCDRC_M_sl1_250m_ll + PHIHOX_M_sl1_250m_ll + slope+SLTPPT_M_sl1_250m_ll +
               SNDPPT_M_sl1_250m_ll, data = mydata, method = "rf", metric = "Kappa", trControl = control)

# Select the variables from the stack -------------------
vars <- colnames(mydata)[2:length(colnames(mydata))]
psts <- grep(paste0(vars, collapse = '|'), names(brk), value = FALSE)
brk <- brk[[psts]]

print(fit)

# Change the names for the brick object -------------------
names(brk)
colnames(mydata)

rst <- raster::predict(object = brk, model = fit, type = 'prob', progress = 'text') # type por default, 0 y 1
rst=1-rst 
rst
plot(rst)
dir.create('../model/run_8', recursive = TRUE)
writeRaster(rst, '../model/run_8/8predict_rf_prob.tif', overwrite = TRUE)
plot(rst)
hist(rst)

trr <- rast(rst)
summary(fit)
fit$confusion

crd <- mydata1[,c(1, 2, 3)]
colnames(crd) <- c('pb', 'lon', 'lat')
crd
crd_prs <- crd %>% filter(pb == 'yes')
points(crd_prs$lon, crd_prs$lat, pch = 16, col = 'red')

# Extract the values from the raster result
crd_prs <- cbind(crd_prs, raster::extract(rst, crd_prs[,2:3]))
crd_prs <- as_tibble(crd_prs)
colnames(crd_prs)[4] <- 'suit'

hist(crd_prs$suit)

# To make the map ---------------------------------- 
rst_tbl <- rasterToPoints(x = rst, spatial = FALSE)
rst_tbl <- as_tibble(rst_tbl)

gmap <- ggplot() +
  geom_tile(data = rst_tbl, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'RdYlGn')) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(x = 1.5, units = 'line'))
plot(g)

################################################
#Exportar
write.csv(backgr, file="ausencias.csv")
write.csv(prese, file="presencias.csv")
write.csv(presvals, file="presenciasvalores.csv")
write.csv(absvals, file="auseciasvalores.csv")

############################################################

### Instalar librerías
install.packages("viridis")
install.packages("viridisLite")
install.packages("tidyverse")
install.packages("sf")
install.packages("rayshader")
install.packages("magick")
install.packages("av")

### Activar librerías
library(viridis)
library(viridisLite)
library(tidyverse)
library(sf)
library(rayshader)
library(magick)
library(av)
plot_gg(rst,multicore=TRUE,width=5,height=5,scale=300,windowsize=c(1280,720),
       zoom = 0.65, phi = 50)
render_snapshot()

### Crear un vídeo
Video <- file.path("D:/R/rayshader_ggplot/", "pop.mp4")
render_movie(filename = Video)
