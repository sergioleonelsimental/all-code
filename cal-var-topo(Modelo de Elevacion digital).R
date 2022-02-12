
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, sf, RColorBrewer, ggpubr)

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

