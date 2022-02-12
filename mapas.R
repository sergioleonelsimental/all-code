install.packages('maptools')
install.packages('rgdal')
library(rgdal)
library(maptools)
library(ggplot2)

a1 <- choose.files()
a2 <- readOGR (a1)
class(a2)



mode(a2)
length(a2)
a2
str(a2)

ggplot(a2)

ggplot(a2) +
  geom_sf(aes(fill = TIPO))

plot(a2)
box()

View(a2@data)


dato <- read.csv(file.choose(), header=T, sep=";", dec=",", row.names=1)







dato <- read.csv(file.choose(), header=T, sep=";", dec=",", row.names=1)
