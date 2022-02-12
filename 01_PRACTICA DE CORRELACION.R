##IMPORTAR BASE DE DATOS
datos <-read.table("F:/2018/DICAF/DR. PABLITO/trabajo/especies.txt", header=TRUE, sep="\t", dec=".") 

pca_genotype <- datos(t(datos1))
pca_genotype <- read.table(t(datos))

cor(datos$Especie,datos$Altitud)
#Obtener la grafica de dispersion 
plot(datos)

cor(datos)
#Obtener los coeficientes de correlacion entre las variables

library(psych)

pairs.panels(ccuatroespecies)

cor(datos$G,datos$NDVI)

help("aov")
