if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("qvalue")


library("pcadapt")
library("qvalue")
library("OutFLANK")
library("ggplot2")
sel <- read.table("a qui tu tabla", head = TRUE)

sel <- read.csv(file.choose())

#eliminar colimnas
sel <- sel[ , -c(1 )] # quita la comuna x primeraEliminate columns

dim (sel)# visualisa tus datos

#Nota: PCAdapt espera que la matriz entrante tenga muestras en columnas y loci en filas. Debido a que nuestros datos son lo opuesto, necesitamos transponer nuestra matriz de datos con la t()función.
genotype <- sel[, 3:ncol(sel)]
dim(genotype)

# PCAdapt requires a pcadapt_class object. You can convert a matrix to 
# pcadapt_class with the read.pcadapt() function.
pca_genotype <- read.pcadapt(t(genotype))

K <-2
x <- pcadapt(pca_genotype, K = K)
plot(x, option = "screeplot") # 19 groups seems to be the correct value

#plot pca
plot(x, option = "scores", pop = sel[, 1]) # how populations are shared among the 19 groups


K <- 84
x <- pcadapt(pca_genotype, K = K, min.maf = 0.2)# de 0 --0.045 filtro

summary(x) # numerical quantities obtained after performing a PCA

plot(x, option = "manhattan")

plot(x, option = "qqplot", threshold = 0.1)

plot(x, option = "stat.distribution") # Distribution of Mahalanobis distances.

qval <- qvalue(x$pvalues)$qvalues #calcula qval
alpha <- 0.0001 #filtro de significacia de adativos
outliers_pcadapt <- which(qval < alpha)
print(outliers_pcadapt) #imprimir lalista

length(outliers_pcadapt) # 6366 outliers cunatos son
write.csv(outliers_pcadapt, file="28302_ma_tras.csv")#guarda la lista de adaptaivos
