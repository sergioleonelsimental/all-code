#42 pop y  snp-cargar aechivo de SNP
sel <- read.table("C:/Tesis/7000.txt", head = TRUE)

datos1 <- sel[ , -c(6 )]
datos2 <- datos1[ , -c(5 )]
datos3 <- datos2[ , -c(4 )]
datos4 <- datos3[ , -c(3 )]
datos5 <- datos4[ , -c(1 )]
write.table(iris_transpose2, file="genotype_TRANSPONER.txt")

dim(sel)
iris_transpose2 <- as.data.frame(t(as.matrix(genotype)))
iris_transpose2


write.csv(outliers_pcadapt, file="pr.csv")

#Note: PCAdapt expects the incoming matrix to have samples in 
#columns and loci in rows. Because our data is the opposite, we need to transpose our data matrix with the t() function.
genotype <- sel[, 3:ncol(sel)]
dim(genotype)


## PCAdapt requires a pcadapt_class object. You can convert a matrix to 
# pcadapt_class with the read.pcadapt() function
pca_genotype <- read.pcadapt(t(genotype))

K <- 2
x <- pcadapt(pca_genotype, K = K)

 
# 19 grupos parece ser el valor correcto
plot(x, option = "screeplot") 
plot(x, option = "scores", pop = sel[, 1])
ggplot2


# numerical quantities obtained after performing a PCA
summary(x)

plot(x, option = "manhattan")

plot(x, option = "qqplot", threshold = 0.1)

# Distribuci�n de las distancias de Mahalanobis.
plot(x, option = "stat.distribution") 

qval <- qvalue(x$pvalues)$qvalues
alpha <- 0.1
outliers_pcadapt <- which(qval < alpha)
print(outliers_pcadapt)

# 14 at�picos
length(outliers_pcadapt)

# uso de un umbral m�s estricto para detectar valores at�picos
alpha <- 0.05 
outliers <- which(qval < alpha)
print(outliers)

# 14 at�picos
length(outliers)

# vector con el nombre de poblaci�n
ind <- paste("pop", sel[, 1]) 

# vector con el nombre de loci
locinames <- as.character(seq(ncol(genotype))) 

FstDataFrame <- MakeDiploidFSTMat(genotype, locinames, ind)

## Calculating FSTs, may take a few minutes...
# Comprobando el efecto del tama�o de muestra en Fst ya que FSTCoCorr se usar� en el seguimiento
plot(FstDataFrame$FST, FstDataFrame$FSTNoCorr, xlim = c(-0.01,0.3), 
     ylim = c(-0.01, 0.3), pch = 20)
abline(0, 1)

hist(FstDataFrame$FSTNoCorr) 

OF <- OutFLANK(FstDataFrame, NumberOfSamples=19, qthreshold = 0.05, 
               RightTrimFraction = 0.05)

# Trazar la distribuci�n de Fst con la distribuci�n de chi cuadrado
OutFLANKResultsPlotter(OF, withOutliers = TRUE, NoCorr = TRUE, Hmin = 0.1, 
                       binwidth = 0.005, Zoom = FALSE, RightZoomFraction = 0.05, 
                       titletext = NULL)

outliers_OF <- OF$results$LocusName[OF$results$OutlierFlag == TRUE]
print(outliers_OF)

# 11 atipicos
length(outliers_OF)

#######################################
##Secci�n 4: Regresi�n log�stica: vinculaci�n de valores at�picos y temperatura