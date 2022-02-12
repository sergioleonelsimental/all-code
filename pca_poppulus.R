library("pcadapt")
library("qvalue")
library("OutFLANK")
library("ggplot2")
sel <- read.table("data/SNPselection1.txt", head = TRUE)

sel <- read.csv(file.choose())

#eliminar colimnas
sel <- sel[ , -c(1 )] # Eliminate columns

sel[sel == -1] <- 9 # sustituir valves 

fix(sel)

dim (sel)

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
x <- pcadapt(pca_genotype, K = K, min.maf = 0)

summary(x) # numerical quantities obtained after performing a PCA

plot(x, option = "manhattan")

plot(x, option = "qqplot", threshold = 0.1)

plot(x, option = "stat.distribution") # Distribution of Mahalanobis distances.

qval <- qvalue(x$pvalues)$qvalues
alpha <- 0.01
outliers_pcadapt <- which(qval < alpha)
print(outliers_pcadapt)

length(outliers_pcadapt) # 6366 outliers
write.csv(iris_transpose, file="28302_ma_tras.csv")

alpha <- 0.05 # use of a more stringent threshold to detect outliers
outliers <- which(qval < alpha)
print(outliers)

length(outliers) #5473 outliers
hist(x$pvalues, xlab = "p-values", main = NULL, breaks = 50, col = "orange")

plot(x , option = "manhattan")

### OutFLANK

ind <- paste("pop", sel[, 1]) # vector with the name of population

locinames <- as.character(seq(ncol(genotype))) # vector with the name of loci

FstDataFrame <- MakeDiploidFSTMat(genotype, locinames, ind)
## Calculating FSTs, may take a few minutes...
plot(FstDataFrame$FST, FstDataFrame$FSTNoCorr, xlim = c(-0.01,0.3), 
     ylim = c(-0.01, 0.3), pch = 20)
abline(0, 1) # Checking the effect of sample size on Fst since FSTCoCorr will be used in the follow

hist(FstDataFrame$FSTNoCorr)

OF <- OutFLANK(FstDataFrame, NumberOfSamples=19, qthreshold = 0.05, 
               RightTrimFraction = 0.05)

# Plot the ditribution of Fst with the chi squared distribution
OutFLANKResultsPlotter(OF, withOutliers = TRUE, NoCorr = TRUE, Hmin = 0.1, 
                       binwidth = 0.005, Zoom = FALSE, RightZoomFraction = 0.05, 
                       titletext = NULL)

outliers_OF <- OF$results$LocusName[OF$results$OutlierFlag == TRUE]
print(outliers_OF)

length(outliers_OF) # 11 outliers

