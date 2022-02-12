install.packages("file:///C:/Users/sergio/Desktop/2.0", repos = NULL, type = "source")
install.packages("curl")
install.packages("xml2")
install.packages("file:///C:/Users/sergio/Desktop/2.0", repos = NULL, type = "source")
install.packages("devtools")
source("https://bioconductor.org/biocLite.R")
browseVignettes("OutFLANK")
install.packages("file:///C:/Users/sergio/Desktop/2.0", repos = NULL, type = "source")
source("https://bioconductor.org/biocLite.R")
biocLite("SNPRelate")
a
install.packages("file:///C:/Users/sergio/Desktop/2.0", repos = NULL, type = "source")
install.packages("devtools")
library("devtools")
source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
a
browseVignettes("OutFLANK")
install_github("whitlock/OutFLANK")
install_github("bcm-uga/pcadapt")
library("pcadapt")
install.packages("C:/Users/sergio/Desktop/pcadapt_4.0.1.tar.gz", repos = NULL, type = "source")
library("pcadapt")
library("qvalue")
install.packages("C:/Users/sergio/Desktop/pcadapt_4.0.1.tar.gz", repos = NULL, type = "source")
library("pcadapt")
path_to_file <- "path_to_directory/foo.lfmm"
filename <- read.pcadapt(path_to_file, type = "lfmm")
sel <- read.table("C:/Tesis/prueba.txt", head = TRUE)
dim(sel)
genotype <- sel[, 3:ncol(sel)]
dim(genotype)
genotype_file <- tempfile()
write.table(x = t(genotype), # transposing the genotype matrix with t()
            file = genotype_file, 
            sep = " ", 
            col.names = FALSE, 
            row.names = FALSE)
K <- 25
x <- pcadapt(genotype_file, K = K)
install.packages("pcadapt")
library(pcadapt)
pcadapt for examples

