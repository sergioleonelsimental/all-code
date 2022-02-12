library(pcadapt)
install.packages("devtools")
library("devtools")
source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
install_github("whitlock/OutFLANK")
if (!("devtools" %in% installed.packages())){install.packages(devtools)}
library(devtools)
devtools::install_github("whitlock/OutFLANK")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("qvalue")
a
browseVignettes("qvalue")
library("pcadapt")
library("qvalue")
library("OutFLANK")
library("ggplot2")
sel <- read.table("data/SNPselection1.txt", head = TRUE)

se12<-read.table('clipboard')

K <- 25
x <- pcadapt(pca_genotype, K = K)
