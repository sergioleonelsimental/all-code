install.packages("devtools")
library(devtools)
library(usethis)
install.packages("snpReady")
library(snpReady)
library(Matrix)
library(matrixcalc)
library(stringr)
library(rgl)

library(impute)

if (! requireNamespace ("BiocManager", silenciosamente = VERDADERO))
  install.packages ("BiocManager")

BiocManager :: install ("impute")
a
source("https://cran.rstudio.com/src/contrib/devtools_2.0.2.tar.gz")
install.packages("processx")
install.packages("devtools", dependencies=TRUE)
install.packages('devtools',dependencies=TRUE, repos='https://stat.ethz.ch/CRAN/')
source("https://bioconductor.org/biocLite.R")
biocLite("devtools")
a
BiocManager::install("devtools")
install.packages ( " devtools " , type  =  " win.binary " )
inf.norm()



M  <-  as.matrix ( POPSNP ) 

rc  <-  raw.data ( M ,  frame = "wide" ,  base = FALSE ,  sweep.sample =  0.5 ,  
                   call.rate = 0.95 ,  maf = 0.05 ,  imput = FALSE , outfile = "012")
