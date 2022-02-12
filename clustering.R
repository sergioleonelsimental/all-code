j6<-read.table('clipboard')
mydata <- read.csv("D:/Alex3.csv", header=TRUE)

j1 <- read.csv(file.choose())
mydata <- na.omit(j6) # listwise deletion of missing

mydata <- scale (mydata) # standardize variables

library(apcluster)

mydata.apclus <- apcluster(negDistMat(r=2), mydata, q=0.5)

cat("affinity propogation optimal number of clusters:", length(mydata.apclus@clusters), "\n")

# 4

heatmap(mydata.apclus, sideColors=c("darkgreen", "yellowgreen"), col=terrain.colors(16), Rowv=FALSE, dendScale=0.3)

