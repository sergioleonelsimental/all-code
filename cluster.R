mydata <- read.csv("D:/Alex3.csv", header=TRUE)
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale (mydata) # standardize variables
library(apcluster)
mydata.apclus <- apcluster(negDistMat(r=2), mydata, q=0.5)
cat("affinity propogation optimal number of clusters:", length(mydata.apclus@clusters), "\n")
# 4
heatmap(mydata.apclus, sideColors=c("darkgreen", "yellowgreen"), col=terrain.colors(16), Rowv=FALSE, dendScale=0.3)
heatmap(mydata.apclus, sideColors=rainbow(length(mydata.apclus)), Rowv=FALSE,  cexRow=(0.2 +           1/log10(nrow(mydata.apclus @sim))), cexCol=(0.2+1/log10(nrow(mydata.apclus @sim)))
        
        
        -- 
          