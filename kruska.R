
mydatad<-read.table('clipboard')
mydata <- read.csv("E:/hhh1.csv", header=TRUE)
require(PMCMR)

data(mydata)
attach(mydata)
posthoc.kruskal.nemenyi.test(grupo~poly, dist="Tukey")

library(SamplingUtil)
kruskal.test(r ~ Grupos, data = mydatad )
kruskal.test(dg ~ Grupo, data = mydatad )


library(dplyr)

kruskal.test(poly ~ Grupo, data = mydatad )


