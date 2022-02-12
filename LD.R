d3 <-  read.genalex(file.choose())
library(poppr)
library(adegenet)
library("magrittr")

D3 <-ia(d2)
d4<-pair.ia(d3) 

hist(d4)

d4<-pair.ia(d3)

res <- pair.ia(d3, sample = 999)

ia(res, sample = 999)

plot(p4, low = "black", high = "green", index = "Ia")
arizonica <- popsub(d2, "Pinus arizonica")
ia(arizonica, sample = 999)

herrerae <- popsub(d3, "Pinus herrerae")
ia(herrerae, sample = 999)

kruskal.test()

durangensis <- popsub(dur, "Pinus durangensis")
ia(durangensis, sample = 999)

help(poppr)

mydata <- read.csv("E:/....csv", header=TRUE)
require(PMCMR)
data(mydata)
attach(mydata)
posthoc.kruskal.nemenyi.test(AFLP1+AFLP2 +....+AFLP373~G, data=mydata, dist="Tukey")
kruskal.test(AFLP373~G, data=mydata)

