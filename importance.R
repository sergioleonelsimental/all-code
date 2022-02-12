library(randomForest)
library(caret)
library(ggplot2)
library(mlbench)
library(lattice)
library(party)
library(zoo)
library(tidyverse)

trainer1<-read.table("F:/2020/importance/dur.txt", stringsAsFactors=TRUE)

summary(trainer1)
attach(trainer1)

# Selección de trainer submuestra del 70% de los datos
set.seed(0.9999)


output.forest <- randomForest (SDM ~ bio2 + bio3 + bio4 + bio7 + bio9 + bio13 + bio15 + bio16 + 
                                 bio17 + bio18 + bio19  + geology + bldfie + cecsol + clyppt + crfvol + phihox + 
                                 sltppt + sndppt + slope + wetind, data = trainer1)
print (output.forest)
randomForest::importance(output.forest)
plot (output.forest)
table(trainer1$SDM)/nrow(trainer1)
write.table(randomForest, file="prueva1.txt")
