library(randomForest)
library(caret)
library(ggplot2)
library(mlbench)
library(lattice)

training1 <-read.table("F:/2020/tesis/model/ari.csv", header=TRUE, sep="\t", dec=".")
datos <-read.table("F:/2020/tesis/model/A_ari.csv", header=TRUE, sep="\t", dec=".")

# Selección de una submuestra del 70% de los datos
set.seed(0.9999)

datos<-train(A209~ Fosforo, data=datos,method="rf",
                 trControl=trainControl(method="cv",number=5),
                 prox=TRUE,allowParallel=TRUE)

