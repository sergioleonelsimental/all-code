mydata <- read.csv("F:/2020/tesis/model/h.csv", header=TRUE)
require(caret)
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
require(lattice)
require(ggplot2)
require(randomForest)
require(quantregForest)
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
dim(mydata)
InTrain<-createDataPartition(y=mydata$clase, p=0.9999,list=FALSE)
training1<-mydata[InTrain,]
fit<-train(A77~ Fierro+H, data=training1,method="brnn",
           trControl=trainControl(method="cv", number=2),
           prox=TRUE,allowParallel=TRUE)

print(fit)
print(fit$finalModel)


