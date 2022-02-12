mydata <- read.csv("F:/2020/tesis/model/h.csv", header=TRUE)

mydata <- read.csv(file.choose())
require(caret)
require(lattice)
require(ggplot2)
require(randomForest)
require(quantregForest)

##metod= rf,ml, nnet, avNNet, mlpWeightDecay,brnn
dim(mydata)
InTrain<-createDataPartition(y=mydata$clase, p=0.9999,list=FALSE)
training1<-mydata[InTrain,]
fit<-train(loci12~ AAi+gsp+smrp, data=training1,method="avNNet",
           trControl=trainControl(method="cv", number=5),
           prox=TRUE,allowParallel=TRUE)


print(fit)
print(fit$finalModel)