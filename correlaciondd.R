
dato <- read.csv(file.choose())

popcor <- cor(mydata, method = "pearson")

write.csv(res[["po"]], file="tecfdgc2.csv")

library(fuzzySim)

corSelect(data = mydata, cor.thresh = 0.4, method = "pearson")

de <- corSelect(data = mydata, cor.thresh = 0.4, method = "pearson")

mydata <- na.omit(dato)


write.csv(de, file="lista00.csv")
corrplot(popcor, method = "ellipse")
library(corrplot)

multicol(mydata)
