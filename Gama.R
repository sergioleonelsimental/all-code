
mydata <- read.csv("F:/2019/Tesis/regno lineal/herre.csv", header=TRUE)
mydata <- na.omit(mydata) # listwise deletion of missing
library(ggplot2)
ggplot(mydata, aes(Ca, a76, method = 'gam')) + geom_point() + geom_smooth()+ labs(x = "sprp (mm)", y ="Locus 416")
theme(text = element_text(size=20))
method = 'REML'


