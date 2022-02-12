library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

tec <-read.table("F:/2019/Tesis/correlacion/r.studio/tec.txt", header=TRUE, sep="\t", dec=".")
tec

tec_cor <- cor(tec, method = "pearson")

write.csv(tec_cor, file="tecc.csv")
#round (cor(datos),4)
#rcorr(as.matrix(datos))
#correlacion<-round(cor(datos), 2)
corrplot(ari_cor, method="number", type="upper")
corrplot(ari_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)
chart.Correlation(ari_cor, histogram = F, pch = 19)

#corrplot(leyohycorr, method="number", type="upper")
#chart.Correlation(base, histogram = F, pch = 19)


#corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

#cor (datos)

#write.table(correlacion,file="ari.txt")
