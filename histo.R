D=d4[,2]/length(d4[,2])

hist(d4[,2], freq = T, col="orange", main="", xlab="", cex.lab =1.2, ylab="", cex.lab =1.2, breaks = 100, axes=T)  
mtext("count", side = 2, line = 2.5, cex = 1.5, font = 1)
mtext("rd", side = 1, line = 2.5, cex = 1.5, font = 1)
axis(side=1, at=seq(-0.1, 1, by=0.2))
axis(side=2, at=seq(0, 1, by=2))
max(d4[,2])
min(d4[,2])
