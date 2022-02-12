##CONVERTIR 012 EN VCFTOOLS (NA=-1)
vcftools --vcf my.file.vcf --012 --out output_geno.vcf

##NOMBRAR COLUMNAS Y FILAS
snps<-read.delim('Mexico012.vcf.012',header=F,row.names=1)
pos<-read.delim('Mexico012.vcf.012.pos',header=F)
indv<-read.delim('Mexico012.vcf.012.indv',header=F)
colnames(snps)<-paste(pos[,1],pos[,2],sep=':')
rownames(snps)<-indv[,1]
snps<-as.matrix(snps)

###En caso de querer trasponer la matriz
snps.convert<-t(snps)
fix(snps)
spe<-read.table('clipboard')
library(vegan)
spe[is.na(spe)] <- -1
spe[spe == -1] <- NA
spe1 <- as.data.frame(t(as.matrix(spe)))
