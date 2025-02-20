snps<-read.delim('Mexico012.vcf.012',header=F,row.names=1)
pos<-read.delim('Mexico012.vcf.012.pos',header=F)
indv<-read.delim('Mexico012.vcf.012.indv',header=F)
colnames(snps)<-paste(pos[,1],pos[,2],sep=':')
rownames(snps)<-indv[,1]
snps<-as.matrix(snps)
snps[snps == -1] <- NA
write.csv(snps, file="snpsna.csv")
library(snpReady)
M  <-  as.matrix ( snps ) 

rc  <-  raw.data ( M ,  frame = "wide" ,  base = FALSE ,  sweep.sample =  0.2 ,  
                   call.rate = 0.95 ,  maf = 0.025 ,  imput = FALSE , outfile = "012")
write.csv(rc[["M.clean"]], file="30932.csv")