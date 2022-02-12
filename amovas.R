library(vcfR)
library(adegenet)
library(poppr)
vcf<-read.vcfR("1177a.recode.vcf")
vcf.genind <- vcfR2genind(vcf)
rm(vcf)

genclone <- as.genclone(vcf.genind)
genind2genalex( vcf.genind, filename = "prueba.csv",)
write.csv(vcf.genind, file="prueva1.csv")


data<-read.table("pop.txt", sep = "\t", header = T)
pop(vcf.genind) <- data$Pop
table(strata(vcf.genind, ~pop/Subpop, combine = FALSE))  # Subpopulations
amova(vcf.genind~pop/Subpop, )
vcf.genind


table(pop(vcf.genind, ~pop))  # Populations

pair.ia(vcf.genind)
