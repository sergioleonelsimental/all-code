library(vcfR)
library(adegenet)
library(poppr)
vcf<-read.vcfR("Mexico2_mmiss85_maf0.05_minDP10.recode.vcf")
vcf.genind <- vcfR2genind(vcf)
rm(vcf)

genclone <- as.genclone(vcf.genind)
genind2genalex( vcf.genind, filename = "ldp.csv",)
write.csv(vcf.genind, file="pruehhva1.csv")
mydata <- read.genalex("ldp.csv")

data<-read.table("ldPop.txt", sep = "\t", header = T)
pop(vcf.genind) <- data$Pop
table(strata(vcf.genind, ~pop/Subpop, combine = FALSE))  # Subpopulations
amova(vcf.genind~pop/Subpop, )
vcf.genind


table(strata(ser ~pop))  # Populations

pair.ia(vcf.genind)

poppr.amova(leo,)

nei.dist(ser)

genind2genalex(ser, file = "mi.csv")

ser <-read.genalex("mi.csv")
ser

data("Aeut")
Aeut
strata(vcff.genind) <- data.frame(other(vcff.genind)$population_hierarchy)

leo <- table(strata(vcff.genind, ~Pop))  # Populations
leo<-table(strata(ser, ~Pop, combine = FALSE))  # Subpopulations


# add x and y
Coord <- popp %>% dplyr::select(Pop,Subpop)
colnames(Coord)<- c("Pop","Subpop")
vcf.genind@other$PopSubpop <- as.list(Coord)

