
mydata <- read.csv("data_fin.csv", header=TRUE)
mydata <- na.omit(mydata)
library("party")
library("Hmisc")

##Fis_mac
set.seed(26)
readingSkills.cf <- cforest(SNP32 ~ log + lat + z + map + mat + gsp + mtcm + mmin + mtwm + mmax + sday + fday + ffp + dd5 +
                              gsdd5 + dd0 + d100 + mmindd0 + smrpb + smrsprpb + sprp + smrp + winp + AAI + 
                              PH__1_2_AG + pH_Buffer + C_E_ds_m + F__Ca_CO3 + F_Materia_ + F__SATURAC + 
                              Densidad_g + F_Arena + F__Limo + F__Arcilla + NITRATOS__ + FOSFORO_pp + 
                              FIERRO__Fe + MANGANESO + ZINC__Zn_ + COBRE__Cu_ + CALCIO__Ca + MAGNESIO__ + 
                              SODIO__Na_ + POTASIO__K + meq__CALCI + meq_MAGNES + meq_SODIO + meq_POTASI + 
                              CIC_meq__1 + F__CALCIO_ + F__MAGNESI + F__SODIO__ + F__POTASIO + F__OTRAS_B + 
                              F__HIDROGE + Curvatu_bi + Slope_bil1 + Aspect_bil + perfil + plan +
                              Curvatu__1 + Slope_bi_1 + Aspect_b_1, data=mydata, control = cforest_unbiased (mtry = 2, ntree = 100))
set.seed(26)
plot(varimp(readingSkills.cf))

varimp.rf1<-varimp(readingSkills.cf, conditional = FALSE) 
dotplot(sort((varimp.rf1/ sum(varimp.rf1))*100) ,cex=1.3,
        xlab=" Conditional variable importance (%) vs SNP 32")



  