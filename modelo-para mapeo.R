mydata<- read.csv("de.csv", header=TRUE)
mydata<- na.omit(mydata) # listwise deletion of missing
require(caret)
mydata$presence[mydata$presence==1] <- "yes"
mydata$presence[mydata$presence==0] <- "no"
control <- trainControl(method="cv", number=5, classProbs= TRUE, summaryFunction=twoClassSummary)
set.seed(7)
fit <- train(presence ~ climate_anusplin_current_fday   +climate_anusplin_current_map+                climate_anusplin_current_pratio+           soils_bdricm_m_250m_ll+           soils_bldfie_m_sl5_250m_ll+                soils_crfvol_m_sl4_250m_ll+      soils_ocstha_m_sd1_250m_ll+  soils_phihox_m_sl1_250m_ll+                soils_sltppt_m_sl4_250m_ll+     soils_sndppt_m_sl2_250m_ll+   terrain_r90_curv, data=mydata, method="rf", metric="Kappa", trControl=control)
print(fit)
library(randomForest)
fit<- randomForest(presence ~ climate_anusplin_current_fday                +climate_anusplin_current_map+                climate_anusplin_current_pratio+           soils_bdricm_m_250m_ll+           soils_bldfie_m_sl5_250m_ll+                soils_crfvol_m_sl4_250m_ll+      soils_ocstha_m_sd1_250m_ll+  soils_phihox_m_sl1_250m_ll+                soils_sltppt_m_sl4_250m_ll+     soils_sndppt_m_sl2_250m_ll+   terrain_r90_curv,data=mydata)
importance(fit)
