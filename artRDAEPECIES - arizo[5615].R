library(vegan)


#####....Borcard, D., Gillet, F., Legendre, P., 2011. Numerical Ecology with R,
#####....Media, Use R! Springer New York
#####....free data available at: http://adn.biol.umontreal.ca/~numericalecology/numecolR/ ###
#####.........Species data set: "DoubsSpe"
#####.........Environmental variables data set: "DoubsEnv"


# Variation partitioning with two sets of explanatory variables
# *************************************************************


# Import the data 

spe<-read.table('clipboard')
spe.hel <- decostand(spe, "hellinger") ## Hellinger-transform the species dataset

envsue<-read.table('clipboard') #x1

envcli<-read.table('clipboard') #x2

envgeo<-read.table('clipboard') #x3

###Forward selection for each data set 
  ##Envsuelos selection

    rda1sue<-rda(spe.hel~1,envsue)###model with intercept only
    rda2sue<-rda(spe.hel~.,envsue)###model with all explanatory variables
    step.forward<-ordistep(rda1sue,scope=formula(rda2sue),
                       direction="forward",perm.max=200,pstep=999)
    
    rdasenvsue<-rda(spe.hel~ Fosforo ,data=envsue)
    
    vif.cca(rdasenvsue)### colinearidade

  ##Envclima selection
    
    rda1cli<-rda(spe.hel~1,envcli)###model with intercept only
    rda2cli<-rda(spe.hel~.,envcli)###model with all explanatory variables
    step.forward<-ordistep(rda1cli,scope=formula(rda2cli),
                           direction="forward",perm.max=200,pstep=999)
    
    rdasenvcli<-rda(spe.hel~ mat + sday + smrpb +AAI + mtcm ,data=envcli)
    
    vif.cca(rdasenvcli)### colinearidade
    
    ##Envgeograficas selection
    
    rda1geo<-rda(spe.hel~1,envgeo)###model with intercept only
    rda2geo<-rda(spe.hel~.,envgeo)###model with all explanatory variables
    step.forward<-ordistep(rda1geo,scope=formula(rda2geo),
                           direction="forward",perm.max=200,pstep=999)
    
    rdasenvcli<-rda(spe.hel~ ,data=envcli)
    
    vif.cca(rdasenvcli)### colinearidade
    
###### Parsimonious subsets of explanatory variables (based on 
    # forward selections)
  
  names(envsue) ## selected: oxy + dbo + nit + amm 
  envsue.pars <- envsue[, c(3)]
    
  names(envcli)##alt
  envcli.pars <- envcli[, c(8,12,17)]
    
# Variation partitioning
spe.part <- varpart(spe.hel, envsue.pars, envcli.pars)
spe.part
plot(spe.part, digits=2)

# Tests of all testable fractions
# Test of fractions [a+b]
anova.cca(rda(spe.hel, envsue.pars), step=1000)
# Test of fractions [b+c]
anova.cca(rda(spe.hel, envcli.pars), step=1000)
# Test of fractions [a+b+c]
env.pars <- cbind(envsue.pars, envcli.pars)
anova.cca(rda(spe.hel, env.pars), step=1000)
# Test of fraction [a]
anova.cca(rda(spe.hel, envsue.pars, envcli.pars), step=1000)
# Test of fraction [c]
anova.cca(rda(spe.hel, envcli.pars, envsue.pars), step=1000)
