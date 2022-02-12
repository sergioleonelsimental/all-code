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

envchem<-read.table('clipboard')

envtopo<-read.table('clipboard')

###Forward selection for each data set 
  ##EnvChem selection

    rda1chem<-rda(spe.hel~1,envchem)###model with intercept only
    rda2chem<-rda(spe.hel~.,envchem)###model with all explanatory variables
    step.forward<-ordistep(rda1chem,scope=formula(rda2chem),
                       direction="forward",perm.max=200,pstep=999)
    
    rdasenvchem<-rda(spe.hel~oxy + dbo + nit + amm ,data=envchem)
    
    vif.cca(rdasenvchem)### colinearidade

  ##EnvTopo selection
    
    rda1topo<-rda(spe.hel~1,envtopo)###model with intercept only
    rda2topo<-rda(spe.hel~.,envtopo)###model with all explanatory variables
    step.forward<-ordistep(rda1topo,scope=formula(rda2topo),
                           direction="forward",perm.max=200,pstep=999)
    
    rdasenvtopo<-rda(spe.hel~alt,data=envtopo)
    
    vif.cca(rdasenvtopo)### colinearidade
    
###### Parsimonious subsets of explanatory variables (based on 
    # forward selections)
  
  names(envchem) ## selected: oxy + dbo + nit + amm 
  envchem.pars <- envchem[, c(4,5,6,7)]
    
  names(envtopo)##alt
  envtopo.pars <- envtopo[, c(1)]
    
# Variation partitioning
spe.part <- varpart(spe.hel, envchem.pars, envtopo.pars)
spe.part
plot(spe.part, digits=2)

# Tests of all testable fractions
# Test of fractions [a+b]
anova.cca(rda(spe.hel, envchem.pars), step=1000)
# Test of fractions [b+c]
anova.cca(rda(spe.hel, envtopo.pars), step=1000)
# Test of fractions [a+b+c]
env.pars <- cbind(envchem.pars, envtopo.pars)
anova.cca(rda(spe.hel, env.pars), step=1000)
# Test of fraction [a]
anova.cca(rda(spe.hel, envchem.pars, envtopo.pars), step=1000)
# Test of fraction [c]
anova.cca(rda(spe.hel, envtopo.pars, envchem.pars), step=1000)
