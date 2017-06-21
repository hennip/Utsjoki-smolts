#######################
# Load jags simulations of arrival model

#load(file=paste(sep="",pathOut,"Smolts_17_04_26.RData")) # vaikuttaa lupaavalta, konvergenssi 1vrk jälkeen?
load(file=paste(sep="",pathOut,"Smolts_17_06_19.RData"))

chains<-window(chains,start=20000, thin=1)

# Unupdated priors
#load(file=paste(sep="",pathOut,"Smolts_17_04_priors.RData"))
load(file=paste(sep="",pathOut,"Smolts_21_06_priors.RData"))
