#######################
# Load jags simulations of arrival model

#load(file=paste(sep="",pathOut,"Smolts_17_04_26.RData")) # vaikuttaa lupaavalta, konvergenssi 1vrk jälkeen?
#load(file=paste(sep="",pathOut,"Smolts_17_06_21.RData")) #
load(file=paste(sep="",pathOut,"Smolts_17_08_02.RData")) # wrong model
load(file=paste(sep="",pathOut,"Smolts_cvDs_17_08_02.RData")) # wrong model

chains<-window(chains,start=50000, thin=1)

# Unupdated priors
#load(file=paste(sep="",pathOut,"Smolts_17_04_priors.RData"))
load(file=paste(sep="",pathOut,"Smolts_17_06_21_priors.RData"))
load(file=paste(sep="",pathOut,"Smolts_17_08_02_priors.RData"))
