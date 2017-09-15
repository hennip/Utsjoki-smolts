#######################
# Load jags simulations of arrival model

#load(file=paste(sep="",pathOut,"Smolts_17_04_26.RData")) # vaikuttaa lupaavalta, konvergenssi 1vrk jälkeen?
#load(file=paste(sep="",pathOut,"Smolts_17_06_21.RData")) #
#load(file=paste(sep="",pathOut,"Smolts_cvDs_17_08_11.RData")) # cvmuD & cvD max 2
#load(file=paste(sep="",pathOut,"Smolts_aDbD_17_08_14.RData")) # cvmuD & cvD max 2; aD & bD vastaa Panun näkemystä 
#load(file=paste(sep="",pathOut,"Smolts_aDbD_17_08_30.RData")) # cvmuD max 1, aD priori korjattu
#load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # mvn matka-ajalle
load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # standardoidut qD:t
#load(file=paste(sep="",pathOut,"Smolts_17_09_tmp.RData")) # 

chains<-window(chains,start=10000, thin=200)

# Unupdated priors
#load(file=paste(sep="",pathOut,"Smolts_17_04_priors.RData"))
#load(file=paste(sep="",pathOut,"Smolts_17_06_21_priors.RData"))
#load(file=paste(sep="",pathOut,"Smolts_cvDs_17_08_14_priors.RData"))
load(file=paste(sep="",pathOut,"Smolts_17_09_priors.RData"))
