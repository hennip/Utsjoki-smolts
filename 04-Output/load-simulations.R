#######################
# Load jags simulations of arrival model

#load(file=paste(sep="",pathOut,"Smolts_cvDs_17_08_11.RData")) # cvmuD & cvD max 2
#load(file=paste(sep="",pathOut,"Smolts_aDbD_17_08_14.RData")) # cvmuD & cvD max 2; aD & bD vastaa Panun näkemystä 
#load(file=paste(sep="",pathOut,"Smolts_aDbD_17_08_30.RData")) # cvmuD max 1, aD priori korjattu
load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # standardoidut qD:t, 3vrk ajo ei merkkejä konvergenssista
#load(file=paste(sep="",pathOut,"Smolts_17_09_mvn.RData")) #  mvn matka-ajalle, ei konvergoi 3vrk ja summat vuotaa pahasti
load(file=paste(sep="",pathOut,"Schools_17_09.RData")) #  pieni dirich-malli

chains<-window(chains,start=400000, thin=200)

# Unupdated priors
#load(file=paste(sep="",pathOut,"Smolts_cvDs_17_08_14_priors.RData"))
#load(file=paste(sep="",pathOut,"Smolts_17_09_priors.RData"))
load(file=paste(sep="",pathOut,"Priors_Smolts_17_09.RData"))
load(file=paste(sep="",pathOut,"Priors_Smolts_17_09_fast.RData"))
