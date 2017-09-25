#######################
# Load jags simulations of arrival model

load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # "simppelisti" standardoidut qD:t
load(file=paste(sep="",pathOut,"Smolts_17_09_fast.RData")) # nopeutettu odotettu matka-aika
load(file=paste(sep="",pathOut,"Smolts_simpleqD.RData")) # "simppelisti" standardoidut qD:t
load(file=paste(sep="",pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t

chains<-window(chains,start=1500000, thin=200)
chains<-window(chains,start=200000, thin=200)
#chains<-window(chains,start=1, end=500000, thin=200)

load(file=paste(sep="",pathOut,"Schools_17_09.RData")) #  pieni dirich-malli
load(file=paste(sep="",pathOut,"Schools_17_09_etaStar.RData")) #  pieni dirich-malli

load(file=paste(sep="",pathOut,"Schools.RData")) #  pieni dirich-malli (siistitty)
chains<-window(chains,start=2000000)


# Unupdated priors
load(file=paste(sep="",pathOut,"Priors_Smolts_17_09.RData"))
load(file=paste(sep="",pathOut,"Priors_Smolts_17_09_fast.RData"))
