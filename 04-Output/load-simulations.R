#######################
# Load jags simulations for arrival model

load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # "simppelisti" standardoidut qD:t
load(file=paste(sep="",pathOut,"Priors_Smolts_17_09.RData"))
chains<-window(chains,start=1500000, thin=200)

load(file=paste(sep="",pathOut,"Smolts_simpleqD.RData")) # "simppelisti" standardoidut qD:t
load(file=paste(sep="",pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t

load(file=paste(sep="",pathOut,"Smolts_standardqD_cvmuD2.RData")) # standardoidut qD:t, piti olla cvmuD U(0,2), mutta on (0,1)
chains<-window(chains,start=500000, thin=200)

chains<-window(chains,start=1500000, thin=200)
chains<-window(chains,start=5000, thin=200)
#chains<-window(chains,start=1, end=500000, thin=200)


#######################
# Load jags simulations for small school model

load(file=paste(sep="",pathOut,"Schools.RData")) #  pieni dirich-malli (siistitty)
chains<-window(chains,start=2000000)

load(file=paste(sep="",pathOut,"Schools_etaStar.RData")) #  
load(file=paste(sep="",pathOut,"Priors_Schools_etaStar.RData")) #  
chains<-window(chains,start=15000000, thin=200)


