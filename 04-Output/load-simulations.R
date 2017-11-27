
source("00-Functions/packages-and-paths.r")

#######################
# Load jags simulations for arrival model

load(file=paste(sep="",pathOut,"Smolts_standardqD_etaStarB_oldinits.RData")) 
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=500000, thin=200)
#chains<-window(chains,start=1, end=1500000, thin=200)
# 3d ajo ja konv ergenssi (so it seems)! Hämmästyttävää nopeutta. 
#Ehdottomasti käytössä jatkossa "vanhat initsit"


load(file=paste(sep="",pathOut,"Smolts_standardqD_oldinits.RData"))
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD_oldinits.RData")) 
chains<-window(chains,start=2000000, thin=200)
# ON KONVERGOINUT!!!
# ajoaika n. 19d
# seuraava ajo valmistuu 1.11.
# tässä etaB~U(5,1000), tsekkaa haittaako



load(file=paste(sep="",pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD.RData")) 
chains<-window(chains,start=2500000, thin=200)
# Ajoaika 24d eikä siltikään kaikki konvergoi (aD, cvD, cvmuD ongelmalliset) -> kaadetaan

load(file=paste(sep="",pathOut,"Smolts_standardqD_etaStarB.RData")) 
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=2000000, thin=200)
# Ajoaika 22d, aD:n konvergenssi edelleen kyseenalainen
# tätä voisi testata "vanhoilla initseillä" jos jaksaisi- > auttaisiko?


#######################
# Load jags simulations for small school model

# Tarvitaan etaB malli dirichlet priorilla

# pelkkä etaB lognormal prioreilla 
load(file=paste(sep="",pathOut,"Schools_etaB_indepN.RData")) #  
#3.5d

# dlnorm priors for N
load(file=paste(sep="",pathOut,"Schools_etaStarB_indepN.RData")) #  
load(file=paste(sep="",pathOut,"Priors_Schools_etaStarB_indepN.RData")) #  
# Ajoaika 4.5d

# dirich prior for Ntot
load(file=paste(sep="",pathOut,"Schools_etaStarB.RData")) #  
load(file=paste(sep="",pathOut,"Priors_Schools_etaStarB.RData")) #  
# Tämä vuotaa nollahavaintopäivinä
# 6.7d

