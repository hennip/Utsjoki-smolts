#######################
# Load jags simulations for arrival model

#load(file=paste(sep="",pathOut,"Smolts_17_09.RData")) # "simppelisti" standardoidut qD:t
#load(file=paste(sep="",pathOut,"Priors_Smolts_17_09.RData"))
#chains<-window(chains,start=1500000, thin=200)

load(file=paste(sep="",pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD.RData")) 
chains<-window(chains,start=2500000, thin=200)
# konvergoi, mutta miksaantyminen huonoa 
# Ajoaika 18d
# seuraava ajo valmistuu 24.10.

load(file=paste(sep="",pathOut,"Smolts_standardqD_oldinits.RData"))
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD_oldinits.RData")) 
chains<-window(chains,start=2000000, thin=200)
# ON KONVERGOINUT!!!
# ajoaika 14d
# seuraava ajo valmistuu 24.10.
# tässä etaB~U(5,1000), tsekkaa haittaako

load(file=paste(sep="",pathOut,"Smolts_standardqD_etaStarB.RData")) 
load(file=paste(sep="",pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=1000000, thin=200)
# tsekkaa konvergointidiagnostiikka
# Ajoaika 16d
# seuraavan ajon pitäisi valmistua 24.10.

#load(file=paste(sep="",pathOut,"Smolts_standardqD_etaStarB_test.RData"))
#chains<-window(chains,start=100000, thin=200)
# ei likelläkään konvergointia
# Ajoaika 8d
# Tässä ajatus oli sallia korkeammat ylärajat cvD:lle ja cvmuD:lle -> ei hyvä idea


load(file=paste(sep="",pathOut,"Smolts_etaStarB_Dmvn.RData"))# standardoidut qD:t
chains<-window(chains,start=800000, thin=200)
#Ntot1 ongelmallinen, samoin sdP
# Ajoaika 6d


#######################
# Load jags simulations for small school model

#load(file=paste(sep="",pathOut,"Schools_17_09.RData")) #  pieni dirich-malli (siistitty)
load(file=paste(sep="",pathOut,"Schools_etaStar.RData")) #  pieni dirich-malli (siistitty)
chains<-window(chains,start=5000000)
load(file=paste(sep="",pathOut,"Schools.RData")) #  pieni dirich-malli (siistitty)
chains<-window(chains,start=2000000)

load(file=paste(sep="",pathOut,"Schools_etaStarB_2.RData")) #  
load(file=paste(sep="",pathOut,"Priors_Schools_etaStar.RData")) #  
chains<-window(chains,start=15000000, thin=200)
# konvergointi näyttää hyvältä, cvS ja cvmuS hakkaa ylärajaan
# Tässä mallissa oli vuoto-ongelmia, onko edelleen?
# Ajoaika 37h

# 2005 mukana (viimein), odotettavissa ajon loppu 23/24 10.
load(file=paste(sep="",pathOut,"Schools_etaStarB.RData")) #  

