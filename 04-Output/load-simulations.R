
source("00-Functions/packages-and-paths.r")

#######################
# Load jags simulations for arrival model

# sdP added again
load(file=str_c(pathOut,"Smolts_fixedObsProp_sdP_0714_run.RData")) 
chains2<-as.mcmc.list(run)

load(file=str_c(pathOut,"Smolts_etaB_sdP_0714_run.RData")) 
chains<-as.mcmc.list(run)
load(file=str_c(pathOut,"Priors_Smolts_etaB_sdP_0714_chains.RData")) 



# Whole time series, for Yann's model
load(file=str_c(pathOut,"Smolts_etaB_all_run.RData")) 


# 2005:2009, 2014, 07 & 14 predictions (NOTE! sdP is fixed!!)
load(file=str_c(pathOut,"Smolts_etaB_0714_chains.RData")) ; chains2<-chains
load(file=str_c(pathOut,"Priors_Smolts_etaB_0714_chains.RData")) 
#load(file=str_c(pathOut,"Smolts_etaB_0714_run.RData")) 
#chains<-as.mcmc.list(run)
# run time 6.2d, burnin 1000k

load(file=str_c(pathOut,"Smolts_fixedObsProp_0714_run.RData")) 
load(file=str_c(pathOut,"Smolts_fixedObsProp_0714_chains.RData")) 
chains2<-chains

# 2005:2009, 2014, 07 predictions and 09 totally missing
load(file=str_c(pathOut,"Smolts_etaB_09_run1.RData")) 
chains<-as.mcmc.list(run1)
chains<-window(chains,start=6000)


# 2005:2009, 2014, 07 & 09 predictions
load(file=str_c(pathOut,"Smolts_etaB_0709pred.RData")) 
chains<-window(chains,start=500000, thin=200)

load(file=str_c(pathOut,"Smolts_fixedObsProp_0709pred.RData")) 



# 2005-2014 (2012 puuttuu)
load(file=str_c(pathOut,"Smolts_etaB_allYears.RData")) 
chains<-window(chains,start=600000)
# Ajetaan vielä pitempään, Ntot:it konvergoi mutta cvD, aD on toivomisen varaa
# summien häntä (<25%) vuotaa, ei välttis ongelma, mutta hyvä seurata tilannetta


load(file=str_c(pathOut,"Smolts_etaB.RData")) 
chains<-window(chains,start=500000, thin=200)
load(file=str_c(pathOut,"Smolts_etaB_monitored.RData")) 


load(file=str_c(pathOut,"Smolts_fixedObsProp.RData")) 
chains2<-chains
# 4.12. tää ois niinku valmis.


load(file=str_c(pathOut,"Smolts_standardqD_etaStarB_oldinits.RData")) 
load(file=str_c(pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=500000, thin=200)
#chains<-window(chains,start=1, end=1500000, thin=200)



load(file=str_c(pathOut,"Smolts_standardqD_oldinits.RData"))
load(file=str_c(pathOut,"Priors_Smolts_standardqD_oldinits.RData")) 
chains<-window(chains,start=2000000, thin=200)
# ON KONVERGOINUT!!!
# ajoaika n. 19d
# seuraava ajo valmistuu 1.11.
# tässä etaB~U(5,1000), tsekkaa haittaako



load(file=str_c(pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t
load(file=str_c(pathOut,"Priors_Smolts_standardqD.RData")) 
chains<-window(chains,start=2500000, thin=200)
# Ajoaika 24d eikä siltikään kaikki konvergoi (aD, cvD, cvmuD ongelmalliset) -> kaadetaan

load(file=str_c(pathOut,"Smolts_standardqD_etaStarB.RData")) 
load(file=str_c(pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=2000000, thin=200)
# Ajoaika 22d, aD:n konvergenssi edelleen kyseenalainen
# tätä voisi testata "vanhoilla initseillä" jos jaksaisi- > auttaisiko?

load(file=str_c(pathOut,"Smolts_etaStarB_sdPfixed.RData")) 
# ei toimi.



#######################
# Load jags simulations for small school model

# Tarvitaan etaB malli dirichlet priorilla

# pelkkä etaB lognormal prioreilla 
load(file=str_c(pathOut,"Schools_etaB_indepN.RData")) #  
#3.5d

# dlnorm priors for N
load(file=str_c(pathOut,"Schools_etaStarB_indepN.RData")) #  
load(file=str_c(pathOut,"Priors_Schools_etaStarB_indepN.RData")) #  
# Ajoaika 4.5d

# dirich prior for Ntot
load(file=str_c(pathOut,"Schools_etaStarB.RData")) #  
load(file=str_c(pathOut,"Priors_Schools_etaStarB.RData")) #  
# Tämä vuotaa nollahavaintopäivinä
# 6.7d


# Prioritarkastelu
load(file=str_c(pathOut,"Priors_Schools_etaStarB_indepN.RData"))   
#load(file=str_c(pathOut,"Priors_Schools_etaStarB_indepN_ones2.RData"))   # tässä ei ole järkeä koska ei ole dirichlet

load(file=str_c(pathOut,"Priors_Schools_etaStarB.RData"))   
load(file=str_c(pathOut,"Priors_Schools_etaStarB_ones2.RData"))   

load(file=str_c(pathOut,"Priors_Schools_etaStarB_s.RData"))   
summary(chainsP)
summary(chainsP[,"Ntot[1]"])
