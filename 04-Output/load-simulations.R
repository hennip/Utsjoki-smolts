
source("00-Functions/packages-and-paths.r")

#######################
# Load jags simulations for arrival model

# sdP added again
load(str_c(pathOut,"Priors_Smolts_etaStarB_sdP_0714_run_ould017.RData")) 
chainsP<-as.mcmc.list(run)

# here school size ==0.001 when Nobs==0, extend.jags does not work
load(str_c(pathOut,"Smolts_etaStarB_s_0714_run_turd010.RData")) 
#load(str_c(pathOut,"Smolts_etaStarB_sdP_0714_run.RData")) # school size =1 when Nobs=0
chains<-as.mcmc.list(run)
chains<-window(chains,start=500000)
# TÄSTÄ TÄYTYY AJAA VASTAAVA PRIORI!!


####
#### Nämä tässä ovat smolttipaperissa olevat versiot. sdP:lle annetaan priori niinkuin kuuluu

# Continued run, etaB monitored
load(str_c(pathOut,"Smolts_etaB_sdP_extra_0714_run_turd010.RData"))
chains<-as.mcmc.list(run)


load(str_c(pathOut,"Smolts_fixedObsProp_sdP_0714_run.RData")) 
chains2<-as.mcmc.list(run)
chains2<-window(chains2,start=50000)

load(str_c(pathOut,"Smolts_etaB_sdP_0714_run.RData")) 
chains<-as.mcmc.list(run)
#ModelName<-"etaB_sdP_0714"

#chains2<-chains
load(str_c(pathOut,"Priors_Smolts_etaB_sdP_0714_chains.RData")) 

####
####


# Whole time series (for Yann's model etc)
load(str_c(pathOut,"Smolts_etaB_covs_all_run_turd010.RData")) 
chains<-as.mcmc.list(run)
chains<-window(chains,start=100000)
ModelName<-"Smolts_etaB_covs_all"
load(str_c(pathOut,"Priors_Smolts_etaB_sdP_0714_chains.RData")) 


# Tästä alaspäin voisi melkeimpä deletoida.

# 2005:2009, 2014, 07 & 14 predictions (NOTE! sdP is fixed!!)
load(str_c(pathOut,"Smolts_etaB_0714_chains.RData")) ; chains2<-chains
load(str_c(pathOut,"Priors_Smolts_etaB_0714_chains.RData")) 
#load(str_c(pathOut,"Smolts_etaB_0714_run.RData")) 
#chains<-as.mcmc.list(run)
# run time 6.2d, burnin 1000k

load(str_c(pathOut,"Smolts_fixedObsProp_0714_run.RData")) 
load(str_c(pathOut,"Smolts_fixedObsProp_0714_chains.RData")) 
chains2<-chains

# 2005:2009, 2014, 07 predictions and 09 totally missing
load(str_c(pathOut,"Smolts_etaB_09_run1.RData")) 
chains<-as.mcmc.list(run1)
chains<-window(chains,start=6000)


# 2005:2009, 2014, 07 & 09 predictions
load(str_c(pathOut,"Smolts_etaB_0709pred.RData")) 
chains<-window(chains,start=500000, thin=200)

load(str_c(pathOut,"Smolts_fixedObsProp_0709pred.RData")) 



# 2005-2014 (2012 puuttuu)
load(str_c(pathOut,"Smolts_etaB_allYears.RData")) 
chains<-window(chains,start=600000)
# Ajetaan vielä pitempään, Ntot:it konvergoi mutta cvD, aD on toivomisen varaa
# summien häntä (<25%) vuotaa, ei välttis ongelma, mutta hyvä seurata tilannetta


load(str_c(pathOut,"Smolts_etaB.RData")) 
chains<-window(chains,start=500000, thin=200)
load(str_c(pathOut,"Smolts_etaB_monitored.RData")) 


load(str_c(pathOut,"Smolts_fixedObsProp.RData")) 
chains2<-chains
# 4.12. tää ois niinku valmis.


load(str_c(pathOut,"Smolts_standardqD_etaStarB_oldinits.RData")) 
load(str_c(pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=500000, thin=200)
#chains<-window(chains,start=1, end=1500000, thin=200)



load(str_c(pathOut,"Smolts_standardqD_oldinits.RData"))
load(str_c(pathOut,"Priors_Smolts_standardqD_oldinits.RData")) 
chains<-window(chains,start=2000000, thin=200)
# ON KONVERGOINUT!!!
# ajoaika n. 19d
# seuraava ajo valmistuu 1.11.
# tässä etaB~U(5,1000), tsekkaa haittaako



load(str_c(pathOut,"Smolts_standardqD.RData")) # standardoidut qD:t
load(str_c(pathOut,"Priors_Smolts_standardqD.RData")) 
chains<-window(chains,start=2500000, thin=200)
# Ajoaika 24d eikä siltikään kaikki konvergoi (aD, cvD, cvmuD ongelmalliset) -> kaadetaan

load(str_c(pathOut,"Smolts_standardqD_etaStarB.RData")) 
load(str_c(pathOut,"Priors_Smolts_standardqD_etaStarB.RData")) #  PRIORS
chains<-window(chains,start=2000000, thin=200)
# Ajoaika 22d, aD:n konvergenssi edelleen kyseenalainen
# tätä voisi testata "vanhoilla initseillä" jos jaksaisi- > auttaisiko?

load(str_c(pathOut,"Smolts_etaStarB_sdPfixed.RData")) 
# ei toimi.



#######################
# Load jags simulations for small school model


# Näissä s=1 kun Nobs=0
load(str_c(pathOut,"Schools_etaB_dirich_050608_run.RData")) #  
chains2<-as.mcmc.list(run)
load(str_c(pathOut,"Schools_etaStarB_dirich_050608_run.RData")) #  
chains<-as.mcmc.list(run)


# pelkkä etaB lognormal prioreilla 
load(str_c(pathOut,"Schools_etaB_indepN.RData")) #  
#3.5d

# dlnorm priors for N
load(str_c(pathOut,"Schools_etaStarB_indepN.RData")) #  
load(str_c(pathOut,"Priors_Schools_etaStarB_indepN.RData")) #  
# Ajoaika 4.5d

# dirich prior for Ntot
load(str_c(pathOut,"Schools_etaStarB.RData")) #  
load(str_c(pathOut,"Priors_Schools_etaStarB.RData")) #  
# Tämä vuotaa nollahavaintopäivinä
# 6.7d


# Prioritarkastelu
load(str_c(pathOut,"Priors_Schools_etaStarB_indepN.RData"))   
#load(str_c(pathOut,"Priors_Schools_etaStarB_indepN_ones2.RData"))   # tässä ei ole järkeä koska ei ole dirichlet

load(str_c(pathOut,"Priors_Schools_etaStarB.RData"))   
load(str_c(pathOut,"Priors_Schools_etaStarB_ones2.RData"))   

load(str_c(pathOut,"Priors_Schools_etaStarB_s.RData"))   
summary(chainsP)
summary(chainsP[,"Ntot[1]"])
