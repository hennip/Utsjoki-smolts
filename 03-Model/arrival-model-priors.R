#
# Remove data, keep covariates and fix abundance.
# Is the timing of the run reasonable a priori?



source("00-Functions/packages-and-paths.r")
source("01-Data/tidy-smolts-data.r")
load("02-Priors/priors-mvn.RData")

data<-list(
  #s=schools,
  ld_R=priors_mvn$R,ld_mumu=priors_mvn$mumu,ld_sdmu=priors_mvn$sdmu,
  flow=datF,
  nDays=61,
  nYears=6,
  #  Nobs=S,                     
  Temp=datT
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model('Smolts.txt',inits=initials,
                           n.adapt=500,
                           data=data,n.chains=2))

var_names<-c(
  "d", # =  "aD","bD","cvD","cvmuD",
  
  "sums06","sums14",
  
  "aP","bP","sdP",
  "etaB","aB","bB","sdBB",
  "eta_alphaN"
#  "Ntot","N"
)

system.time(
  chains1<-coda.samples(jm,variable.names=var_names,
                        n.iter=100000, thin=100)) 
system.time(
  chains2<-coda.samples(jm,variable.names=var_names,
                        n.iter=3000, thin=1))

chainsP<-combine.mcmc(list(chains1, chains2))

#summary(chainsP)
save(chainsP, file=paste(sep="", pathOut,"Smolts_17_09_priors.RData"))
 
     
     