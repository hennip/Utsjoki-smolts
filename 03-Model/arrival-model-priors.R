#
# Remove data, keep covariates and fix abundance.
# Is the timing of the run reasonable a priori?



#source("00-Functions/packages-and-paths.r")

modelName<-"Smolts_simpleqD"
Mname<-str_c("03-Model/",modelName, ".txt")

# full data; temp data missing for 2012 and partly for 2010
#years<-c(2005:2009,2011,2013:2014) 
years<-c(2005:2006,2008,2014) # 4 years of data for testing  
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july

#load("02-Priors/priors-mvn.RData")

data<-list(
  #s=df$Schools,
  #  ld_covar=priors_mvn$Covar_d,
  #  ld_mu=priors_mvn$Mu_d,
  flow=df$Flow,
  #  Nobs=df$Smolts,                     
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years)
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model(Mname,inits=initials,
                           n.adapt=500,
                           data=data,n.chains=2))

var_names<-c(
  "aD","bD","cvD","cvmuD",
  
  "sums1","sums2",
  
  "aP","bP","sdP",
  "etaB","aB","bB","sdBB",
  "eta_alphaN"
  #  "Ntot","N"
)

system.time(chains1<-coda.samples(jm,variable.names=var_names,n.iter=100000, thin=100)) 
system.time(chains2<-coda.samples(jm,variable.names=var_names,n.iter=100000, thin=100))

chainsP<-combine.mcmc(list(chains1, chains2))

save(chainsP, file=str_c(pathOut,"Priors_",modelName,".RData"))

#summary(chainsP)
#save(chainsP, file=paste(sep="", pathOut,"Priors_Smolts_17_09.RData"))
#save(chainsP, file=paste(sep="", pathOut,"Priors_Smolts_17_09_fast.RData"))


