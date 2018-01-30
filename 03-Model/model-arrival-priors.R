#
# Remove data, keep covariates and fix abundance.
# Is the timing of the run reasonable a priori?



#source("00-Functions/packages-and-paths.r")


#modelName<-"Smolts_standardqD_etaStarB"
#modelName<-"Smolts_standardqD_oldinits"
#modelName<-"Smolts_standardqD"
#modelName<-"Smolts_etaStarB_sdP"
modelName<-"Smolts_etaB"

Mname<-str_c("03-Model/",modelName, ".txt")

# Select years
#years<-c(2005:2006,2007,2008,2014) # 4 years of data plus simulated 2007  
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005:2009,2014) # 6 years to study
#years<-c(2005:2011,2013,2014) # 2012 temp data missing
dataName<-"0714"

n_days<-61
dat<-dat_all3 # 2007 first 17% missing, 2014 +- 2 days from the peak missing

df<-smolts_data_to_jags(dat,years, n_days) # 61: only june & july

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

inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

var_names<-c(
  "aD","bD","cvD","cvmuD",
#  "K","slope","cvS", "cvmuS",
  "sums1","sums2",
  
  "aP","bP","sdP",
  "etaB","aB","bB","sdBB",
  "eta_alphaN"
  #  "Ntot","N"
)

t1<-Sys.time()
run1 <- run.jags(Mname, 
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'rjparallel', thin=1, burnin =0, 
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)

run<-run1
save(run, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_run.RData"))

t1<-Sys.time()
run2 <- extend.jags(run1, combine=F, sample=2000, thin=1, keep.jags.files=F)
t2<-Sys.time()
difftime(t2,t1)

run<-run2
save(run, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_run.RData"))

run<-run2
summary(run, var="D")
summary(run, var="P")
summary(run, var="B")
summary(run, var="Ntot")
summary(run, var="eta_alphaN")
summary(run, var="sum")


plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="Ntot")
plot(run, var="eta_alphaN")

chainsP<-as.mcmc.list(run)
#chainsP<-window(chainsP,start=1000000)
save(chainsP, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_chains.RData"))
