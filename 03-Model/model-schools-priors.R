
#modelName<-"Schools_test"

modelName<-"Schools_etaStarB_indepN"

#modelName<-"Schools_etaStarB"

Mname<-str_c("03-Model/",modelName, ".txt")

# full data; temp data missing for 2012 and partly for 2010, note that the
# run was late in 2009
#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005,2006,2008)
n_days<-61
dat<-dat_all # all real data
#dat<-dat_all2 # 2007 simulated
df<-smolts_data_to_jags(dat,years, n_days) # 61: only june & july

ones<-rep(1,n_days)
#ones<-rep(1/n_days,n_days)


data<-list(
  s=df$Schools,
  flow=df$Flow,
  ones=ones,
#  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)

#initials<-list(list(LNtot=rep(14,data$nYears)),
#               list(LNtot=rep(14,data$nYears)))
initials<-list(list(logN=array(8.2,dim=c(data$nDays,data$nYears))),
               list(logN=array(8.2,dim=c(data$nDays,data$nYears))))


system.time(jm<-jags.model(Mname,inits=initials,n.adapt=100000, data=data,n.chains=2))

var_names<-c(
  "Ntot",#"N",
  "qN",
   # "etaStarB",
  "etaB",
  "aB","bB","sdBB",
  "K", "slope",
  "cvS", "cvmuS"
)
#
#system.time(chains0<-coda.samples(jm,variable.names=var_names,n.iter=1000, thin=1))

a1<-Sys.time();a1
chains1<-coda.samples(jm,variable.names=var_names,n.iter=1000000, thin=10000) 
b1<-Sys.time() ; t1<-b1-a1; t1
#summary(chains1)

a2<-Sys.time();a2
chains2<-coda.samples(jm,variable.names=var_names,n.iter=1000000, thin=10000)
b2<-Sys.time() ; t2<-b2-a2; t2


chainsP<-combine.mcmc(list(chains1, chains2))

save(chainsP, file=str_c(pathOut,"Priors_",modelName,".RData"))

gelman.diag(chainsP[,"aB"])
gelman.diag(chainsP[,"bB"])
gelman.diag(chainsP[,"etaB"])
gelman.diag(chainsP[,"sdBB"])
gelman.diag(chainsP[,"K"])
gelman.diag(chainsP[,"slope"])
gelman.diag(chainsP[,"cvS"])
gelman.diag(chainsP[,"cvmuS"])
gelman.diag(chainsP[,"s[1,1]"])
gelman.diag(chainsP[,"s[2,1]"])
gelman.diag(chainsP[,"s[3,1]"])
gelman.diag(chainsP[,"s[4,1]"])
gelman.diag(chainsP[,"s[5,1]"])
gelman.diag(chainsP[,"s[6,1]"])
gelman.diag(chainsP[,"s[7,1]"])
gelman.diag(chainsP[,"s[8,1]"])
gelman.diag(chainsP[,"s[9,1]"])
gelman.diag(chainsP[,"s[10,1]"])


traceplot(chainsP[,"s[9,1]"])
