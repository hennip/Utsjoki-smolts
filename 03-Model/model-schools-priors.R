
#modelName<-"Schools_test"

#modelName<-"Schools_etaStarB_indepN"

modelName<-"Schools_etaStarB"

Mname<-str_c("03-Model/",modelName, ".txt")

# full data; temp data missing for 2012 and partly for 2010, note that the
# run was late in 2009
#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005,2006,2008)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july
ones<-rep(1,n_days)


data<-list(
#  s=df$Schools,
  flow=df$Flow,
  ones=ones,
#  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)

initials<-list(list(LNtot=rep(14,data$nYears)),
               list(LNtot=rep(14,data$nYears)))
#initials<-list(list(logN=array(8.2,dim=c(data$nDays,data$nYears))),
#               list(logN=array(8.2,dim=c(data$nDays,data$nYears))))


system.time(jm<-jags.model(Mname,inits=initials,n.adapt=100000, data=data,n.chains=2))

var_names<-c(
   # "etaStarB",
  "etaB",
  "muB",
  "aB","bB","sdBB",#"s",
  "K", "slope",
  #"aS","bS",
  "cvS", "cvmuS"#,
#  "Ntot","N"
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

