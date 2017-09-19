# This is a small arrival model for complete arrival data to include
# school size as a determining factor to variation of observation process
# small set on known data, small (expected) observation process. 
# Two model versions for observation variance (overdispersion):
# 1) with minimally informative etaB, no school size (vrt 2002 paper)
# 2) with school size (and no etaB-component)
# What is the difference for evaluated uncertainty?

#source("00-Functions/packages-and-paths.r")


M1<-"
model{

  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
      
      # Observed number of fish
      # Nobs[i,y]~dbetabin(100,10,N[i,y])  
      #Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y])  
      Nobs[i,y]~dbetabin(muB[i,y]*etaStarB[i,y],(1-muB[i,y])*etaStarB[i,y],N[i,y])  
      
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))
      
#      etaStarB[i,y]<-((N[i,y]-s[i,y])*etaB)/((s[i,y]-1)*etaB+N[i,y]-1)
      etaStarB[i,y]<-(N[i,y]-s[i,y])/((s[i,y]-1)+N[i,y]-1)
      s[i,y]~dlnorm(log(muS[i,y])-0.5/TS,TS)
      muS[i,y]~dlnorm(log(mumuS[i,y])-0.5/TmuS,TmuS)
      mumuS[i,y]<-aS+bS*N[i,y]
    }
  }
  aB~dlnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(1,1000)


  #Based on 2008:2014 data
  aS~dlnorm(0.606,200)
  bS~dlnorm(-3.63,400)
  cvS~dlnorm(-1.482,3.6)
  cvmuS~dlnorm(-1.062,9.1)

  TmuS<-1/log(cvmuS*cvmuS+1)
  TS<-1/log(cvS*cvS+1)
  
  # Abundance
  # ==============
  for(y in 1:nYears){
    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y
    
    #N[1:nDays,y]~dmulti(qN[1:nDays,y],Ntot[y]) # daily true number of fish
    for(i in 1:(nDays-1)){
      N[i,y]<-round(qN[i,y]*Ntot[y])
    }
    N[nDays,y]<-round(Ntot[y]*(1-sum(qN[1:(nDays-1),y])))    
  }
  
  # Timig of the smolt run
  # i.e. how total number of smolts passing the video site
  # is distributed between 61 days
  # =============================================
  for(y in 1:nYears){
    # qN: daily proportion of smolts
    # dirichlet-distribution approximated with lognormal
    qN[1:nDays,y]<-zN[1:nDays,y]/sum(zN[1:nDays,y])
    
    for(i in 1:nDays){
      zN[i,y]~dlnorm(MN[i,y], TauN[i,y])
    }
    
    alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN+0.001
    MN[1:nDays,y]<-log(muqN[1:nDays,y])-0.5/TauN[1:nDays,y]
    TauN[1:nDays,y]<-1/log((1/alphaN[1:nDays,y])+1)  
    
    muqN[1:nDays,y]~ddirich(ones) # flat prior
  }
  
  eta_alphaN~dunif(0.001,100000)

}"
cat(M1,file="Schools.txt")

# full data; temp data missing for 2012 and partly for 2010, note that the
# run was late in 2009
#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2006,2008)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july
ones<-rep(1,n_days)


data<-list(
  s=df$Schools,
  flow=df$Flow,
  ones=ones,
  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model('Schools.txt',inits=initials,
                           n.adapt=5000,
                           data=data,n.chains=2))


var_names<-c(
#  "etaB",
  "etaStarB",
  "aB","bB","sdBB",
  "eta_alphaN",
  "Ntot","N"
)
#
system.time(
  chains0<-coda.samples(jm,variable.names=var_names,
                        n.iter=1000, thin=1)) #3.5h

system.time(
  chains1<-coda.samples(jm,variable.names=var_names,
                        n.iter=10000, thin=200)) #8h

system.time(
  chains2<-coda.samples(jm,variable.names=var_names,
                        n.iter=10000, thin=200))

chains<-combine.mcmc(list(chains1, chains2))
save(chains, file=paste(sep="", pathOut,"Schools_17_09_etaStar.RData"))

system.time(
  chains3<-coda.samples(jm,variable.names=var_names,
                        n.iter=400000, thin=200))

chains<-combine.mcmc(list(chains2, chains3))
save(chains, file=paste(sep="", pathOut,"Schools_17_09_etaStar.RData"))

system.time(
  chains4<-coda.samples(jm,variable.names=var_names,
                        n.iter=400000, thin=200))

chains<-combine.mcmc(list(chains2, chains3, chains4))
save(chains, file=paste(sep="", pathOut,"Schools_17_09_etaStar.RData"))
