


source("packages-and-paths.r")
source("tidy-smolts-data.r")


M1<-"
model{

  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
      Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y]) # observed number of fish  

      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))

      #    etaStarB[i,y]<-((N[i,y]-s[i,y])*etaB)/((s[i,y]-1)*etaB+N[i,y]-1)

    }
  }
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(0.001,100000)
  
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
  
    alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN
    MN[1:nDays,y]<-log(muqN[1:nDays,y])-0.5/TauN[1:nDays,y]
    TauN[1:nDays,y]<-1/log((1/alphaN[1:nDays,y])+1)  
  }
  
  # Process for departure
  # ===========================
  for(y in 1:nYears){
    for(i in 1:nDays){
      # p: probability to start migration at day t, if haven't done so earlier
      # departure probability depends on temperature
      logit(p[i,y])<-P[i,y]
      P[i,y]~dnorm(aP+bP*Temp[i,y],1/pow(sdP,2))
    }
  }
  
  # Migration speed (in days to video site)
  # ==============
  # probability to be at video site in day j, if departing at day i
  for(y in 1:nYears){
    for(i in 1:nDays){ 
      # i: day of departure
      # j: day of passing the video site
      #j==i
      qD[i,i,y]<-phi((log(0.5)-MD[i,y])/SD)
  
      # j>i
      for(j in (i+1):(i+14)){ 
        qD[i,j,y]<-phi((log(j-i+0.5)-MD[i,y])/SD)-phi((log(j-i-0.5)-MD[i,y])/SD)
      }
  
      MD[i,y]<-log(muD[i,y])-0.5/TD
      muD[i,y]~dlnorm(log(exp(aD-bD*flow[i,y]))-0.5/taumuD, taumuD)
    }
  }
  cvD~dunif(0.001,2)
  taumuD<-1/log(cvmuD*cvmuD+1)
  TD<-1/log(cvD*cvD+1)
  SD<-1/sqrt(TD)
  
  cvmuD~dunif(0.001,2)
  
  aD~dlnorm(0.68,45) # mu=2,cv=0.15
  bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
  #aD~dlnorm(0.9,45) # mu=2.5,cv=0.2
  #bD~dlnorm(-4.6,25) # mu=0.03,cv=0.2
  
  # Proportion departing in each day  
  # ========================================
  for(y in 1:nYears){
    p2[1,y]<-p[1,y]
    for(i in 2:(nDays-1)){
      p2[i,y]<-(1-sum(p2[1:(i-1),y]))*p[i,y]
    }
    p2[nDays,y]<-1-sum(p2[1:60,y])
  
    # Joint distribution of qD and p2
    for(i in 1:nDays){ # day of departure
      for(j in i:(i+13)){ # day of passing the video site
        qtmp[j,i,y]<-qD[i,j,y]*p2[i,y]
      }
    }
  
    # Expected proportion of smolts passing the video site each day
    for(j in 1:14){
      muqN[j,y]<-sum(qtmp[j,1:j,y])+0.0001
    }
    for(j in 15:nDays){
      muqN[j,y]<-sum(qtmp[j,(j-13):j,y])+0.0001
    }
  }
  eta_alphaN~dunif(0.001,100000)
  
  aP~dnorm(-20,1) #mu=-20
  bP~dlnorm(0.6,10) #mu=1.91
  sdP~dlnorm(0,1) #mu=1.6
  #exp(0.6+0.5/10)
  #exp(0.5/1)
  
  # check sums (should be close to 1, otherwise fish is lost)
  for(i in 48:61){ # last 2 weeks of July 2006
    sums06[i]<-sum(qD[i,i:(i+13),4])
  }
  for(i in 48:61){ # last 2 weeks of July 2014
    sums14[i]<-sum(qD[i,i:(i+13),6])
  }

}"
cat(M1,file=paste(sep="", pathOut,"Smolts.txt"))


#schools<-read.table("input/Schools_03-06.txt", header=T)

data<-list(
  #s=schools,
  flow=datF,
  nDays=61,
  nYears=6,
  Nobs=S,                     
  Temp=datT
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model('Smolts.txt',inits=initials,
                           n.adapt=100,
                           data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sums06","sums14",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB",
                                    "etaB","aB","bB","sdBB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=200000, 
                                  thin=100)) 

system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sums06","sums14",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB",
                                    "etaB",  "aB","bB","sdBB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=200000, 
                                  thin=100))

chains<-combine.mcmc(list(chains1,chains2))
save(chains, file=paste(sep="", pathOut,"Smolts_cvDs_02_08_16.RData"))
