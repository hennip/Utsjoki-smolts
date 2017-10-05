N<-c(0,1,2,100)
muS<-(K*N)/((K/slope)+N)+0.001;muS
esB<-(N-muS)/(muS-1+0.01)+1
esB

MX<-"
model{

  # Observation process
  # ====================
  for(y in 1:nYears){
  for(i in 1:nDays){ # 61 days in June-July
  
  Nobs[i,y]~dbetabin(muB[i,y]*etaStarB[i,y],(1-muB[i,y])*etaStarB[i,y],N[i,y]) # observed number of fish  
  
  muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
  BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))
  
  etaStarB[i,y]<-step((N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1)*((N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1)+
(1-step((N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1))*1

  s[i,y]~dlnorm(log(muS[i,y])-0.5/TS,TS)
  muS[i,y]~dlnorm(log((K*N[i,y])/((K/slope)+N[i,y])+0.0001)-0.5/TmuS,TmuS)
  }
  }

  TmuS<-1/log(cvmuS*cvmuS+1)
  TS<-1/log(cvS*cvS+1)
  
  
  # Abundance
  # ==============
  for(y in 1:nYears){
  Ntot[y]<-20000
#  LNtot[y]~dunif(7,15) # total run size in year y
  
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
  
  alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN#+0.001
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
  qDx[i,i,y]<-phi((log(0.5)-MD[i,y])/SD)
  
  # j>i
  for(j in (i+1):(i+13)){ #13 
  qDx[i,j,y]<-phi((log(j-i+0.5)-MD[i,y])/SD)-phi((log(j-i-0.5)-MD[i,y])/SD)
  }
  sumqDx[i,y]<-sum(qDx[i,i:(i+13),y])
  
  for(j in i:(i+13)){
  qD[i,j,y]<-qDx[i,j,y]/(sumqDx[i,y]+0.0001)
  }
  
  MD[i,y]<-log(muD[i,y])-0.5/TD
  muD[i,y]~dlnorm(log(exp(aD-bD*flow[i,y]))-0.5/TmuD, TmuD)
  }
  }
  SD<-1/sqrt(TD)
  TmuD<-1/log(cvmuD*cvmuD+1)
  TD<-1/log(cvD*cvD+1)
  
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

  # check sums (should be close to 1, otherwise fish is lost)
  for(i in 48:61){ # last 2 weeks of July 2006
  sums[i]<-sum(qD[i,i:(i+13),1])
  }

  aB<-2.9
  bB<-0.074
  sdBB<-0.8
  K<-16.8
  slope<-0.2
  cvS<-0.37
  cvmuS<-0.36
  
  aD<-2.4
  bD<-0.01
  cvmuD<-0.21
  cvD<-1.4
  
  eta_alphaN<-222
  
  aP<--20
  bP<-1.4
  sdP<-2.56
}"


modelName<-"Smolts_simulate"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(MX,file=Mname)


# full data; temp data missing for 2012 and partly for 2010
years<-c(2007)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july


data<-list(
  #s=df$Schools,
  flow=df$Flow,
  #Nobs=df$Smolts,                     
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years)
)



#initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
#               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))


system.time(jm<-jags.model(Mname,#inits=initials, 
                           n.adapt=100, data=data,n.chains=2))


var_names<-c(
  "sums",
  "N"
)

system.time(chains0<-coda.samples(jm,variable.names=var_names,n.iter=100, thin=1))/60#min per 1000 iter
#chains<-chains0
# [1] 5.62
a1<-Sys.time();a1
system.time(
  chains1<-coda.samples(jm,variable.names=var_names,n.iter=100000, thin=100))/3600
b1<-Sys.time() ; t1<-b1-a1; t1

