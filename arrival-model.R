


library(rjags)
library(runjags)
load.module("mix")

setwd("H:/Projects/ISAMA/prg/Utsjoki_smolts/")  # laptop

M1<-"
model{

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

# Observation process
# ====================
for(y in 1:nYears){
for(i in 1:nDays){ # 61 days in June-July
# Fixed probability to be seen
Nobs[i,y]~dbetabin(100,10,N[i,y]) # observed number of fish  
}
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

P[i,y]~dnorm(muP[i,y],1/pow(sdP,2))
muP[i,y]<-aP+bP*Temp[i,y]  # later: hier. structure for annual a and b
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

muD[i,y]~dlnorm(MmuD[i,y], 1/log(cvmuD*cvmuD+1))
MmuD[i,y]<-log(mumuD[i,y])-0.5*log(cvmuD*cvmuD+1)
mumuD[i,y]<-exp(aD-bD*flow[i,y])
}
}
#cvD<-0.1#~dunif(0.001,1)
cvD~dunif(0.001,1)
TD<-1/log(cvD*cvD+1)
SD<-1/sqrt(TD)

cvmuD~dunif(0.001,1)

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
#sdP<-1
sdP~dlnorm(0,1) #mu=1.6
#exp(0.6+0.5/10)
#exp(0.5/1)

# check sums (should be close to 1, otherwise fish is lost)
sum1<-sum(qD[1,1:14,4])
sum31<-sum(qD[31,31:44,4])

}"
cat(M1,file="model/Smolts.txt")

S<-read.table("input/Smolts_6years_2008weekendsOff.txt",header=T)

temp1<-read.table("input/Temperature_4years.txt",header=T)
source("model/UtsjokiWaterTemperature2005.r")
source("model/UtsjokiWaterTemperature2014.r")
temp<-cbind(temp1[,1:2],round(DailyTemp,1), temp1[,3],temp1[,4],round(DailyTemp14,1))

datF<-read.table("input/FlowPatoniva_03-14.txt", header=T)
summary(datF);dim(datF)
datF<-cbind(datF[,1:4], datF[,6], datF[,12])

schools<-read.table("input/Schools_03-06.txt", header=T)

data<-list(
  #s=schools,
  flow=datF,
  nDays=61,
  nYears=6,
  Nobs=S,                     
  Temp=temp
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model('model/Smolts.txt',inits=initials,
                           n.adapt=100,
                           data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sum1","sum31",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB","etaB",#  "aB","bB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=50000, 
                                  thin=100)) #5h

system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sum1","sum31",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB","etaB",#  "aB","bB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=150000, 
                                  thin=100)) #150k=22h

chains<-combine.mcmc(list(chains1,chains2))
save(chains, file="output/Smolts_17_04_26.RData")

system.time(chains3<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sum1","sum31",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB","etaB",#  "aB","bB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=750000, # 5x22h 
                                  thin=100)) #22h

system.time(chains4<-coda.samples(jm,
                                  variable.names=c(
                                    "cvD", "cvmuD",
                                    
                                    "sum1","sum31",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    #"muB","etaB",#  "aB","bB",
                                    
                                    "eta_alphaN",
                                    
                                    "Ntot","N"
                                    
                                  ),
                                  n.iter=450000, # 42h (toteuma 40h)
                                  thin=100)) 

chains<-combine.mcmc(list(chains2,chains3,chains4))
save(chains, file="output/Smolts_17_04_26.RData")

