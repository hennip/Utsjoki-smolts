#
# Remove data, keep covariates and fix abundance.
# Is the timing of the run reasonable a priori?






library(rjags)
library(runjags)
load.module("mix")

setwd("H:/Projects/ISAMA/prg/Utsjoki_smolts/")  # laptop

M1<-"
model{

# Abundance
# ==============
for(y in 1:nYears){
  Ntot[y]<-100 # fixed annual abundance

  for(i in 1:nDays){
    N[i,y]<-qN[i,y]*Ntot[y] # Daily abundances
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

cat(M1,file="model/Smolts_priors.txt")

Nobs<-read.table("input/Smolts_6years_2008weekendsOff.txt",header=T)

temp1<-read.table("input/Temperature_4years.txt",header=T)
source("model/UtsjokiWaterTemperature2005.r")
source("model/UtsjokiWaterTemperature2014.r")
temp<-cbind(temp1[,1:2],round(DailyTemp,1), temp1[,3],round(DailyTemp14,1),temp1[,4])

datF<-read.table("input/FlowPatoniva_03-14.txt", header=T)
summary(datF);dim(datF)
datF<-cbind(datF[,1:4], datF[,6], datF[,12])

#schools<-read.table("input/Schools_03-06.txt", header=T)

data<-list(
  #s=schools,
  flow=datF,
  nDays=61,
  nYears=6,
#  Nobs=S,                     
  Temp=temp
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

system.time(jm<-jags.model('model/Smolts_priors.txt',#inits=initials,
                           n.adapt=100,
                           data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
variable.names=c(
  "N",
  "sum1", "sum31",
  "cvD", "cvmuD",
    
  "aP","bP","sdP",
  "aD","bD",
  
  "eta_alphaN"
  
),
n.iter=10000, 
thin=10))

system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "N",
                                    "sum1", "sum31",
                                    "cvD", "cvmuD",
                                    
                                    "aP","bP","sdP",
                                    "aD","bD",
                                    
                                    "eta_alphaN"
                                    
                                  ),
                                  n.iter=10000, 
                                  thin=10))


#chainsP<-chains1

chainsP<-combine.mcmc(list(chains1,chains2))
save(chainsP, file="output/Smolts_17_04_priors.RData")

chains<-chainsP

source("functions/bx.r")


# Temp vs. prob to start migration
######################################
Temp<-seq(1,20, by=1)
nT<-length(Temp)
n_samp<-length(chains[,"aP"][[1]])
aP_samp<-chains[,"aP"][[1]]
bP_samp<-chains[,"bP"][[1]]
sdP_samp<-chains[,"sdP"][[1]]

muP_samp<-array(NA, dim=c(nT,n_samp))
P_samp<-array(NA, dim=c(nT,n_samp))
p_samp<-array(NA, dim=c(n_samp,nT))
for(j in 1:n_samp){
  for(i in 1:nT){
    muP_samp[i,j]<-aP_samp[j]+bP_samp[j]*Temp[i]
    P_samp[i,j]<-rnorm(1,muP_samp[i,j],sdP_samp[j])
    p_samp[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))
  }
}
#p_samp
colnames(p_samp)<-paste(sep="", "p[",1:20,"]")

steps<-length(Temp) 
#windows()
par(mfrow=c(1,1))
bx2_y(p_samp,1,steps,"p[","]",ylim=c(0,1),Temp,ylab="Probability","Temperature (degrees celsius)",
      cex.lab=1.2)

#n<-0
#for(i in 1:20){
#  for(j in 1:n_samp){
#    if(is.na(p_samp[j,i])==T){print(c(j,i));n<-n+1}
#  }
#}

#################################
# Travel time to video vs flow
Flow<-seq(10,150, by=10)
nF<-length(Flow)

n_samp<-length(chains[,"aD"][[1]])
aD_samp<-chains[,"aD"][[1]]
bD_samp<-chains[,"bD"][[1]]
cvmuD_samp<-chains[,"cvmuD"][[1]]

muD_samp<-array(NA, dim=c(n_samp,nF))
mumuD_samp<-array(NA, dim=c(n_samp,nF))
sdmuD_samp<-array(NA, dim=c(n_samp,nF))
#cvmuD<-0.1
for(j in 1:n_samp){
  for(i in 1:nF){
    mumuD_samp[j,i]<-exp(aD_samp[j]-bD_samp[j]*Flow[i])
    sdmuD_samp[j,i]<-mumuD_samp[j,i]*cvmuD_samp[j]
    muD_samp[j,i]<-rnorm(1,mumuD_samp[j,i], sdmuD_samp[j,i])
  }
}
#muD_samp
#colnames(muD_samp)<-colnames(d)
colnames(muD_samp)<-paste(sep="", "muD[",1:15,"]")

#windows()
par(mfrow=c(1,1))
steps<-nF 
bx2_y(muD_samp,1,steps,"muD[","]",ylim=c(0,17),Flow,"Flow",ylab="Expected travel time",
      main="Travel time to video site, prior", cex.lab=1.2)


# Predicted timing of migration
################################################
d<-as.matrix(chains)       # transform to a matrix for boxplotting
#colnames(d)
year<-  c(2003:2006,2008,2014)

showData<-0
steps<-61 # number of steps to model (years)
par(mfrow=c(2,3))
par(mar=c(3,5,3,1)+0.1)

for(i in 1:6){
bx2_y(d,1,steps,"N[",paste(sep="",",",i,"]"),ylim=c(0,30),
      xlabseq=c(1:61),xlabname="", ylab=ifelse(i==1|i==4,"Proportion (%)",""),
      main=year[i], cex.lab=1.2, cex.main=1.5)
if(showData==1){points(1:61,Nobs[,i], pch=17, col="blue")}
lines(data$flow[,i]/5, col="blue", lwd=2)
lines(data$Temp[,i], col="red", lwd=2)
if(i==1){
  legend("topleft", c("Temperature", "Flow"), col=c("red", "blue"), lty=1,lwd=2, cex=1.5)
}
}

windows()
plot(data$flow[,3], type="l", ylab="Flow (m3)", xlab="Day (in June-July)", 
     main="2005",cex.main=1.5, cex.lab=1.2, lwd=2)
abline(v=24)

plot(1:61,Nobs[,3], pch=17, type="b", col="blue", xlab="Day (in June-July)", main="2005", 
     ylab="Number of smolts", cex.lab=1.2, cex.main=1.5)
abline(v=24)
cbind(1:61,Nobs[,3])

