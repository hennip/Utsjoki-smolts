

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

etaStarB[i,y]<-(N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1

s[i,y]~dlnorm(log((K*N[i,y])/((K/slope)+N[i,y])+0.0001)-0.5/TS,TS)
}
}
# priors for observation process
aB~dnorm(2.9,60)
bB~dlnorm(-2.6,984)
sdBB~dlnorm(-0.23,210)
etaB~dunif(5,1000)

# priors for schooling
  K~dlnorm(6.07,0.7)
  slope~dlnorm(-1.94,66)
  cvS~dunif(0.001,2)
  TS<-1/log(cvS*cvS+1)

# Abundance
# ==============
for(y in 1:nYears){
Ntot[y]<-exp(LNtot[y])
LNtot[y]~dunif(7,15) # total run size in year y
#Ntot[y]<-20000

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
#P[i,y]~dnorm(aP+bP*Temp[i,y],1000)
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

for(j in i:(i+13)){
qD[i,j,y]<-qDx[i,j,y]/(sum(qDx[i,i:(i+13),y])+0.0001)
}

MD[i,y]<-log(muD[i,y])-0.5/TD
muD[i,y]~dlnorm(log(exp(aD-bD*flow[i,y]))-0.5/TmuD, TmuD)
}
}
SD<-1/sqrt(TD)
TmuD<-1/log(cvmuD*cvmuD+1)
TD<-1/log(cvD*cvD+1)

aD~dlnorm(0.52,14) # mu=1.75,cv=0.27
bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
cvmuD~dunif(0.001,1)
cvD~dunif(0.001,2)

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

# check sums (should be close to 1, otherwise fish is lost)
for(i in 48:61){ # last 2 weeks of July 2006
sums1[i]<-sum(qD[i,i:(i+13),1])
}
for(i in 48:61){ # last 2 weeks of July 2014
sums2[i]<-sum(qD[i,i:(i+13),2])
}

}"

#modelName<-"Smolts_fixedObsProp"
#modelName<-"Smolts_etaB_sdP"
modelName<-"Smolts_etaStarB_sdP"


Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

# Select years
#years<-c(2005:2006,2007,2008,2014) # 4 years of data plus simulated 2007  
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005:2009,2014) # 6 years to study
#years<-c(2005:2011,2013,2014) # 2012 temp data missing

n_days<-61
dat<-dat_all # all real data
#dat<-dat_all2 # 2007 simulated
#dat<-dat_all3 # 2007 first 17% missing, 2009 +- 2 days from the peak missing
#dat<-dat_all3 # 2007 first 17% missing, 2009 totally missing
dat<-dat_all3 # 2007 first 17% missing, 2014 +- 2 days from the peak missing
#dataName<-"all"
dataName<-"0714"
compName<-"turd010"

df<-smolts_data_to_jags(dat,years, n_days) # 61: only june & july

data<-list(
   s=df$Schools,
  flow=df$Flow,
  Nobs=df$Smolts,                     
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years)
)


inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
            list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))


var_names<-c(
  "aD","bD","cvD","cvmuD",
    "K","slope","cvS", "cvmuS",
  "sums1","sums2",
  "aP","bP","sdP",
#  "etaB",
  "aB","bB","sdBB",
  "eta_alphaN",
  #"Nobs",  
  "Ntot","N"
)


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!


t1<-Sys.time();t1
run1 <- run.jags(M1, 
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=300, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =10000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 7d
run<-run1
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#3.3d?
run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
run<-run4
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run5 <- extend.jags(run4, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
run<-run5
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))


