M1<-"
model{

# Migration speed (in days to video site)
# ==============
# probability to be at video site in day j, if departing at day i
#i==1
    # 1: day of departure
    # j: day of passing the video site
    #j==i
    qD[1]<-phi((log(0.5)-MD)/SD)
    
    # j>i
    for(j in 2:14){ 
      qD[j]<-phi((log(j-1+0.5)-MD)/SD)-phi((log(j-1-0.5)-MD)/SD)
    }

MD<-log(muD)-0.5/TD
# Minimum flow: 10m3/s
muD~dlnorm(log(exp(aD-bD*10))-0.5/TmuD, TmuD)

aD~dlnorm(0.52,14) # mu=1.75,cv=0.27
bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2

cvmuD~dunif(0.001,1)
TmuD<-1/log(cvmuD*cvmuD+1)

cvD~dunif(0.001,2)
TD<-1/log(cvD*cvD+1)
SD<-1/sqrt(TD)

sums<-sum(qD[1:14])

}"


cat(M1,file="travel-time.txt")



system.time(jm<-jags.model('travel-time.txt',
                           n.adapt=100,n.chains=1))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "aD", "bD", "cvD", "cvmuD",
                                    "sums"
                                  ),
                                  n.iter=1000, 
                                  thin=1)) 
summary(chains1)
chains1

# Pick sample in which sums>0.9
tmp<-c()
aD_samp<-c();bD_samp<-c()
cvD_samp<-c();cvmuD_samp<-c()
n<-1
for(i in 1:1000){
  if(chains1[,"sums"][[1]][i]>0.95){
    tmp[n]<-i
    aD_samp[n]<-chains1[,"aD"][[1]][i]
    bD_samp[n]<-chains1[,"bD"][[1]][i]
    cvD_samp[n]<-chains1[,"cvD"][[1]][i]
    cvmuD_samp[n]<-chains1[,"cvmuD"][[1]][i]
    n<-n+1
  }
}
aD_samp
n_samp<-length(tmp)
dat_samp<-cbind(aD_samp,bD_samp, cvD_samp, cvmuD_samp)
logdat_samp<-log(dat_samp)
summary(dat_samp)
summary(logdat_samp)

# Next: fit multinormal distribution for this sample 
# -> approximate distribution and see whether such could be used as a joint prior
# for d(aD, bD, cvD, cvmuD)

M2<-
"model{
  
  for(i in 1:N){
    logdat[i,1:4]~dmnorm(mu[1:4], R[,]) 
  }
  
  for(i in 1:4){
    mu[i]~dunif(-10,10)
  }
  R[1:4,1:4]~dwish(omega[1:4,1:4],4)	
  covar[1:4,1:4]<-inverse(R[,])
}
"
cat(M2,file="travel-time2.txt")


omega<-array(c(0.01,0,0,0,
               0,0.01,0,0,
               0,0,0.01,0,
               0,0,0,0.01
               ), dim=c(4,4))

data<-list(
  N=n_samp,
  logdat=logdat_samp,
  omega=omega
)

system.time(jm<-jags.model('travel-time2.txt',data=data,
                           n.adapt=100,n.chains=2))


system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                   "mu", "R"
                                  ),
                                  n.iter=1000, 
                                  thin=1)) 
summary(chains2)$statistics[,4]/
summary(chains2)$statistics[,2]

summary(chains2)$statistics
traceplot(chains2)

# Check if approximation is good enough (sums roughly 1)

M3<-"
model{

# Migration speed (in days to video site)
# ==============
# probability to be at video site in day j, if departing at day i
#i==1
# 1: day of departure
# j: day of passing the video site
#j==i
qD[1]<-phi((log(0.5)-MD)/SD)

# j>i
for(j in 2:14){ 
qD[j]<-phi((log(j-1+0.5)-MD)/SD)-phi((log(j-1-0.5)-MD)/SD)
}
sums<-sum(qD[1:14])

MD<-log(muD)-0.5/TD
# Minimum flow: 10m3/s
muD~dlnorm(log(exp(d[1]-d[2]*10))-0.5/TmuD, TmuD)

TmuD<-1/log(d[4]*d[4]+1)

TD<-1/log(d[3]*d[3]+1)
SD<-1/sqrt(TD)

logd[1:4]~dmnorm(mu[1:4], R[,]) 

for(i in 1:4){
  d[i]<-exp(logd[i])
  # mu[i] logaritmiskaalalla
  taumu[i]<-1/(sdmu[i]*sdmu[i])
  mu[i]~dnorm(mumu[i],taumu[i])
}

}"


cat(M3,file="travel-time3.txt")

# Statistics from previous run
R<-array(c(round(summary(chains2)$statistics[,1], 3)[1:16]), dim=c(4,4))
mumu<-round(summary(chains2)$statistics[,1], 4)[17:20]
sdmu<-round(summary(chains2)$statistics[,2], 4)[17:20]

data<-list(R=R, mumu=mumu, sdmu=sdmu)

system.time(jm<-jags.model('travel-time3.txt', data=data,
                           n.adapt=100,n.chains=1))


system.time(chains3<-coda.samples(jm,
                                  variable.names=c(
                                    "sums"
                                  ),
                                  n.iter=10000, 
                                  thin=1)) 
summary(chains3)

# Save to 02-Priors
priors_mvn<-list(R, mumu, sdmu)
names(priors_mvn)<-c("R", "mumu", "sdmu")
save(priors_mvn,file="02-Priors/priors-mvn.RData")

