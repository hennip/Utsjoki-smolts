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

# Pick sample on which sums>0.9
tmp<-c()
aD_samp<-c();bD_samp<-c()
cvD_samp<-c();cvmuD_samp<-c()
n<-1
for(i in 1:1000){
  if(chains1[,"sums"][[1]][i]>0.9){
    tmp[n]<-i
    aD_samp[n]<-chains1[,"aD"][[1]][i]
    bD_samp[n]<-chains1[,"bD"][[1]][i]
    cvD_samp[n]<-chains1[,"cvD"][[1]][i]
    cvmuD_samp[n]<-chains1[,"cvmuD"][[1]][i]
    n<-n+1
  }
}
aD_samp
length(tmp)

# Next: fit multinormal distribution for this sample 
# -> approximate distribution and see whether such could be used as a joint prior
# for d(aD, bD, cvD, cvmuD)
