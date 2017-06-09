# Need to have faster travel time to video site compared to
#aD~dlnorm(0.9,45) # mu=2.5,cv=0.2
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2



Flow<-seq(10,150, by=10)
nF<-length(Flow)
M2<-"
model{
for(i in 1:n){
      muD[i]~dnorm(mumuD[i], taumuD[i])
      mumuD[i]<-exp(aD-bD*Flow[i])
      sdmuD[i]<-mumuD[i]*cvmuD
      taumuD[i]<-1/pow(sdmuD[i],2)
}
cvmuD<-0.1

#Priors:
muaD<-2
cvaD<-0.15
mubD<-0.01
cvbD<-0.2
#aD~dlnorm(log(muaD)-0.5*log(cvaD*cvaD+1),1/log(cvaD*cvaD+1)) 
#bD~dlnorm(log(mubD)-0.5*log(cvbD*cvbD+1),1/log(cvbD*cvbD+1)) 
aD~dlnorm(0.68,45) # mu=2,cv=0.15
bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2


# These are ok, but travel time could be shorter
#aD~dlnorm(0.9,45) # mu=2.5,cv=0.15
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
#exp(0.9+0.5/45)
#sqrt(exp(1/45)-1)
#exp(-4.6+0.5/25)
#sqrt(exp(1/25)-1)

}"

cat(M2,file="model/priori2.txt")

data<-list(Flow=Flow, n=length(Flow))

system.time(jm<-jags.model('model/priori2.txt',
                           n.adapt=100,data=data,n.chains=2))

system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "muD"
                                  ),
                                  n.iter=5000,
                                  thin=1))
chainsP2<-chains1
summary(chainsP2)

source("functions/bx.r")
d<-as.matrix(chainsP2)       # transform to a matrix for boxplotting
colnames(d)

steps<-length(Flow)
windows()
par(mfrow=c(1,1))
bx2(d,1,steps,"muD[","]",Flow,ylab="Expected travel time","Flow",main="Travel time to video site, prior")


