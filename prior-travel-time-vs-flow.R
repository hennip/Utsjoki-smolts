# Need to have faster travel time to video site compared to
#aD~dlnorm(0.9,45) # mu=2.5,cv=0.2
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2

# 8/17 Need to have yet faster travel time compared to
#aD~dlnorm(0.68,45) # mu=2,cv=0.15
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
# Discussion with Panu in June; travel time at lowest flow
# q95 at 14 days 
# q75 at 8-9 days
# q50 at 4-5 days
# q25 at 2-3 days

source("packages-and-paths.r")


Flow<-seq(10,150, by=10)
nF<-length(Flow)
M2<-"
model{
for(i in 1:n){
 #     muD[i]~dbeta(alphaD[i], betaD[i])
 #     alphaD[i]<-mumuD[i]*etaD[i]
 #     betaD[i]<-mumuD[i]*etaD[i]

     muD[i]~dnorm(mumuD[i], taumuD[i])
      mumuD[i]<-exp(aD-bD*Flow[i])
      sdmuD[i]<-mumuD[i]*cvmuD
      taumuD[i]<-1/pow(sdmuD[i],2)
}
#cvmuD<-0.1
cvmuD~dunif(0.001,0.8)

#Priors:
muaD<-2 #1.75
cvaD<-0.1 # 0.33
mubD<-0.01
cvbD<-0.2
aD~dlnorm(log(muaD)-0.5*log(cvaD*cvaD+1),1/log(cvaD*cvaD+1)) 
bD~dlnorm(log(mubD)-0.5*log(cvbD*cvbD+1),1/log(cvbD*cvbD+1)) 

#aD~dlnorm(0.51,9.7) # mu=1.75,cv=0.33
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2


}"
#muaD<-1.75 
#cvaD<-0.33 
#mubD<-0.01
#cvbD<-0.2
#log(muaD)-0.5*log(cvaD*cvaD+1)
#1/log(cvaD*cvaD+1) 
#log(mubD)-0.5*log(cvbD*cvbD+1)
#1/log(cvbD*cvbD+1) 



cat(M2,file="prior_traveltime.txt")

data<-list(Flow=Flow, n=length(Flow))

system.time(jm<-jags.model('prior_traveltime.txt',
                           n.adapt=100,data=data,n.chains=2))

system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "muD"
                                  ),
                                  n.iter=10000,
                                  thin=1))
chainsP2<-chains1
summary(chainsP2, quantiles = c(0.05,0.25,0.50,0.75,0.95))

#source("functions/bx.r")
#d<-as.matrix(chainsP2)       # transform to a matrix for boxplotting
#colnames(d)

#steps<-length(Flow)
#windows()
#par(mfrow=c(1,1))
#bx2(d,1,steps,"muD[","]",Flow,ylab="Expected travel time","Flow",main="Travel time to video site, prior")

df<-boxplot.jags.df(chainsP2,"muD", Flow)

ggplot(df, aes(x))+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Flow (m3/s)", y="E(travel time) (in days)", title="Travel time to video site")+
  geom_line(aes(x,q50))+
  coord_cartesian(ylim=c(0,20))+
  theme_bw()




