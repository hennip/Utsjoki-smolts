source("packages-and-paths.r")
source("tidy-functions.r")

# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

Flow<-seq(10,150, by=1)
nF<-length(Flow)

a<-5
b<-0.1
mu<-c();P<-c();p<-c();sd<-c()
sd<-0.7
for(i in 1:nF){
  mu[i]<-a-b*Flow[i]

  P[i]<-rnorm(1,mu[i],sd)
  p[i]<-0.65*(exp(P[i])/(1+exp(P[i]))) +0.25
}

tF<-as.tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_point(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))


Flow<-seq(10,150, by=1)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
#p[i]<-0.65*p2[i]+0.25
#logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]
}
tauB<-1/pow(sdB,2)

#sdB<-0.7
#aB~dnorm(5,1)
#bB~dlnorm(log(1)-0.5/tau_bB,tau_bB)
#cv_bB<-0.3
#tau_bB<-1/log(cv_bB*cv_bB+1)

aB~dnorm(1,0.01)
bB~dlnorm(0.1,1)
sdB~dlnorm(1,0.1)
#sdB~dlnorm(log(0.7)-0.5/tau_sdB,tau_sdB)
#tau_sdB<-1
}"

cat(M2,file="prior-obs.txt")

data<-list(Flow=Flow,P=P, n=nF)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                  "aB","bB", "sdB"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)


  


# Sitten katsotaan millaista matskua saadut priorit tuottaisivat

# odotusarvot ja hajonnat edellisesta ajosta
muaB<-5 
sdaB<- 0.13
tauaB<-1/(sdaB*sdaB)
muaB;tauaB

mubB<-0.1 
sdbB<- 0.003
cvbB<-sdbB/mubB
taubB<-1/log(cvbB*cvbB+1)
MbB<-log(mubB)-0.5/taubB
MbB;taubB

musdB<-0.7
sdsdB<-0.1
cvsdB<-sdsdB/musdB
tausdB<-1/log(cvsdB*cvsdB+1)
MsdB<-log(musdB)-0.5/tausdB
MsdB;tausdB

Flow<-seq(10,150, by=10)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
p[i]<-0.65*p2[i]+0.25
logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]
}
tauB<-1/pow(sdB,2)

aB~dnorm(5,59)
bB~dlnorm(-2.3,111)
sdB~dlnorm(-0.4,50)

}"

cat(M2,file="prior-obs.txt")

data<-list(Flow=Flow, n=nF)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)






df<-boxplot.jags.df(chains1,"p",Flow)


ggplot(df, aes(x))+
  geom_boxplot(
    
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity"
  )+
  coord_cartesian(ylim=c(0,1))



