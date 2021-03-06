
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

# Panun expert-info 16.6.17:
# Mahdolliset arvot välillä [0.9,0.3], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.75-0.9), 
# tästä verkkaisesti laskee (ei romahda) minimiin, jossa kuitenkin 
# suuri epävarmuus (0.3-0.6), jakauma ennemmin tasainen kuin huipukas.
# Virtaamilla 50-60 m3/s olosuhteet havaita ovat selkeästi huonot.
# Kun virtaama nousee 10m3/s->60m3/s, veden korkeus nousee metrin.

Flow<-seq(-10,100, by=0.5)
nF<-length(Flow)

a<-2.2
b<-0.15
mu<-c();P<-c();p<-c();sd<-c()
sd<-0.8
for(i in 1:nF){
  mu[i]<-a-b*Flow[i]
  
  P[i]<-rnorm(1,mu[i],sd)
  p[i]<-0.6*(exp(P[i])/(1+exp(P[i]))) +0.3
}

tF<-as.tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_point(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))


# Sovitetaan P:t edellisestä ja estimoidaan aB, bB ja sdB

Flow<-seq(-10,100, by=1)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
#p[i]<-0.6*p2[i]+0.3
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
muaB<-summary(chainsM[,"aB"])$statistics[1]
sdaB<-summary(chainsM[,"aB"])$statistics[2]
tauaB<-1/(sdaB*sdaB)
muaB;tauaB

mubB<-summary(chainsM[,"bB"])$statistics[1]
sdbB<-summary(chainsM[,"bB"])$statistics[2]
cvbB<-sdbB/mubB
taubB<-1/log(cvbB*cvbB+1)
MbB<-log(mubB)-0.5/taubB
MbB;taubB

musdB<-summary(chainsM[,"sdB"])$statistics[1]
sdsdB<-summary(chainsM[,"sdB"])$statistics[2]
cvsdB<-sdsdB/musdB
tausdB<-1/log(cvsdB*cvsdB+1)
MsdB<-log(musdB)-0.5/tausdB
MsdB;tausdB

Flow<-seq(-10,100, by=5)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
p[i]<-0.6*p2[i]+0.3
logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]
}
tauB<-1/pow(sdB,2)

#aB~dnorm(mu.aB,t.aB)
#bB~dlnorm(M.bB,T.bB)
#sdB~dlnorm(M.sdB,T.sdB)

aB~dnorm(2.9,60)
bB~dlnorm(-2.6,984)
sdB~dlnorm(-0.23,210)

}"

cat(M2,file="prior-obs.txt")

data<-list( 
  #  mu.aB=muaB,t.aB=tauaB, 
  #  M.bB=MbB, T.bB=taubB, 
  #  M.sdB=MsdB, T.sdB=tausdB,
  Flow=Flow, n=nF
)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p"
                                  ),
                                  n.iter=5000,
                                  thin=1))


df<-boxplot.jags.df(chains1,"p",Flow)
df<-as.tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  coord_cartesian(ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

filter(df, x==10 | x==20 |x==50 |x==60)
