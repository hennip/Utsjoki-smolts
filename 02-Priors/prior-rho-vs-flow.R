
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle keskiuomassa menevien smolttien osuuteen

# Panun ja Jaskan expert-info 10.11.22:
# Missään olosuhteissa sivu-uomissa ei mene enempää kuin puolet
# Jokin vedenkorkeus on niin matala, että sivuissa ei ole muuta kuin kivet/lätäköitä



Flow<-seq(-10,100, by=0.2)
nF<-length(Flow)

a<-4
b<-0.08
mu<-c();P<-c();p<-c();sd<-c()
sd<-2
for(i in 1:nF){
  mu[i]<-a-b*Flow[i]
  
  P[i]<-rnorm(1,mu[i],sd)
  p[i]<-0.5*(exp(P[i])/(1+exp(P[i]))) +0.5
}

tF<-as_tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_point(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))+
  geom_vline(xintercept=0)


# Sovitetaan P:t edellisestä ja estimoidaan aB, bB ja sdB

# Sopivien priorien etsimiseksi
# mu<-0.01
# cv<-10
# Tau<-1/(log(cv*cv+1))
# M<-log(mu)-0.5/Tau
# M;Tau


M2<-"
model{
for(i in 1:n){

P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]
}
tauB<-1/pow(sdB,2)


aB~dunif(0,10)#dnorm(1,0.01)
bB~dunif(0,10)#dlnorm(-3.4,0.43)
sdB~dunif(0.001,5)#dlnorm(1,0.1)

aBX~dunif(0,10)#dnorm(1,0.01)
bBX~dunif(0,10)#dlnorm(-3.4,0.43)
sdBX~dunif(0.001,5)#dlnorm(1,0.1)
}"

cat(M2,file="prior-obs.txt")

data<-list(Flow=Flow,P=P, n=nF)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    #"p",
                                    "aB","bB", "sdB",
                                    "aBX","bBX", "sdBX"
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

Flow<-seq(-10,100, by=2)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
p[i]<-0.5*p2[i]+0.5
logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-a-b*Flow[i]
}
tauB<-1/pow(sd,2)

# aB~dnorm(mu.aB,t.aB)
# bB~dlnorm(M.bB,T.bB)
# sdB~dlnorm(M.sdB,T.sdB)


# rho: proportion of smolts passing site at mid river
  a~dnorm(3.86,1)
  b~dlnorm(-2.59,10)
  sd<-0.001#~dlnorm(-0.5,1)


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
                                    "p", "sd", "a", "b"
                                  ),
                                  n.iter=5000,
                                  thin=1))

# var_names=c("p", "sdB", "aB", "bB")
# run<-run.jags(M2,
#          monitor= var_names,data=data,#inits = inits,
#          n.chains = 2, method = 'parallel', 
#          thin=1,modules = "mix",sample =5000, adapt = 100,
#          progress.bar=F)
# 
# summary(run, var="B")
# 
 summary(chains1[,"a"])
 summary(chains1[,"b"])
 summary(chains1[,"sd"])


df<-boxplot.jags.df(chains1,"p",Flow)
df<-as.tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x, group=x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  coord_cartesian(ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  labs(title="Smolttien osuus keskiuomassa")+
  xlab("Virtaama")

filter(df, x==10 | x==20 |x==50 |x==60)

