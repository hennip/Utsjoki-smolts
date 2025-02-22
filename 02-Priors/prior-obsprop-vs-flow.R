
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

# Panun expert-info 16.6.17:
# Mahdolliset arvot välillä [0.9,0.3], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.75-0.9), 
# tästä verkkaisesti laskee (ei romahda) minimiin, jossa kuitenkin 
# suuri epävarmuus (0.3-0.6), jakauma ennemmin tasainen kuin huipukas.
# Virtaamilla 50-60 m3/s olosuhteet havaita ovat selkeästi huonot.
# Kun virtaama nousee 10m3/s->60m3/s, veden korkeus nousee metrin.

# Panun expert-info 10.11.22:
# Sivu-uomassa mahdolliset arvot välillä [0.95,0.45], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.95-0.9), 
# tästä lähtee laskemaan kun virtaama kasvaa. 
# Samankaltainen käyrä kuin keskiosan hav tn:llä
# mutta havaittavuus pysyy pitempään korkeana

# Yhdistetään aikaisemmat prior-obsprop-vs-flow - tiedostot ja poistetaan ylimääräiset
# -> yksi perustiedosto, gitissä versiot

Flow<-seq(0,100, by=0.3)
nF<-length(Flow)

# Nämä kun alarajaa ei ole
a<-3.5
b<-0.08
# Alaraja 0.3
#a<-2.5
#b<-0.15
mu<-c();P<-c();p<-c();sd<-c()
sd<-.5
for(i in 1:nF){
  mu[i]<-a-b*Flow[i]
  
  P[i]<-rnorm(1,mu[i],sd)
  #p[i]<-0.6*(exp(P[i])/(1+exp(P[i])))+0.3
  p[i]<-0.9*(exp(P[i])/(1+exp(P[i]))) # ei alarajaa
}

tF<-as_tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_point(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))


# Sovitetaan P:t edellisestä ja estimoidaan aB, bB ja sdB

Flow<-seq(0,100, by=1)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
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

mubB<-summary(chainsM[,"bB"])$statistics[1]
sdbB<-summary(chainsM[,"bB"])$statistics[2]
cvbB<-sdbB/mubB
taubB<-1/log(cvbB*cvbB+1)
MbB<-log(mubB)-0.5/taubB

musdB<-summary(chainsM[,"sdB"])$statistics[1]
sdsdB<-summary(chainsM[,"sdB"])$statistics[2]
cvsdB<-sdsdB/musdB
tausdB<-1/log(cvsdB*cvsdB+1)
MsdB<-log(musdB)-0.5/tausdB


muaB;tauaB
MbB;taubB
MsdB;tausdB

Flow<-seq(0,100, by=2)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
#p[i]<-0.6*p2[i]+0.3
p[i]<-0.9*p2[i]
#p[i]<-(0.9-z)*p2[i]+z

logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]


muB_side[i]<-aB*(coef_side*0.5+1)-bB_side*Flow[i]
P_side[i]~dnorm(muB_side[i],tauB_side)
#p_side[i]<-0.5*p2_side[i]+0.45
p_side[i]<-0.6*p2_side[i]+0.3 # only to compare curves with the same limits
logit(p2_side[i])<-P_side[i]

}
tauB<-1/pow(sdB,2)
tauB_side<-1/pow(sdB_side,2)
coef_side~dbeta(2,2)T(0.02,0.98) # for aB_side
#z~dbeta(0.3*20,0.7*20)

#aB~dnorm(mu.aB,t.aB)
#bB~dlnorm(M.bB,T.bB)
#sdB~dlnorm(M.sdB,T.sdB)

#Ei alarajaa
aB~dnorm(3.6,10)
bB~dlnorm(-2.5,10)
sdB~dlnorm(0.01,20)

# # Alaraja, E(z)=0.3
# aB~dnorm(3,10)
# bB~dlnorm(-2.5,10)
# sdB~dlnorm(0.01,20)


#Sivuoma vapaasti päivittyvillä prioreilla
# aB_side = aB*(coef_side*0.5+1)
bB_side~dlnorm(-2.5,10)
sdB_side~dlnorm(0.01,20)


}"

cat(M2,file="prior-obs.txt")

data<-list( 
  #   mu.aB=muaB,t.aB=tauaB, 
  #   M.bB=MbB, T.bB=taubB, 
  #   M.sdB=MsdB, T.sdB=tausdB,
  Flow=Flow, n=nF
)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p", "p_side", "z"
                                  ),
                                  n.iter=5000,
                                  thin=1))

summary(chains1[,"z"])

df<-boxplot.jags.df(chains1,"p",Flow)
df<-as_tibble(df)
df<-filter(df, x>=0)
df_side<-boxplot.jags.df(chains1,"p_side",Flow)
df_side<-as_tibble(df_side)
d_sidef<-filter(df_side, x>=0)

ggplot(df, aes(x, group=x))+
  geom_boxplot(
    data=df_side,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",colour="grey", fill="grey95")+
  #geom_boxplot(
  #  aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #  stat = "identity",fill=rgb(1,1,1,0.6))+
  coord_cartesian(ylim=c(0,1), xlim=c(0,100))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  labs(x="Flow", y="Probability to be observed", title="Obs prop mid stream/ side stream")



filter(df, x==10 |x==15 | x==20 |x==50 |x==60|x==80)

