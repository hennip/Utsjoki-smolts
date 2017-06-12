
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

Flow<-seq(10,150, by=10)
nF<-length(Flow)

a<-5
b<-0.1
mu<-c();P<-c();p<-c();sd<-c()
#cv<-0.1
for(i in 1:nF){
  mu[i]<-a-b*Flow[i]
  sd[i]<-0.001#<-rlnorm(1,0.1)

  P[i]<-rnorm(1,mu[i],sd[i])
  p[i]<-0.65*(exp(P[i])/(1+exp(P[i]))) +0.25
}

tF<-as.tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_line(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))


# Sitten lisataan hajontaa ja simuloidaan enemman dataa
Flow<-seq(10,150, by=1)
nF<-length(Flow)
a<-5
b<-0.1
mu<-c();P<-c();p<-c();sd<-c()
sd<-1
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



M2<-"
model{
for(i in 1:n){
#logit(p[i])<-P[i]
P[i]~dnorm(muP[i],tauP)
muP[i]<-aP+bP*Temp[i]
}

#Priors:
aP~dnorm(-20,0.01)
bP~dlnorm(0.1,1)
sdP~dlnorm(1,0.1)
tauP<-1/pow(sdP,2)

}"

cat(M2,file="model/priori2.txt")

data<-list(Temp=Temp,P=P, n=length(Temp))

system.time(jm<-jags.model('model/priori2.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "aP","bP"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)

# Sitten katsotaan millaista matskua saadut priorit tuottaisivat

# odotusarvot ja hajonnat edellisesta ajosta
muaP<--7.9 
sdaP<- 0.18
tauaP<-1/(sdaP*sdaP)
muaP;tauaP

mubP<-0.2 
sdbP<- 0.01
cvbP<-sdbP/mubP
taubP<-1/log(cvbP*cvbP+1)
MbP<-log(mubP)-0.5/taubP
MbP;taubP

#musdP<-4.3
#sdsdP<-0.63
#cvsdP<-sdsdP/musdP
#tausdP<-1/log(cvsdP*cvsdP+1)
#MsdP<-log(musdP)-0.5/tausdP
#MsdP;tausdP

Temp<-seq(1,30, by=1)

M2<-"
model{
for(i in 1:n){
logit(p[i])<-P[i]
P[i]~dnorm(muP[i],tauP)
muP[i]<-aP+bP*Temp[i]
}
aP~dnorm(-7.9,32)
bP~dlnorm(-1.61,401)
#sdP<-0.1#~dlnorm(1,0.1)
sdP~dlnorm(0.1,10)
tauP<-1/pow(sdP,2)


}"

cat(M2,file="model/priori2.txt")

data<-list(Temp=Temp, n=length(Temp))

system.time(jm<-jags.model('model/priori2.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                   "p"
                                  #  "P"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsP2<-chains1
summary(chainsP2)

d<-as.matrix(chainsP2)       # transform to a matrix for boxplotting
colnames(d)

steps<-length(Temp) 
par(mfrow=c(1,1))
bx2(d,1,steps,"p[","]",xlabseq=Temp,ylab="p",xlabname="Temperature")

SaveStats(d,"p_test")


SaveStats<-function(x,name){
  n<-dim(x)[2]
  tmp<-array(NA, dim=c(n,7))
  for(i in 1:n){
    tmp[i,1:2]<-summary(as.mcmc(x[,i]))$statistics[1:2]
    tmp[i,3:7]<-summary(as.mcmc(x[,i]))$quantiles
  }
  colnames(tmp)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")
  write.xlsx(tmp, paste(sep="", name,".xlsx"))
  }








################################
# Tarkastellaan smolttimallin posterioreja

# posteriori mediaanit ajosta
muaP<--20.6 
sdaP<- 1.96
tauaP<-1/(sdaP*sdaP)
muaP;tauaP

mubP<-1.22 
sdbP<- 0.14
cvbP<-sdbP/mubP
taubP<-1/log(cvbP*cvbP+1)
MbP<-log(mubP)-0.5/taubP
MbP;taubP

musdP<-4.3
sdsdP<-0.63
cvsdP<-sdsdP/musdP
tausdP<-1/log(cvsdP*cvsdP+1)
MsdP<-log(musdP)-0.5/tausdP
MsdP;tausdP

max(data$Temp[,3])

Temp<-seq(1,20, by=1)

M2<-"
model{
for(i in 1:n){
logit(p[i])<-P[i]
P[i]~dnorm(muP[i],tauP)
muP[i]<-aP+bP*Temp[i]
}
#Posteriors:
#aP~dnorm(-20.6,0.26)
#bP~dlnorm(0.19,76)
#sdP~dlnorm(1.45,47)


#Priors:
aP~dnorm(-20,0.01)
bP~dlnorm(0.1,1)
sdP~dlnorm(1,0.1)
tauP<-1/pow(sdP,2)


}"

cat(M2,file="model/priori2.txt")

data<-list(Temp=Temp, n=length(Temp))

system.time(jm<-jags.model('model/priori2.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsP2<-chains1
summary(chainsP2)

source("functions/bx.r")
d<-as.matrix(chainsP2)       # transform to a matrix for boxplotting
colnames(d)

steps<-length(Temp) 
par(mfrow=c(1,1))
#bx(d,1,steps,"p[","]",ylab="p")
bx2(d,1,steps,"p[","]",xlabseq=Temp,ylab="p",xlabname="Temperature")

p_prior<-array(NA, dim=c(20,7))
for(i in 1:20){
#  i<-1
  p_prior[i,1:2]<-summary(as.mcmc(d[,i]))$statistics[1:2]
  p_prior[i,3:7]<-summary(as.mcmc(d[,i]))$quantiles
}
colnames(p_prior)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")
write.xlsx(p_prior, "pP_prior.xlsx")



# Sama suoraan malliestimaattien pohjalta, pidetaan parametrien
# valiset korrelaatiot mukana 
# -> pitaisi vastata paremmin ennustejakaumia tietylla lampotilalla
nT<-length(Temp)
n_samp<-length(chains[,"aP"][[1]])
aP_samp<-chains[,"aP"][[1]]
bP_samp<-chains[,"bP"][[1]]
sdP_samp<-chains[,"sdP"][[1]]
tauP_samp<-1/(sdP_samp^2)

muP_samp<-array(NA, dim=c(nT,n_samp))
P_samp<-array(NA, dim=c(nT,n_samp))
p_samp<-array(NA, dim=c(n_samp,nT))
for(j in 1:n_samp){
    for(i in 1:nT){
      muP_samp[i,j]<-aP_samp[j]+bP_samp[j]*Temp[i]
      P_samp[i,j]<-rnorm(1,muP_samp[i,j],tauP_samp[i])
      p_samp[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))
    }
}
p_samp
colnames(p_samp)<-colnames(d)

par(mfrow=c(1,1))
#bx(p_samp,1,steps,"p[","]",ylab="p")
bx2(p_samp,1,steps,"p[","]",Temp,ylab="p","Temperature")
bx2_y(p_samp,1,steps,"p[","]",ylim=c(0,1),Temp,ylab="p","Temperature")


summary(as.mcmc(p_samp))

SaveStats(p_samp,"P_new")




p_table<-array(NA, dim=c(20,7))
for(i in 1:20){
  p_table[i,1:2]<-summary(as.mcmc(p_samp[,i]))$statistics[1:2]
  p_table[i,3:7]<-summary(as.mcmc(p_samp[,i]))$quantiles
}
colnames(p_table)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")
#write.xlsx(p_table, "pP_predicted_08weekendsOff.xlsx")
write.xlsx(p_table, "pP_new.xlsx")
