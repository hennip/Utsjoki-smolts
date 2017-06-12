source("packages-and-paths.r")

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



M2<-"
model{
for(i in 1:n){
logit(p[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB-bB*Flow[i]
}
sdB<-0.01
tauB<-1/pow(sdB,2)

aB~dnorm(5,1/pow(0.1,2))
bB~dlnorm(log(0.1)-0.5*pow(0.01,2),1/pow(0.01,2))

}"

cat(M2,file="prior-obs.txt")

data<-list(Flow=Flow,P=P, n=nF)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "aB","bB", "sdB","p"
                                  ),
                                  n.iter=5000,
                                  thin=1))

#source("sample-obsprop.r")
c1<-chainsM[[1]]

# Posterior
n_samp<-length(c1[,"aB"])
aB_samp<-c1[,"aB"]
bB_samp<-c1[,"bB"]
sdB_samp<-c1[,"sdB"]

muD_samp<-array(NA, dim=c(n_samp,nF))
mumuD_samp<-array(NA, dim=c(n_samp,nF))
sdmuD_samp<-array(NA, dim=c(n_samp,nF))
#cvmuD<-0.1
for(j in 1:n_samp){
  for(i in 1:nF){
    muB_samp[j,i]<-aB_samp[j]-bB_samp[j]*Flow[i]
    
    sdB_samp[j,i]<-mumuD_samp[j,i]*cvmuD_samp[j]
    PB_samp[j,i]<-rnorm(1,muB_samp[j,i], sdB_samp[j,i])
    
  }
}


df<-boxplot.df(p_samp, Temp)
df.prior<-boxplot.df(p_sampP, Temp)


# In black and white
ggplot(df, aes(x))+
  #  theme_bw()+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey99")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Temperature (degrees celsius)", y="Probability", title="Probability to begin migration")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50),col="grey")


