
# special cases: N=s, N=0, N=s=1
# s: keskim. parvikoko, N/s parvien lkm
  
#  N<-c(0,1,2,3,2)
#  s<-c(1,1,2,3,1)
#  N-s
#  ((N-s)/((s-1)+0.1))+1
  
################################################

# In here we could use data from years that are not used in the analysis,
# but preferably we should have priors that can be used for any dataset
# => expert info

# Panu 28.9.17: 
# - Schools are relatively small but those can occur frequently -> need to have asymptotic school size
# - Daily number passing video site a good covariate
# - At the beginning of the season schools may be larger (temporal variation)

K<-500
slope<-0.15
N<-c(0,1,100)
(K*N)/((K/slope)+N)+0.0001

Ntrue<-seq(1,20000, by=50)
n<-length(Ntrue)

K<-500
slope<-0.15
muS<-c();s<-c();
etaStarB<-c()
sdS<-0.15
for(i in 1:n){
  muS[i]<-(K*Ntrue[i])/((K/slope)+Ntrue[i])
  s[i]<-rlnorm(1,log(muS[i])-0.5*sdS*sdS,sdS)
  etaStarB[i]<-(Ntrue[i]-s[i])/(s[i]-1+0.01)+1
  
}
cbind(Ntrue,s)[1:10,]
tS<-as.tibble(cbind(Ntrue,muS,s, etaStarB))

ggplot(tS) + 
#  coord_cartesian(xlim=c(0,1000))+
  geom_point(aes(Ntrue, s))+
  labs(title=paste(sep="","K=",K," slope=",slope))
#  geom_line(aes(Ntrue, Ntrue))



# Sovitetaan s:t edellisestä ja estimoidaan K, slope ja sd

M2<-"
model{
for(i in 1:n){
  s[i]~dlnorm(MS[i], TS)
  MS[i]<-log(muS[i])-0.5/TS
  muS[i]~dlnorm(log(mumuS[i])-0.5/TmuS,TmuS)
  mumuS[i]<-(K*Ntrue[i])/((K/slope)+Ntrue[i])
}
TS<-1/log(cvS*cvS+1)
TmuS<-1/log(cvmuS*cvmuS+1)
K~dunif(1,10000)
slope~dunif(0.000001,1)

cvS<-0.01
cvmuS~dunif(0.001,1)

}"

cat(M2,file="prior-schools.txt")

data<-list(Ntrue=Ntrue,s=s, n=n)

system.time(jm<-jags.model('prior-schools.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "K","slope", "cvmuS"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)




# Sitten katsotaan millaista matskua saadut priorit tuottaisivat

# odotusarvot ja hajonnat edellisesta ajosta
muK<-summary(chainsM[,"K"])$statistics[1]
sdK<-summary(chainsM[,"K"])$statistics[2]
cvK<-sdK/muK
tauK<-1/log(cvK*cvK+1)
MK<-log(muK)-0.5/tauK
MK;tauK

mu_slope<-summary(chainsM[,"slope"])$statistics[1]
sd_slope<-summary(chainsM[,"slope"])$statistics[2]
cv_slope<-sd_slope/mu_slope
tau_slope<-1/log(cv_slope*cv_slope+1)
M_slope<-log(mu_slope)-0.5/tau_slope
M_slope;tau_slope


Ntrue<-seq(1,10000, by=50)
n<-length(Ntrue)

M2<-"
model{
for(i in 1:n){
  s[i]~dlnorm(MS[i], TS)
  MS[i]<-log(muS[i])-0.5/TS
  muS[i]~dlnorm(log(mumuS[i])-0.5/TmuS,TmuS)
  mumuS[i]<-(K*Ntrue[i])/((K/slope)+Ntrue[i])
}
TS<-1/log(cvS*cvS+1)
TmuS<-1/log(cvmuS*cvmuS+1)
#K~dlnorm(6.07,1.22)
K~dlnorm(6.07,0.7)
slope~dlnorm(-1.94,66)

cvS~dunif(0.001,2)
cvmuS~dunif(0.001,2)

}"

cat(M2,file="prior-schools.txt")

data<-list( 
  Ntrue=Ntrue, n=n
)

system.time(jm<-jags.model('prior-schools.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "muS","mumuS","s"
                                  ),
                                  n.iter=5000,
                                  thin=1))


#df<-boxplot.jags.df(chains1,"mumuS",Ntrue)
#df<-boxplot.jags.df(chains1,"muS",Ntrue)
df<-boxplot.jags.df(chains1,"s",Ntrue)
df<-as.tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x))+
  coord_cartesian(xlim=c(0,1000), ylim=c(0,200))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")





#################################
# priors based on data, not needed now


df2<-filter(dat_all, Year>2008 & Year<2014)

ggplot(df2)+
#  geom_point(aes(x=log(smolts), y=log(schools), color=factor(Year)))
  geom_point(aes(x=smolts, y=schools, color=factor(Year)))+
  geom_smooth(aes(x=smolts, y=schools))


Nnew<-seq(1,2000, by=30)
length(Nnew)
M2<-"
model{
  for(y in 1:nYears){
    for(i in 1:n){
      s[i,y]~dlnorm(log(muS[i,y])-0.5/TS,TS)
      # simple hierarhcical model for school size
      muS[i,y]~dlnorm(log(mumuS[i,y])-0.5/TmuS,TmuS)
      mumuS[i,y]<-aS+bS*N[i,y]
    }
  }
  
  aS~dunif(0.001,10)
  bS~dunif(0.001,1)
  cvmuS~dunif(0.001,2)
  cvS~dunif(0.001,2)
  TmuS<-1/log(cvmuS*cvmuS+1)
  TS<-1/log(cvS*cvS+1)

  for(i in 1:67){
    mumuS_pred[i]<-aS+bS*N_pred[i]
    muS_pred[i]~dlnorm(log(mumuS_pred[i])-0.5/TmuS,TmuS)
    s_pred[i]~dlnorm(log(muS_pred[i])-0.5/TS,TS)
  }

}"

cat(M2,file="prior-schools.txt")

#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2009:2013)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july


data<-list(
  N_pred=Nnew,
  n=n_days,
  nYears=length(years),
  s=df$Schools,
  N=df$Smolts)

system.time(jm<-jags.model('prior-schools.txt',
                           n.adapt=100,data=data,n.chains=2))

var_names<-c(
  "s_pred",
  "aS","bS", "cvS", "cvmuS"
)

system.time(chains1<-coda.samples(jm,variable.names=var_names,n.iter=10000,thin=10))
system.time(chains2<-coda.samples(jm,variable.names=var_names,n.iter=10000,thin=10))
system.time(chains3<-coda.samples(jm,variable.names=var_names,n.iter=10000,thin=10))

chainsM<-combine.mcmc(list(chains2, chains3))
summary(chainsM)
sumM<-summary(chainsM)$statistics

par(mfrow=c(2,2))
traceplot(chainsM)

cv<-sumM[,2]/sumM[,1]
Tau<-1/log(cv*cv+1)
M<-log(sumM[,1])-0.5/Tau
cbind(M,Tau)


df<-boxplot.jags.df(chainsM,"s_pred",Nnew)
df<-as.tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+  
  geom_point(data=df2,aes(x=smolts, y=schools, color=factor(Year)))
  
  
  View(df2)
  


