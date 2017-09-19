
# What should s be when N==0
N<-0
s<-1
etastar<-(N-s)/((s-1)+N-1)
etastar
# => if s=1 when N=0, etastar>1 always

df2<-filter(dat_all, Year>2008)

ggplot(df2)+
#  geom_point(aes(x=log(smolts), y=log(schools), color=factor(Year)))
  geom_point(aes(x=smolts, y=schools, color=factor(Year)))+
  geom_smooth(aes(x=smolts, y=schools))

point(aes(x=smolts, y=schools, color=factor(Year)))

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

}"

cat(M2,file="prior-schools.txt")

#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2009:2014)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july


data<-list(
  n=n_days,
  nYears=length(years),
  s=df$Schools,
  N=df$Smolts)

system.time(jm<-jags.model('prior-schools.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "aS","bS", "cvS", "cvmuS"
                                  ),
                                  n.iter=10000,
                                  thin=10))
system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "aS","bS", "cvS", "cvmuS"
                                  ),
                                  n.iter=10000,
                                  thin=10))
system.time(chains3<-coda.samples(jm,
                                  variable.names=c(
                                    "aS","bS", "cvS", "cvmuS"
                                  ),
                                  n.iter=10000,
                                  thin=10))

chainsM<-chainsP<-combine.mcmc(list(chains2, chains3))
summary(chainsM)
sumM<-summary(chainsM)$statistics

par(mfrow=c(2,2))
traceplot(chainsM)

cv<-sumM[,2]/sumM[,1]
Tau<-1/log(cv*cv+1)
M<-log(sumM[,1])-0.5/Tau
cbind(M,Tau)
