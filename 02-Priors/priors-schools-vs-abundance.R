df<-dat_all


df<-filter(dat_all, Year>2008)

ggplot(df)+
#  geom_point(aes(x=log(smolts), y=log(schools), color=factor(Year)))
  geom_point(aes(x=smolts, y=schools, color=factor(Year)))

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
years<-c(2009,2010,2011,2012,2013,2014)
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
                                  n.iter=5000,
                                  thin=1))
system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "aS","bS", "cvS", "cvmuS"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains2
summary(chainsM)

windows()
traceplot(chainsM)
