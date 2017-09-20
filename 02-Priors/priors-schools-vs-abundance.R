
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
years<-c(2009:2014)
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
  geom_point(df2,aes(x=smolts, y=schools, color=factor(Year)))
  
  
  
  


