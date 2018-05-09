# This is a small arrival model for complete arrival data to include
# school size as a determining factor to variation of observation process
# small set on known data, small (expected) observation process. 
# Two model versions for observation variance (overdispersion):
# 1) with minimally informative etaB, no school size (vrt 2002 paper)
# 2) with school size (and no etaB-component)
# What is the difference for evaluated uncertainty?

#source("00-Functions/packages-and-paths.r")



M1<-"
model{

  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){
      
      # Observed number of fish
      # Nobs[i,y]~dbetabin(100,10,N[i,y])  
      #Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y])  
      Nobs[i,y]~dbetabin(muB[i,y]*etaStarB[i,y],(1-muB[i,y])*etaStarB[i,y],N[i,y])  
      
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))
      
      etaStarB[i,y]<-(N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1

      s[i,y]~dlnorm(log(muS[i,y])-0.5/TS,TS)
      muS[i,y]~dlnorm(log((K*N[i,y])/((K/slope)+N[i,y])+0.0001)-0.5/TmuS,TmuS)
    }
  }
  # priors for observation process
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(5,1000)

  # priors for schooling
  K~dlnorm(6.07,0.7)
  slope~dlnorm(-1.94,66)
  cvS~dunif(0.001,2)
  cvmuS~dunif(0.001,2)

  TmuS<-1/log(cvmuS*cvmuS+1)
  TS<-1/log(cvS*cvS+1)
  
  # Abundance
  # ==============
  for(y in 1:nYears){
#    for(i in 1:nDays){
#      qN[i,y]<-N[i,y]/Ntot[y]
#      N[i,y]<-round(exp(logN[i,y]))
#      logN[i,y]~dunif(-1000,9.2) # total run size in year y
#    }
#     Ntot[y]<-sum(N[1:nDays,y])

    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y
    
    for(i in 1:nDays){
      N[i,y]<-round(qN[i,y]*Ntot[y])
    }
    qN[1:nDays,y]~ddirich(ones) # flat prior
  }
}
"

modelName<-"Schools_etaStarB_dirich"
#modelName<-"Schools_etaB_dirich"
#modelName<-"Schools_etaStarB_indepN"
#modelName<-"Schools"
#modelName<-"Schools_etaStarB"
#modelName<-"Schools_etaB_indepN"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)


# full data; temp data missing for 2012 and partly for 2010, note that the
# run was late in 2009
#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005,2006,2008)
dataName<-"050608"

n_days<-61
dat<-dat_all # all real data
#dat<-dat_all2 # 2007 simulated
df<-smolts_data_to_jags(dat,years, n_days) # 61: only june & july

ones<-rep(1,n_days)
#ones<-rep(1/n_days,n_days)


data<-list(
  ones=ones,
  s=df$Schools,
  flow=df$Flow,
  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)

inits<-list(list(LNtot=rep(14,data$nYears)),
               list(LNtot=rep(14,data$nYears)))
#initials<-list(list(logN=array(8.2,dim=c(data$nDays,data$nYears))),
#               list(logN=array(8.2,dim=c(data$nDays,data$nYears))))

var_names<-c(
  "etaB",
  "muB", "qN",
  "aB","bB","sdBB",
  "K","slope","cvS", "cvmuS",
  "Ntot","N"
)


t1<-Sys.time();t1
run1 <- run.jags(M1, 
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=300, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 20min


t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=4000, thin=3000, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#10h

run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=F, sample=4000, thin=3000, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#10h
run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=T, sample=4000, thin=3000, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#10h
run<-run4
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run5 <- extend.jags(run4, combine=T, sample=4000, thin=3000, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#10h

run<-run5
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

