

source("00-Functions/packages-and-paths.R")



M1<-"
model{

  # Predicting missing values of flow and water temperature using weatherdata
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      
      #   Water temperature is estimated from air temperature
      Temp[i,y] ~ dlnorm(log(mu_temp[i,y])-0.5*log(cv_temp[i,y]*cv_temp[i,y]+1), 1/log(cv_temp[i,y]*cv_temp[i,y]+1))
      mu_temp_r[i,y] = a_temp + b_temp[1]*Temp_air[i,y]
      #   this is to make sure it stays positive
      mu_temp[i,y] =  ifelse(mu_temp_r[i,y]>=0.0001, mu_temp_r[i,y], 0.0001)
      
      #   Flow is estimetetd using water temperature, rain and days since last rain 
      flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl[i,y]*cv_fl[i,y]+1), 1/log(cv_fl[i,y]*cv_fl[i,y]+1))
      mu_fl_r[i,y] =  a_fl + b_fl[1]*Temp[i,y] + b_fl[2]*Rain_bf[i,y] + b_fl[3]*Rain[i,y]
      #   this is to make sure it stays positive
      mu_fl[i,y] =  ifelse(mu_fl_r[i,y]>=0.0001, mu_fl_r[i,y], 0.0001)
      
    }
  }
  #   uninformative priors
  a_temp ~ dnorm(0, 100^-2)
  a_fl ~ dnorm(0, 100^-2)
  
  b_temp[1] ~ dnorm(0, 100^-2)
  
  for(i in 1:3){
    b_fl[i] ~ dnorm(0, 100^-2)
  }

 
  for(y in 1:nYears){
    for(i in 1:nDays){
      
      cv_temp[i,y] <- phi_temp[i,y]*2+0.01
      phi_temp[i,y]~dbeta(mu_phi_temp*eta_phi_temp, (1-mu_phi_temp)*eta_phi_temp)

      cv_fl[i,y] <- phi_fl[i,y]*2+0.01
      phi_fl[i,y]~dbeta(mu_phi_fl*eta_phi_fl, (1-mu_phi_fl)*eta_phi_fl)
    }
  }
      mu_phi_temp~dbeta(2,2)
      eta_phi_temp~dunif(1,1000) 
      mu_phi_fl~dbeta(2,2)
      eta_phi_fl~dunif(1,1000) 


  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
    
      # Observed number of fish
      Nobs[i,y]~dbin(p_obs[i,y],N[i,y])  
      p_obs[i,y]~dbeta(muB[i,y]*etaB, (1-muB[i,y])*etaB)
      #Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y])  
      #Nobs[i,y]~dbetabin(muB[i,y]*etaStarB[i,y],(1-muB[i,y])*etaStarB[i,y],N[i,y])
      
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))
      
      #etaStarB[i,y]<-(N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1
      
      #s[i,y]~dlnorm(log((K*N[i,y])/((K/slope)+N[i,y])+0.0001)-0.5/TS,TS)
    }
  }
  # priors for observation process
# wide priors 
#aB~dnorm(2.9,1)
#bB~dlnorm(-2.6,1)
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(5,1000)
  
  # priors for schooling
  K~dlnorm(6.07,0.7)
  slope~dlnorm(-1.94,66)
  cvS~dunif(0.001,2)
  TS<-1/log(cvS*cvS+1)
  
  # Abundance
  # ==============
  for(y in 1:nYears){
    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y
    #Ntot[y]<-100000
    
    #N[1:nDays,y]~dmulti(qN[1:nDays,y],Ntot[y]) # daily true number of fish
    for(i in 1:(nDays-1)){
      N[i,y]<-round(qN[i,y]*Ntot[y])
    }
    N[nDays,y]<-round(Ntot[y]*(1-sum(qN[1:(nDays-1),y])))    
  }
    
    # Timig of the smolt run
    # i.e. how total number of smolts passing the video site
    # is distributed between 61 days
    # =============================================
  for(y in 1:nYears){
    # qN: daily proportion of smolts
    # dirichlet-distribution approximated with lognormal
    qN[1:nDays,y]<-zN[1:nDays,y]/sum(zN[1:nDays,y])
    
    for(i in 1:nDays){
      zN[i,y]~dlnorm(MN[i,y], TauN[i,y])
    }
    
    alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN#+0.001
    MN[1:nDays,y]<-log(muqN[1:nDays,y])-0.5/TauN[1:nDays,y]
    TauN[1:nDays,y]<-1/log((1/alphaN[1:nDays,y])+1)  
  }
  
  # Process for departure
  # ===========================
  for(y in 1:nYears){
    for(i in 1:nDays){
      # p: probability to start migration at day t, if haven't done so earlier
      # departure probability depends on temperature
      logit(p[i,y])<-P[i,y]
      P[i,y]~dnorm(aP+bP*Temp[i,y],1/pow(sdP,2))
    }
  }
  
  # Migration speed (in days to video site)
  # ==============
  # probability to be at video site in day j, if departing at day i
  for(y in 1:nYears){
    for(i in 1:nDays){ 
      # i: day of departure
      # j: day of passing the video site
      #j==i
      qDx[i,i,y]<-phi((log(0.5)-MD[i,y])/SD)
      
      # j>i
      for(j in (i+1):(i+13)){ #13 
        qDx[i,j,y]<-phi((log(j-i+0.5)-MD[i,y])/SD)-phi((log(j-i-0.5)-MD[i,y])/SD)
      }
      
      for(j in i:(i+13)){
        qD[i,j,y]<-qDx[i,j,y]/(sum(qDx[i,i:(i+13),y])+0.0001)
      }
      
      MD[i,y]<-log(muD[i,y])-0.5/TD
      muD[i,y]~dlnorm(log(exp(aD-bD*flow[i,y]))-0.5/TmuD, TmuD)
    }
  }
  SD<-1/sqrt(TD)
  TmuD<-1/log(cvmuD*cvmuD+1)
  TD<-1/log(cvD*cvD+1)
  
  aD~dlnorm(0.52,14) # mu=1.75,cv=0.27
  bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
  cvmuD~dunif(0.001,1)
  cvD~dunif(0.001,2)
  
  # Proportion departing in each day  
  # ========================================
  for(y in 1:nYears){
    p2[1,y]<-p[1,y]
    for(i in 2:(nDays-1)){
      p2[i,y]<-(1-sum(p2[1:(i-1),y]))*p[i,y]
    }
    p2[nDays,y]<-1-sum(p2[1:60,y])
  
    # Joint distribution of qD and p2
    for(i in 1:nDays){ # day of departure
      for(j in i:(i+13)){ # day of passing the video site
        qtmp[j,i,y]<-qD[i,j,y]*p2[i,y]
      }
    }
  
    # Expected proportion of smolts passing the video site each day
    for(j in 1:14){
      muqN[j,y]<-sum(qtmp[j,1:j,y])+0.0001
    }
    for(j in 15:nDays){
      muqN[j,y]<-sum(qtmp[j,(j-13):j,y])+0.0001
    }
  }
  eta_alphaN~dunif(0.001,100000)

  aP~dnorm(-20,1) #mu=-20
  bP~dlnorm(0.6,10) #mu=1.91
  sdP~dlnorm(0,1) #mu=1.6

  
}"

modelName<-"Smolts_etaB_sdP_extracovs_Priors"
#modelName<-"Smolts_etaB_wideB"
#modelName<-"Smolts_etaStarB_s" # school size ==0.001 when Nobs==0


Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

# Years to include
#years<-c(2005:2009,2011,2013:2014)  
years<-c(2002:2021)  

# Number of days to include
n_days<-61 # june & july

# Select data
# =================================

load("01-Data/dat0221.RData")

df<-s_dat_jags(dat,years, n_days) # 61: only june & july

dataName<-"all"


# Computer name
compName<-"Dell"

# 
# data<-list(
#  #  s=df$Schools,
#   flow=df$Flow,
#   Nobs=df$Smolts,                     
#   Temp=df$Temp,
#   nDays=n_days,
#   nYears=length(years)
# )

data<-list(
  nYears=length(years),
  nDays = n_days,
  #s=df$Schools,
  flow=df$Flow,
#  Nobs=df$Smolts,
  #Nobs_side=df$side,
  Temp = df$Temp,
  Temp_air = df$Temp_air,
  Rain = df$Rain,
  Rain_bf = df$Rain_bf
)



inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
            list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))


var_names<-c(
  "a_temp", "b_temp", "a_fl", "b_fl", 
  "mu_phi_temp","eta_phi_temp",
  "mu_phi_fl","eta_phi_fl",
  #"muT", "cvT", "muF", "cvF",
  "aD","bD","cvD","cvmuD",
  "aP","bP","sdP",
  "aB","bB","sdBB",
  "etaB",
  # "K","slope","cvS", 
  # "sums1","sums2",
  "Ntot","N","eta_alphaN"
)


# 
# run0 <- run.jags(M1,
#                  monitor= var_names,data=data,inits = inits,
#                  n.chains = 2, method = 'parallel',
#                  thin=1, burnin =0,
#                  modules = "mix",
#                  keep.jags.files=F,sample =1000, adapt=100,
#                  progress.bar=TRUE)
# 

t1<-Sys.time();t1
run1 <- run.jags(M1,
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 7d
run<-run1

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=1000, thin=10, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#3.3d?
run<-run2


plot(run, var="temp")
plot(run, var="fl")

save(run, file=str_c(pathOut,modelName,"_run_",compName,".RData"))

