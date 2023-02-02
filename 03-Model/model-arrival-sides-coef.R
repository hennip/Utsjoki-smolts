


M1<-"
model{

  # Predicting missing values of flow and water temperature using weather data
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      
      #   Water temperature is estimated using 30 days moving average air temperature
      Temp[i,y] ~ dlnorm(log(mu_temp[i,y])-0.5*log(cv_temp[i,y]*cv_temp[i,y]+1), 1/log(cv_temp[i,y]*cv_temp[i,y]+1))
      mu_temp_r[i,y] = a_temp + b_temp*Temp_air_MA[i,y]
      mu_temp[i,y] =  ifelse(mu_temp_r[i,y]>=0.0001, mu_temp_r[i,y], 0.0001)      #   this is to make sure it stays positive

      #   Flow is estimetetd using water temperature, rain and days since last rain 
      flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl[i,y]*cv_fl[i,y]+1), 1/log(cv_fl[i,y]*cv_fl[i,y]+1))
      mu_fl_r[i,y] =  a_fl + b_fl[1]*Temp[i,y] + b_fl[2]*Rain_bf[i,y] + b_fl[3]*Rain[i,y]
      mu_fl[i,y] =  ifelse(mu_fl_r[i,y]>=0.0001, mu_fl_r[i,y], 0.0001)      #   this is to make sure it stays positive

    }
  }
  #   uninformative priors
  a_temp ~ dnorm(0, 100^-2)
  b_temp ~ dnorm(0, 100^-2)
  a_fl ~ dnorm(0, 100^-2)
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
    
      # Observed number of fish in the middle of the river
      Nobs_mid[i,y]~dbin(p_obs_mid[i,y]*rho[i,y],N[i,y])  
      p_obs_mid[i,y]~dbeta(muB_mid[i,y]*etaB, (1-muB_mid[i,y])*etaB)
      muB_mid[i,y]<-0.6*(exp(BB_mid[i,y])/(1+exp(BB_mid[i,y])))+0.3
      BB_mid[i,y]~dnorm(aB_mid-bB_mid*flow[i,y],1/pow(sdBB_mid,2))

      # Observed number of fish at sides
      # Eastern side (data from 2020) is slightly more preferred a priori
      Nobs_east[i,y]~dbin(p_obs_side[i,y]*(1-rho[i,y])*pref,N[i,y])  
      
      # Western side (data from 2004) is slightly less preferred a priori
      Nobs_west[i,y]~dbin(p_obs_side[i,y]*(1-rho[i,y])*(1-pref),N[i,y])  
      
      # Probability to be observed at given flow at either side
      p_obs_side[i,y]~dbeta(muB_side[i,y]*etaB, (1-muB_side[i,y])*etaB)
      muB_side[i,y]<-0.5*(exp(BB_side[i,y])/(1+exp(BB_side[i,y])))+0.45
      BB_side[i,y]~dnorm(aB_side-bB_side*flow[i,y],1/pow(sdBB_side,2))

      # Proportion that passes cameras in the middle of the river  
      rho[i,y]~dbeta(mu_rho[i,y]*etaB, (1-mu_rho[i,y])*etaB)T(0.001,0.999)
      mu_rho[i,y]<-0.5*(exp(rhoe[i,y])/(1+exp(rhoe[i,y])))+0.5
      rhoe[i,y]~dnorm(a_rho-b_rho*flow[i,y],1/(pow(sd_rho,2)))

    }
  }
  # priors for observation process
  # wide priors 
#  aB_mid~dnorm(2.9,1)
#  bB_mid~dlnorm(-2.6,1)
  #aB_mid~dnorm(2.9,60)
  #bB_mid~dlnorm(-2.6,984)
#  sdBB_mid~dlnorm(-0.23,210)

aB_mid~dnorm(2.9,3)
bB_mid~dlnorm(-2.6,50)
sdBB_mid~dlnorm(-0.23,1)


  aB_side<-aB_mid*1.5#(coef_side+1)#~dnorm(5.63,86)
  bB_side<-bB_mid#~dlnorm(-1.88,6073)
  sdBB_side<-sdBB_mid#~dlnorm(-0.59,2.04)

  # rho: proportion of smolts passing site at mid river
  a_rho~dnorm(3.86,1)#~dnorm(3.86,47.5)
  b_rho~dlnorm(-2.59,10)#~dlnorm(-2.59,798)
  sd_rho~dlnorm(-0.5,1)#~dlnorm(0.67,1076)

  # Preferability of eastern side (1-pref for western side)
  pref ~ dbeta(50,40)

  # overdisperison in beta-binomial rho & obs prop due to overdispersion
  etaB~dunif(5,1000)

 # coef_side~dbeta(2,2) # for aB_side


  # Abundance
  # ==============
  for(y in 1:nYears){
    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y

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
      #P[i,y]~dnorm(aP+bP*Temp[i,y],1000)
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




