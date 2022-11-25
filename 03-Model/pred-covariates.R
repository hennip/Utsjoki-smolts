library(runjags);library(rjags); library(tidyverse)
source("00-Functions/packages-and-paths.R")
source("01-Data/data-tempSumCalc.R")


#load("01-Data/dat0221.RData")
#summary(dat)

# length(dat$smolts)
# df<-dat%>%filter(Year>2016, Month<8)
# View(df)
# df

#View(dat)
years<-c(2002:2021)
n_days<-61


s_dat_jags <- function(dat, years, days){
  nYears <- length(years)
  nDays <- days
  #   filter data first
  #   needed variables: smolts, schools, flow, temp, temp_air, rain, rainbf
  dat_f <- dat %>% filter(Year %in% years, day <= days) %>% group_by(Year) %>% 
    group_split(.keep=F) %>% unlist(recursive = F) %>% as.data.frame() 
  
  data = list(
    nYears = nYears,
    nDays = nDays,
    Smolts = dat_f %>% select(matches("smolts\\.|smolts$")) %>% as.matrix(),
    Schools = dat_f %>% select(matches("schools\\.|schools$")) %>% as.matrix(),
    Flow = dat_f %>% select(matches("flow\\.|flow$")) %>% as.matrix(),
    Temp = dat_f %>% select(matches("meanTemp\\.|meanTemp$")) %>% as.matrix(),
    Temp_air = dat_f %>% select(matches("temp_air\\.|temp_air$")) %>% as.matrix(), 
    Temp_air_sum = dat_f %>% select(matches("tempSum\\.|tempSum$")) %>% as.matrix(), 
    Rain = dat_f %>% select(matches("rain\\.|rain$")) %>% as.matrix(),
    Rain_bf = dat_f %>% select(matches("rainbf\\.|rainbf$")) %>% as.matrix(),
    side = dat_f %>% select(matches("side\\.|side$")) %>% as.matrix()
  )
  return(data)
}




df<-s_dat_jags(dat_m,years, n_days) # 61: only june & july


par(mfrow=c(1,2))
plot(df$Temp, df$Temp_air_sum/30, ylim=c(0,25))
plot(df$Temp, df$Temp_air, ylim=c(0,25))


datalist<-list(
  nYears=length(years),
  nDays = n_days,
  flow=df$Flow,
#  Nobs=df$Smolts,
#  Nobs_side=df$side,
  Temp = df$Temp,
Temp_air_MA = df$Temp_air_sum/30,
#Temp_air = df$Temp_air,
  Rain = df$Rain,
  Rain_bf = df$Rain_bf
)

#initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
#               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))
M1<-"
model{
  
  # Predicting missing values of flow and water temperature using weatherdata
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      
      #   Water temperature is estimated from air temperature
      Temp[i,y] ~ dlnorm(log(mu_temp[i,y])-0.5*log(cv_temp[i,y]*cv_temp[i,y]+1), 1/log(cv_temp[i,y]*cv_temp[i,y]+1))
      mu_temp_r[i,y] = a_temp + b_temp[1]*Temp_air_MA[i,y]
      #   this is to make sure it stays positive
      mu_temp[i,y] =  ifelse(mu_temp_r[i,y]>=0.0001, mu_temp_r[i,y], 0.0001)
      
Temp_pred[i,y] ~ dlnorm(log(mu_temp[i,y])-0.5*log(cv_temp[i,y]*cv_temp[i,y]+1), 1/log(cv_temp[i,y]*cv_temp[i,y]+1))

      #   Flow is estimetetd using water temperature, rain and days since last rain 
       flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl[i,y]*cv_fl[i,y]+1), 1/log(cv_fl[i,y]*cv_fl[i,y]+1))
       mu_fl_r[i,y] =  a_fl + b_fl[1]*Temp[i,y] + b_fl[2]*Rain_bf[i,y] + b_fl[3]*Rain[i,y]
      #   this is to make sure it stays positive
       mu_fl[i,y] =  ifelse(mu_fl_r[i,y]>=0.0001, mu_fl_r[i,y], 0.0001)
       
       flow_pred[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl[i,y]*cv_fl[i,y]+1), 1/log(cv_fl[i,y]*cv_fl[i,y]+1))
       
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

      mu_phi_tempX~dbeta(2,2)
      eta_phi_tempX~dunif(1,1000) 
      mu_phi_flX~dbeta(2,2)
      eta_phi_flX~dunif(1,1000) 

a_tempX ~ dnorm(0, 100^-2)
  a_flX ~ dnorm(0, 100^-2)
  
  b_tempX[1] ~ dnorm(0, 100^-2)
  
  for(i in 1:3){
    b_flX[i] ~ dnorm(0, 100^-2)
  }

}"


par <- c("a_temp", "b_temp", "a_fl", "b_fl", "mu_phi_temp", "eta_phi_temp","mu_phi_fl", "eta_phi_fl",
         "a_tempX", "b_tempX", "a_flX", "b_flX","mu_phi_tempX", "eta_phi_tempX","mu_phi_flX", "eta_phi_flX",
         "Temp_pred", "flow_pred", "Temp", "flow")


print(Sys.time())
res <- run.jags(M1, data = datalist, monitor = par, sample = 1000,
                method = "parallel", n.chains = 2, thin = 300, keep.jags.files=F)#, inits = initials)
print(Sys.time())

saveRDS(res, paste0(pathMain,"output/utsjoki-smolts/pred_covariates.rds" ))

summary(res)
#plot(res)

run<-readRDS(paste0(pathMain,"output/utsjoki-smolts/pred_covariates.rds"))
chains<-as.mcmc.list(run)

#chains<-window(chains,start=10000)

# Diagnostics

summary(chains[,"a_temp"])
gelman.diag(chains[,"a_temp"])
summary(chains[,"b_temp"])
gelman.diag(chains[,"b_temp"])


traceplot(chains[,"a_temp"])
traceplot(chains[,"b_temp"])
traceplot(chains[,"a_fl"])
traceplot(chains[,"b_fl[1]"])
traceplot(chains[,"b_fl[2]"])
traceplot(chains[,"b_fl[3]"])
traceplot(chains[,"mu_phi_temp"])
traceplot(chains[,"eta_phi_temp"])
traceplot(chains[,"mu_phi_fl"])
traceplot(chains[,"eta_phi_fl"])


  chains2<-combine.mcmc(chains)

par(mfrow=c(3,4))
plot(density(chains2[,"a_temp"]), main="a_temp", xlab="")
lines(density(chains2[,"a_tempX"]), col="red")
plot(density(chains2[,"b_temp"]), main="b_temp", xlab="")
lines(density(chains2[,"b_tempX"]), col="red")
plot(density(chains2[,"a_fl"]), main="a_fl", xlab="")
lines(density(chains2[,"a_flX"]), col="red")
plot(density(chains2[,"b_fl[1]"]), main="b_fl1", xlab="")
lines(density(chains2[,"b_flX[1]"]), col="red")
plot(density(chains2[,"b_fl[2]"]), main="b_fl2", xlab="")
lines(density(chains2[,"b_flX[2]"]), col="red")
plot(density(chains2[,"b_fl[3]"]), main="b_fl3", xlab="")
lines(density(chains2[,"b_flX[3]"]), col="red")
plot(density(chains2[,"mu_phi_fl"]), main="mu_phi_fl", xlab="")
lines(density(chains2[,"mu_phi_flX"]), col="red")
plot(density(chains2[,"eta_phi_fl"]), main="eta_phi_fl", xlab="")
lines(density(chains2[,"eta_phi_flX"]), col="red")
plot(density(chains2[,"mu_phi_temp"]), main="mu_phi_temp", xlab="")
lines(density(chains2[,"mu_phi_tempX"]), col="red")
plot(density(chains2[,"eta_phi_temp"]), main="eta_phi_temp", xlab="")
lines(density(chains2[,"eta_phi_tempX"]), col="red")


# Compare water temperature data with model predicted

datalist$Temp[,2]
dim(datalist$Temp)

quants<-array(NA, dim=c(61, 19, 3))
for(y in 1:19){
  for(i in 1:61){
  quants[i,y,]<-summary(chains2[,paste0("Temp_pred[",i,",",y,"]")], quantiles = c(0.05,0.5,0.95), na.rm=T)$quantiles
  }
}

par(mfrow=c(3,4))
for(y in 1:19){
  plot(1:61,quants[,y,1], type = "l", ylim=c(0,25),  main=(2001+y), ylab="Water temp")
  lines(1:61,quants[,y,2])
  lines(1:61,quants[,y,3])
  points(1:61,datalist$Temp[,y], col="red")
}


# Compare flow data with model predicted

datalist$flow[,2]
dim(datalist$flow)

quants<-array(NA, dim=c(61, 19, 3))
for(y in 1:19){
  for(i in 1:61){
    quants[i,y,]<-summary(chains2[,paste0("flow_pred[",i,",",y,"]")], quantiles = c(0.05,0.5,0.95), na.rm=T)$quantiles
  }
}

par(mfrow=c(3,4))
for(y in 1:19){
  plot(1:61,quants[,y,1], type = "l", ylim=c(0,100),  main=(2001+y), ylab="Flow")
  lines(1:61,quants[,y,2])
  lines(1:61,quants[,y,3])
  points(1:61,datalist$flow[,y], col="red")
}


#run<-readRDS(paste0(pathMain,"output/utsjoki-smolts/est_covariates.rds"))
#chains<-as.mcmc.list(run)
#chains2<-combine.mcmc(chains)

# Compare water temperature data with model estimates


quants<-array(NA, dim=c(61, 19, 3))
for(y in 1:19){
  for(i in 1:61){
    quants[i,y,]<-summary(chains2[,paste0("Temp[",i,",",y,"]")], quantiles = c(0.05,0.5,0.95), na.rm=T)$quantiles
  }
}

par(mfrow=c(4,5))
for(y in 1:19){
  plot(1:61,quants[,y,1], type = "l", ylim=c(0,25),  main=(2001+y), ylab="Water temp")
  lines(1:61,quants[,y,2])
  lines(1:61,quants[,y,3])
  points(1:61,datalist$Temp[,y], col="red")
}


# Compare flow data with model estimates

quants<-array(NA, dim=c(61, 19, 3))
for(y in 1:19){
  for(i in 1:61){
    quants[i,y,]<-summary(chains2[,paste0("flow[",i,",",y,"]")], quantiles = c(0.05,0.5,0.95), na.rm=T)$quantiles
  }
}

par(mfrow=c(4,5))
for(y in 1:19){
  plot(1:61,quants[,y,1], type = "l", ylim=c(0,120),  main=(2001+y), ylab="Flow")
  lines(1:61,quants[,y,2])
  lines(1:61,quants[,y,3])
  points(1:61,datalist$flow[,y], col="red")
}

