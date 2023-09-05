
source("00-Functions/packages-and-paths.R")

RunPost<-T # T for posteriors, F for priors only (covariate data is included) 


# Select model
# =================================
source("03-Model/model-arrival-sides-coef.R")
#source("/home/henni/Utsjoki-smolts/03-Model/model-arrival-sides.R")


# Select data
# =================================
df<-readRDS("01-Data/df0221.RDS")
summary(df)
#df<-readRDS("/home/henni/Utsjoki-smolts/01-Data/df0221.RDS")


#View(dat)
years<-c(2002:2021)
n_days<-61

dataName<-"0221"
compName<-"dell"


if(RunPost==T){
  data<-list(
    nYears=length(years),
    nDays = n_days,
    #s=df$Schools,
    flow=df$Flow,
    Nobs_mid=df$Smolts,
    Nobs_east=df$side_east,
    Nobs_west=df$side_west,
    Temp = df$Temp,
    Temp_air_MA = df$Temp_air_sum/30,
    Rain = df$Rain,
    Rain_bf = df$Rain_bf
  )
  modelName<-"Smolts_sides"
}

if(RunPost==F){
  data<-list(
    nYears=length(years),
    nDays = n_days,
    #s=df$Schools,
    flow=df$Flow,
    #Nobs_mid=df$Smolts,
    #Nobs_east=df$side_east,
    #Nobs_west=df$side_west,
    Temp = df$Temp,
    Temp_air_MA = df$Temp_air_sum/30,
    Rain = df$Rain,
    Rain_bf = df$Rain_bf
  )
  modelName<-"Smolts_sides_priors"
}


print(modelName)


inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
            list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))

var_names<-c(
  "coef_side","pref",
  "a_temp", "b_temp", "a_fl", "b_fl", 
  "mu_phi_temp", "eta_phi_temp","mu_phi_fl", "eta_phi_fl",
  "aD","bD","cvD","cvmuD",
  "aP","bP","sdP",
  "aB_mid","bB_mid","sdBB_mid","etaB",
  "aB_side","bB_side","sdBB_side",
  "a_rho","b_rho","sd_rho",
  # "K","slope","cvS", 
  "Ntot","N","eta_alphaN"
)

t1<-Sys.time();t1
run1 <- run.jags(M1,
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =300000,
                 modules = "mix",keep.jags.files=T,sample =100, adapt = 1000,
                 progress.bar=TRUE)
t2<-Sys.time()
print("========================================================================")
print("Run 1 (burn-in) finished")
print(difftime(t2,t1))
print("========================================================================")
# 1.3d
run<-run1
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=1000, thin=300, keep.jags.files=T)
t2<-Sys.time()
print("========================================================================")
print("Run 2 finished")
print(difftime(t2,t1))
print("========================================================================")
#19h
run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=T, sample=1000, thin=300, keep.jags.files=T)
t2<-Sys.time()
print("========================================================================")
print("Run 3 finished")
print(difftime(t2,t1))
print("========================================================================")
#17h
run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=T, sample=2000, thin=300, keep.jags.files=T)
t2<-Sys.time()
print("========================================================================")
print("Run 4 finished")
print(difftime(t2,t1))
print("========================================================================")
run<-run4
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time();t1
run5 <- extend.jags(run4, combine=T, sample=2000, thin=300, keep.jags.files=T)
t2<-Sys.time()
print("========================================================================")
print("Run 5 finished")
print(difftime(t2,t1))
print("========================================================================")
run<-run5
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))

