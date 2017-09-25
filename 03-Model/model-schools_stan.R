# This is a small arrival model for complete arrival data to include
# school size as a determining factor to variation of observation process
# small set on known data, small (expected) observation process. 
# Two model versions for observation variance (overdispersion):
# 1) with minimally informative etaB, no school size (vrt 2002 paper)
# 2) with school size (and no etaB-component)
# What is the difference for evaluated uncertainty?

#source("00-Functions/packages-and-paths.r")
# library(rstan)

data<-list(
  s=df$Schools,
  flow=df$Flow,
  ones=ones,
  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)

M1<-"
data{
  int<lower=0> nYears; // number of years
  int<lower=0> nDays; // number of days
  vector[nDays] ones; // vector of ones

  real<lower=0> flow[nDays,nYears]; // flow
  int<lower=0> Nobs[nDays,nYears]; // daily number of smolts
  //real<lower=0> s[nDays,nYears]; // schools
  
}

parameters{
  real<lower=0> aB;
  real<lower=0> bB;
  real<lower=0> sdBB;
  real<lower=1> etaB;

  real<lower=7,upper=15>  LNtot[nYears];
  real  BB[nDays,nYears];
}

transformed parameters{
  real<lower=0> Ntot[nYears];
  real<lower=0.3, upper=0.6> muB[nDays,nYears];

  for(y in 1:nYears){
    Ntot[y]=exp(LNtot[y]);

    for(i in 1:nDays){
      muB[i,y]=0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3;
    }  
  }
}


model{
  int N[nDays,nYears];
  vector[nDays] qN[nYears];

  aB~lognormal(2.9,0.129);
  bB~lognormal(-2.6,0.139);
  sdBB~lognormal(-0.23,0.069);
  etaB~uniform(1,1000);

  for(y in 1:nYears){
    LNtot[y]~uniform(7,15);
    qN[y]~dirichlet(ones); 
  }

  for(y in 1:nYears){
    for(i in 1:nDays){
      N[i,y]=round(qN[i,y]*Ntot[y]);
      BB[i,y]~normal(aB-bB*flow[i,y],sdBB);
      Nobs[i,y]~beta_binomial(N[i,y],muB[i,y]*etaB,(1-muB[i,y])*etaB);  
      }
    }

}"
#modelName<-"Schools_etaStar"
modelName<-"Schools_etaB_stan"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)


# full data; temp data missing for 2012 and partly for 2010, note that the
# run was late in 2009
#years<-c(2005:2009,2011,2013:2014) 
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2006,2008)
n_days<-61
df<-smolts_data_to_jags(years, n_days) # 61: only june & july
ones<-rep(1,n_days)


data<-list(
#  s=df$Schools,
  flow=df$Flow,
  ones=ones,
  Nobs=df$Smolts,                     
  nDays=n_days,
  nYears=length(years)
)


system.time(fit <- stan(
  model_code = M1,
  data = data,
  iter = 100,
  chains = 2,
  warmup = 50,
  pars=c("Ntot"))
)



