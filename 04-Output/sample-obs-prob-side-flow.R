#################################
# Observation probability at sides vs flow

# First call obs prop at middle

#df<-p_vs_flow(chains, "aB_mid", "bB_mid", "sdBB_mid", 0.6, 0.3)
#df.prior<-p_vs_flow(chainsP, "aB_mid", "bB_mid", "sdBB_mid", 0.6, 0.3)

a<-"aB_mid"  
b<-"bB_mid"  
sdX<-"sdBB_mid"  
coef<-"coef_side"  

upr<-0.6
lwr<-0.3

k1_coef<-0.444
k2_coef<-1.056
#  coef_side*0.444+1.056


Flow<-seq(0,100, by=2)
nF<-length(Flow)

c1<-chains[[1]]
c2<-chainsP[[1]]

# Posterior

n_samp<-length(c1[,a])
a_samp<-c1[,a]
b_samp<-c1[,b]
sd_samp<-c1[,sdX]
coef_samp<-c1[,coef]

BB_samp<-array(NA, dim=c(n_samp,nF))
muB_samp<-array(NA, dim=c(n_samp,nF))
muB_side_samp<-array(NA, dim=c(n_samp,nF))
for(j in 1:n_samp){
  for(i in 1:nF){
    BB_samp[j,i]<-rnorm(1,a_samp[j]-b_samp[j]*Flow[i], sd_samp[j])
    muB_samp[j,i]<-upr*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+lwr
    muB_side_samp[j,i]<-muB_samp[j,i]*(coef_samp[j]*k1_coef+k2_coef)
  }
}

df<-boxplot.df(muB_side_samp, Flow)










