#################################
# Observation probability vs flow

Flow<-seq(0,100, by=5)
nF<-length(Flow)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]

# Posterior

n_samp<-length(c1[,"aB_mid"])
aB_samp<-c1[,"aB_mid"]
bB_samp<-c1[,"bB_mid"]
sdBB_samp<-c1[,"sdBB_mid"]

BB_samp<-array(NA, dim=c(n_samp,nF))
muB_samp<-array(NA, dim=c(n_samp,nF))
for(j in 1:n_samp){
  for(i in 1:nF){
    BB_samp[j,i]<-rnorm(1,aB_samp[j]-bB_samp[j]*Flow[i], sdBB_samp[j])
    muB_samp[j,i]<-0.6*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+0.3
  }
}

# Prior

n_samp<-length(c2[,"aB_mid"])
aB_samp<-c2[,"aB_mid"]
bB_samp<-c2[,"bB_mid"]
sdBB_samp<-c2[,"sdBB_mid"]

BB_samp<-array(NA, dim=c(n_samp,nF))
muB_sampP<-array(NA, dim=c(n_samp,nF))
for(j in 1:n_samp){
  for(i in 1:nF){
    BB_samp[j,i]<-rnorm(1,aB_samp[j]-bB_samp[j]*Flow[i], sdBB_samp[j])
    muB_sampP[j,i]<-0.6*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+0.3
  }
}

#aB_samp-bB_samp*10
#aB_samp-bB_samp*50
