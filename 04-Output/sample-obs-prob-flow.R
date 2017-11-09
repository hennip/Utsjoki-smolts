#################################
# Observation probability vs flow

Flow<-seq(-10,100, by=5)
nF<-length(Flow)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]

# Posterior

n_samp<-length(c1[,"aB"])
aB_samp<-c1[,"aB"]
bB_samp<-c1[,"bB"]
sdBB_samp<-c1[,"sdBB"]

BB_samp<-array(NA, dim=c(n_samp,nF))
muB_samp<-array(NA, dim=c(n_samp,nF))
for(j in 1:n_samp){
  for(i in 1:nF){
    BB_samp[j,i]<-rnorm(1,aB_samp[j]-bB_samp[j]*Flow[i], sdBB_samp[j])
    muB_samp[j,i]<-0.6*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+0.3
  }
}

# Prior

n_samp<-length(c2[,"aB"])
aB_samp<-c2[,"aB"]
bB_samp<-c2[,"bB"]
sdBB_samp<-c2[,"sdBB"]

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
