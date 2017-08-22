#################################
# Travel time to video vs flow
Flow<-seq(10,150, by=10)
nF<-length(Flow)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]

# Posterior
n_samp<-length(c1[,"aD"])
aD_samp<-c1[,"aD"]
bD_samp<-c1[,"bD"]
cvmuD_samp<-c1[,"cvmuD"]

muD_samp<-array(NA, dim=c(n_samp,nF))
mumuD_samp<-array(NA, dim=c(n_samp,nF))
MmuD_samp<-array(NA, dim=c(n_samp,nF))
SDmuD_samp<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  SDmuD_samp[j]<-sqrt(log(cvmuD_samp[j]^2+1))
  for(i in 1:nF){
    MmuD_samp[j,i]<-log(exp(aD_samp[j]-bD_samp[j]*Flow[i]))-0.5*(SDmuD_samp[j]^2)
    muD_samp[j,i]<-rlnorm(1,MmuD_samp[j,i], SDmuD_samp[j])
  }
}

# Prior
n_samp<-length(c2[,"aD"])
aD_samp<-c2[,"aD"]
bD_samp<-c2[,"bD"]
cvmuD_samp<-c2[,"cvmuD"]

muD_sampP<-array(NA, dim=c(n_samp,nF))
mumuD_samp<-array(NA, dim=c(n_samp,nF))
MmuD_samp<-array(NA, dim=c(n_samp,nF))
SDmuD_samp<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  SDmuD_samp[j]<-sqrt(log(cvmuD_samp[j]^2+1))
  for(i in 1:nF){
    MmuD_samp[j,i]<-log(exp(aD_samp[j]-bD_samp[j]*Flow[i]))-0.5*(SDmuD_samp[j]^2)
    muD_sampP[j,i]<-rlnorm(1,MmuD_samp[j,i], SDmuD_samp[j])
  }
}
