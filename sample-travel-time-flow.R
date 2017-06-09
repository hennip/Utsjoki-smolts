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
sdmuD_samp<-array(NA, dim=c(n_samp,nF))
#cvmuD<-0.1
for(j in 1:n_samp){
  for(i in 1:nF){
    mumuD_samp[j,i]<-exp(aD_samp[j]-bD_samp[j]*Flow[i])
    sdmuD_samp[j,i]<-mumuD_samp[j,i]*cvmuD_samp[j]
    muD_samp[j,i]<-rnorm(1,mumuD_samp[j,i], sdmuD_samp[j,i])
    
  }
}
#colnames(muD_samp)<-paste(sep="", "muD[",1:15,"]")

# Prior
n_samp<-length(c2[,"aD"])
aD_samp<-c2[,"aD"]
bD_samp<-c2[,"bD"]
cvmuD_samp<-c2[,"cvmuD"]

muD_sampP<-array(NA, dim=c(n_samp,nF))
mumuD_samp<-array(NA, dim=c(n_samp,nF))
sdmuD_samp<-array(NA, dim=c(n_samp,nF))
#cvmuD<-0.1
for(j in 1:n_samp){
  for(i in 1:nF){
    mumuD_samp[j,i]<-exp(aD_samp[j]-bD_samp[j]*Flow[i])
    sdmuD_samp[j,i]<-mumuD_samp[j,i]*cvmuD_samp[j]
    muD_sampP[j,i]<-rnorm(1,mumuD_samp[j,i], sdmuD_samp[j,i])
  }
}
