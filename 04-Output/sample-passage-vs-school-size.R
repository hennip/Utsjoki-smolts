#################################
# Daily passage vs school size

tmp<-seq(0,8, by=0.4)
Ntrue<-exp(tmp)
#Ntrue<-seq(10,1000, by=50)
n<-length(Ntrue)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]

# Posterior

n_samp<-length(c1[,"slope"])
slope_samp<-c1[,"slope"]
K_samp<-c1[,"K"]
cvmuS_samp<-c1[,"cvmuS"]

mumuS_samp<-array(NA, dim=c(n_samp,n))
muS_samp<-array(NA, dim=c(n_samp,n))
SDmuS<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  for(i in 1:n){
    SDmuS[j]<-sqrt(log(cvmuS_samp[j]^2+1))
    mumuS_samp[j,i]<-(K_samp[j]*Ntrue[i])/((K_samp[j]/slope_samp[j])+Ntrue[i])
    muS_samp[j,i]<-rlnorm(1,log(mumuS_samp[j,i])-0.5*SDmuS[j]*SDmuS[j], SDmuS[j])
  }
}

# Prior

n_samp<-length(c2[,"slope"])
slope_samp<-c2[,"slope"]
K_samp<-c2[,"K"]
cvmuS_samp<-c2[,"cvmuS"]

mumuS_samp<-array(NA, dim=c(n_samp,n))
muS_sampP<-array(NA, dim=c(n_samp,n))
SDmuS<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  for(i in 1:n){
    SDmuS[j]<-sqrt(log(cvmuS_samp[j]^2+1))
    mumuS_samp[j,i]<-(K_samp[j]*Ntrue[i])/((K_samp[j]/slope_samp[j])+Ntrue[i])
    muS_sampP[j,i]<-rlnorm(1,log(mumuS_samp[j,i])-0.5*SDmuS[j]*SDmuS[j], SDmuS[j])
  }
}
