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
cvS_samp<-c1[,"cvS"]

S_samp<-array(NA, dim=c(n_samp,n))
muS_samp<-array(NA, dim=c(n_samp,n))
SDS<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  for(i in 1:n){
    SDS[j]<-sqrt(log(cvS_samp[j]^2+1))
    muS_samp[j,i]<-(K_samp[j]*Ntrue[i])/((K_samp[j]/slope_samp[j])+Ntrue[i])
    S_samp[j,i]<-rlnorm(1,log(muS_samp[j,i])-0.5*SDS[j]*SDS[j], SDS[j])
  }
}

# Prior
n_samp<-length(c2[,"slope"])
slope_sampP<-c2[,"slope"]
K_sampP<-c2[,"K"]
cvS_sampP<-c2[,"cvS"]

S_samp<-array(NA, dim=c(n_samp,n))
muS_samp<-array(NA, dim=c(n_samp,n))
SDS<-array(NA, dim=c(n_samp))
for(j in 1:n_samp){
  for(i in 1:n){
    SDS[j]<-sqrt(log(cvS_sampP[j]^2+1))
    muS_samp[j,i]<-(K_sampP[j]*Ntrue[i])/((K_sampP[j]/slope_sampP[j])+Ntrue[i])
    S_sampP[j,i]<-rlnorm(1,log(muS_samp[j,i])-0.5*SDS[j]*SDS[j], SDS[j])
  }
}


