# Temp vs. prob to start migration
######################################
Temp<-seq(1,20, by=1)
nT<-length(Temp)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]
# choose
#pick<-1 # just expected value
pick<-2 # sd too

# Posterior
n_samp<-length(c1[,"aP"])
aP_samp<-c1[,"aP"]
bP_samp<-c1[,"bP"]
sdP_samp<-c1[,"sdP"]

muP_samp<-array(NA, dim=c(nT,n_samp))
P_samp<-array(NA, dim=c(nT,n_samp))
p_samp<-array(NA, dim=c(n_samp,nT))
for(j in 1:n_samp){
  for(i in 1:nT){
    muP_samp[i,j]<-aP_samp[j]+bP_samp[j]*Temp[i]
    P_samp[i,j]<-rnorm(1,muP_samp[i,j],sdP_samp[j])
    if(pick==2){p_samp[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))}
    if(pick==1){p_samp[j,i]<-exp(muP_samp[i,j])/(1+exp(muP_samp[i,j]))}
  }
}

# Prior
n_samp<-length(c2[,"aP"])
aP_samp<-c2[,"aP"]
bP_samp<-c2[,"bP"]
sdP_samp<-c2[,"sdP"]

muP_samp<-array(NA, dim=c(nT,n_samp))
P_samp<-array(NA, dim=c(nT,n_samp))
p_sampP<-array(NA, dim=c(n_samp,nT))
for(j in 1:n_samp){
  for(i in 1:nT){
    muP_samp[i,j]<-aP_samp[j]+bP_samp[j]*Temp[i]
    P_samp[i,j]<-rnorm(1,muP_samp[i,j],sdP_samp[j])
    if(pick==2){p_sampP[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))}
    if(pick==1){p_sampP[j,i]<-exp(muP_samp[i,j])/(1+exp(muP_samp[i,j]))}
  }
}
