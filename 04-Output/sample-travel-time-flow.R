#################################
# Travel time to video at chosen flow (draw-graphs.r)

# pick chains for comparison
c1<-chains[[1]]
#c2<-chains[[2]]
c2<-chainsP[[1]]

# Posterior
n_samp<-length(c1[,"aD"])
aD_samp<-c1[,"aD"]
bD_samp<-c1[,"bD"]
cvmuD_samp<-c1[,"cvmuD"]
cvD_samp<-c1[,"cvD"]

muD_samp<-array(NA, dim=c(n_samp))
mumuD_samp<-array(NA, dim=c(n_samp))
MmuD_samp<-array(NA, dim=c(n_samp))
SDmuD_samp<-array(NA, dim=c(n_samp))
SDD_samp<-array(NA, dim=c(n_samp))
MD_samp<-array(NA, dim=c(n_samp))
qDx_samp<-array(NA, dim=c(n_samp,14))
qD_samp<-array(NA, dim=c(n_samp,14))
qD_cumul_samp<-array(NA, dim=c(n_samp,14))

for(j in 1:n_samp){
  SDD_samp[j]<-sqrt(log(cvD_samp[j]^2+1))
  SDmuD_samp[j]<-sqrt(log(cvmuD_samp[j]^2+1))
  MmuD_samp[j]<-log(exp(aD_samp[j]-bD_samp[j]*FLOW))-0.5*(SDmuD_samp[j]^2)
  muD_samp[j]<-rlnorm(1,MmuD_samp[j], SDmuD_samp[j])

  MD_samp[j]<-log(muD_samp[j])-0.5*SDD_samp[j]^2  

  qDx_samp[j,1]<-pnorm((log(0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  for(k in 2:14){
    qDx_samp[j,k]<-pnorm((log((k-1)+0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)-
      pnorm((log((k-1)-0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  }

  for(k in 1:14){
    qD_samp[j,k]<-qDx_samp[j,k]/(sum(qDx_samp[j, 1:14])+0.0001)
    qD_cumul_samp[j,k]<-sum(qD_samp[j,1:k])
    
  }
}


# Prior
n_samp<-length(c2[,"aD"])
aD_samp<-c2[,"aD"]
bD_samp<-c2[,"bD"]
cvmuD_samp<-c2[,"cvmuD"]
cvD_samp<-c2[,"cvD"]

muD_samp<-array(NA, dim=c(n_samp))
mumuD_samp<-array(NA, dim=c(n_samp))
MmuD_samp<-array(NA, dim=c(n_samp))
SDmuD_samp<-array(NA, dim=c(n_samp))
SDD_samp<-array(NA, dim=c(n_samp))
MD_samp<-array(NA, dim=c(n_samp))
qDx_samp<-array(NA, dim=c(n_samp,14))
qD_sampP<-array(NA, dim=c(n_samp,14))
qD_cumul_sampP<-array(NA, dim=c(n_samp,14))

for(j in 1:n_samp){
  SDD_samp[j]<-sqrt(log(cvD_samp[j]^2+1))
  SDmuD_samp[j]<-sqrt(log(cvmuD_samp[j]^2+1))
  MmuD_samp[j]<-log(exp(aD_samp[j]-bD_samp[j]*FLOW))-0.5*(SDmuD_samp[j]^2)
  muD_samp[j]<-rlnorm(1,MmuD_samp[j], SDmuD_samp[j])
  
  MD_samp[j]<-log(muD_samp[j])-0.5*SDD_samp[j]^2  
  
  qDx_samp[j,1]<-pnorm((log(0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  for(k in 2:14){
    qDx_samp[j,k]<-pnorm((log((k-1)+0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)-
      pnorm((log((k-1)-0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  }
  
  for(k in 1:14){
    qD_sampP[j,k]<-qDx_samp[j,k]/(sum(qDx_samp[j, 1:14])+0.0001)
    qD_cumul_sampP[j,k]<-sum(qD_sampP[j,1:k])
  }
}

#par(mfrow=c(4,4))
#for(i in 1:14){
#x<-mcmc.list(as.mcmc(qD_samp[,i]),as.mcmc(qD_sampP[,i]))
#traceplot(x, main=str_c("qD_samp ",i))
#}
