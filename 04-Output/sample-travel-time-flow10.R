#################################
# Travel time to video at flow = 10m3/s
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
cvD_samp<-c1[,"cvD"]

muD_samp<-array(NA, dim=c(n_samp))
mumuD_samp<-array(NA, dim=c(n_samp))
MmuD_samp<-array(NA, dim=c(n_samp))
SDmuD_samp<-array(NA, dim=c(n_samp))
SDD_samp<-array(NA, dim=c(n_samp))
MD_samp<-array(NA, dim=c(n_samp))
qDx_samp<-array(NA, dim=c(n_samp,14))
qD_samp<-array(NA, dim=c(n_samp,14))

for(j in 1:n_samp){
  SDD_samp[j]<-sqrt(log(cvD_samp[j]^2+1))
  SDmuD_samp[j]<-sqrt(log(cvmuD_samp[j]^2+1))
  MmuD_samp[j]<-log(exp(aD_samp[j]-bD_samp[j]*10))-0.5*(SDmuD_samp[j]^2)
  muD_samp[j]<-rlnorm(1,MmuD_samp[j], SDmuD_samp[j])

  MD_samp[j]<-log(muD_samp[j])-0.5*SDD_samp[j]^2  

  qDx_samp[j,1]<-pnorm((log(0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  for(k in 2:14){
    qDx_samp[j,k]<-pnorm((log((k-1)+0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)-
      pnorm((log((k-1)-0.5)-MD_samp[j])/SDD_samp[j],mean=0, sd=1)
  }

  for(k in 1:14){
    qD_samp[j,k]<-qDx_samp[j,k]/(sum(qDx_samp[j, 1:14])+0.0001)
  }
}


forthnight<-c(1:14)
df<-boxplot.df(qD_samp, forthnight)
sum(df[,4])

ggplot(df, aes(x))+
    geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Flow (m3/s)", y="E(travel time) (in days)", title="Travel time to video site")+
  geom_line(aes(x,q50))+
#  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()




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
