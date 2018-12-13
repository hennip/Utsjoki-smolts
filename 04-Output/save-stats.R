#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]  13334   942 # dimensions: iterations x number of variables

#ModelName<-"Smolts_etaStarB_s_0714_run_turd010"

headtext<-c("Varname","mean","sd","cv","5%","50%","95%","grdPE", "grdUCI")
statsfile<-str_c(pathOut,"stats_",ModelName,".csv")

write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)

for(i in 1:dim(d)[2]){ # loop over all monitored variables
  m<-mean(d[,i])
  s<-sd(d[,i])
  cv<-s/m
  q5<-quantile(d[,i],0.05)
  q50<-quantile(d[,i],0.50)
  q95<-quantile(d[,i],0.95)
  grdPE<-gelman.diag(chains[,i])$psrf[1]
  grdUCI<-gelman.diag(chains[,i])$psrf[2]
  
  printtxt<-c(colnames(d)[i],m,s,cv,q5,q50,q95,grdPE, grdUCI)
  write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
}
