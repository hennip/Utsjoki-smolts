#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]  13334   942 # dimensions: iterations x number of variables

headtext<-c("Varname","mean","sd","cv","5%","50%","95%","psrf")
statsfile<-str_c(pathOut,"stats_",ModelName,".csv")

write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)

for(i in 1:dim(d)[2]){ # loop over all monitored variables
  m<-mean(d[,i])
  s<-sd(d[,i])
  cv<-s/m
  q5<-quantile(d[,i],0.05)
  q50<-quantile(d[,i],0.50)
  q95<-quantile(d[,i],0.95)
  grd<-gelman.diag(chains[,i])$psrf[1]
  
  printtxt<-c(colnames(d)[i],m,s,cv,q5,q50,q95,grd)
  write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
}
