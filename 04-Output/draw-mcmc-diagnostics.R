
#source("04-Output/load-simulations.r")

#chains<-chains0
#years
 
#################################
# Densities

par(mfrow=c(4,3),mar=c(2.5,4,4,1))
plot(density(chains[,"aP"][[1]]), main="aP", xlim=c(-25,-15), ylim=c(0,0.5))
lines(density(chains[,"aP"][[2]]))
lines(density(chainsP[,"aP"][[1]]), lty=2)
plot(density(chains[,"bP"][[1]]), main="bP", xlim=c(0.5,2.5))
lines(density(chains[,"bP"][[2]]))
lines(density(chainsP[,"bP"][[1]]), lty=2)
plot(density(chains[,"sdP"][[1]]), main="sdP", xlim=c(0,4), ylim=c(0,1.2))
lines(density(chains[,"sdP"][[2]]))
lines(density(chainsP[,"sdP"][[1]]), lty=2)
plot(density(chains[,"eta_alphaN"][[1]]), main="eta_alphaN", ylim=c(0,0.01))
lines(density(chains[,"eta_alphaN"][[2]]))
lines(density(chainsP[,"eta_alphaN"][[1]]), lty=2)
plot(density(chains[,"aD"][[1]]), main="aD", ylim=c(0,4))
lines(density(chains[,"aD"][[2]]))
lines(density(chainsP[,"aD"][[1]]), lty=2)
plot(density(chains[,"bD"][[1]]), main="bD", ylim=c(0,250))
lines(density(chains[,"bD"][[2]]))
lines(density(chainsP[,"bD"][[1]]), lty=2)
plot(density(chains[,"aB"][[1]]), main="aB", ylim=c(0,4))
lines(density(chains[,"aB"][[2]]))
lines(density(chainsP[,"aB"][[1]]), lty=2)
plot(density(chains[,"bB"][[1]]), main="bB", ylim=c(0,200))
lines(density(chains[,"bB"][[2]]))
lines(density(chainsP[,"bB"][[1]]), lty=2)
plot(density(chains[,"sdBB"][[1]]), main="sdBB")
lines(density(chains[,"sdBB"][[2]]))
lines(density(chainsP[,"sdBB"][[1]]), lty=2)

plot(density(chains[,"etaB"][[1]]), main="etaB", ylim=c(0,0.0015))
lines(density(chains[,"etaB"][[2]]))
lines(density(chainsP[,"etaB"][[1]]), lty=2)
plot(density(chains[,"cvD"][[1]]), main="cvD", ylim=c(0,2))
lines(density(chains[,"cvD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)
plot(density(chains[,"cvmuD"][[1]]), main="cvmuD", ylim=c(0,4))
lines(density(chains[,"cvmuD"][[2]]))
lines(density(chainsP[,"cvmuD"][[1]]), lty=2)

plot(density(chains[,"K"][[1]]), main="K")
lines(density(chains[,"K"][[2]]))
lines(density(chainsP[,"K"][[1]]), lty=2)
plot(density(chains[,"slope"][[1]]), main="slope", xlim=c(0.1,0.27))
lines(density(chains[,"slope"][[2]]))
lines(density(chainsP[,"slope"][[1]]), lty=2)

plot(density(chains[,"cvS"][[1]]), main="cvS", ylim=c(0,4))
lines(density(chains[,"cvS"][[2]]))
lines(density(chainsP[,"cvS"][[1]]), lty=2)
plot(density(chains[,"cvmuS"][[1]]), main="cvmuS", ylim=c(0,8))
lines(density(chains[,"cvmuS"][[2]]))
lines(density(chainsP[,"cvmuS"][[1]]), lty=2)


# correlations
##################################################
aD_orig<-as.data.frame(chains[,"aD"][[1]])
bD_orig<-as.data.frame(chains[,"bD"][[1]])
cvD_orig<-as.data.frame(chains[,"cvD"][[1]])
cvmuD_orig<-as.data.frame(chains[,"cvmuD"][[1]])


par(mfrow=c(2,3))
plot(aD_orig[,1],bD_orig[,1], xlab="aD", ylab="bD", col="red")
#points(aD_samp,bD_samp)

plot(aD_orig[,1],cvD_orig[,1], xlab="aD", ylab="cvD", col="red")
#points(aD_samp,cvD_samp)

plot(aD_orig[,1],cvmuD_orig[,1], xlab="aD", ylab="cvmuD", col="red")
#points(aD_samp,cvmuD_samp)

plot(bD_orig[,1],cvD_orig[,1], xlab="bD", ylab="cvD", col="red")
#points(bD_samp,cvD_samp)

plot(bD_orig[,1],cvmuD_orig[,1], xlab="bD", ylab="cvmuD", col="red")
#points(bD_samp,cvmuD_samp)

plot(cvD_orig[,1],cvmuD_orig[,1], xlab="cvD", ylab="cvmuD", col="red")
#points(cvD_samp,cvmuD_samp)

##################################################


#windows(record=T)
par(mfrow=c(3,6))
for(i in 1:61){
  traceplot(chains[,paste(sep="","N[",i,",3]")],main=i, cex.main=1.5, col=c("black", "red"))
}  
tmp<-as.matrix(chains[,"N[1,3]"][[1]])
for(i in 2:23){
  tmp<-tmp+as.matrix(chains[,paste(sep="","N[",i,",3]")][[1]])
}  
traceplot(as.mcmc(tmp))
summary(as.mcmc(tmp), quantiles=c(0.05,0.25,0.5,0.75,0.95))
windows()
plot(density(tmp))



par(mfrow=c(3,6))
for(i in 1:61){
  traceplot(chains[,paste(sep="","qmu[",i,",3]")],main="qmu", cex.main=1.5, col=c("black", "red"))
}  

gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])
gelman.diag(chains[,"Ntot[6]"])


gelman.diag(chains[,"aP"])
gelman.plot(chains[,"aP"])

gelman.plot(chains[,"N"])



##########

# Summaries 

summary(chains[,"eta_alphaN"])
summary(chains[,"etaB"])
summary(chains[,"sum1"])
summary(chains[,"sum31"])

summary(chains[,"aB"])
summary(chains[,"bB"])

summary(chains[,"aP"])
summary(chains[,"bP"])
summary(chains[,"sdP"])

summary(chains[,"aD"])
summary(chains[,"bD"])
summary(chains[,"cvD"])
summary(chains[,"cvmuD"])

# joutohetkena taman voisi kirjoittaa funktioksi jossa vaihtoehdot 2 tai 3 -ulotteisille dimensioille
# ts. syotettaisiin sisaan muuttuja (ja nimi), laskisi suoraan tunnusluvut ja tallentaisi excelin oikealla nimella 
# saveStats

#saveStats<-function{}

qmu<-array(NA, dim=c(61,7))
for(i in 1:61){
  qmu[i,1:2]<-summary(chains[,paste(sep="","qmu[",i,",3]")])$statistics[1:2]
  qmu[i,3:7]<-summary(chains[,paste(sep="","qmu[",i,",3]")])$quantiles
}
colnames(qmu)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")
write.xlsx(qmu, "qmuStats_cv.xlsx")


N05<-array(NA, dim=c(61,7))
for(i in 1:61){
  N05[i,1:2]<-summary(chains[,paste(sep="","N[",i,",3]")])$statistics[1:2]
  N05[i,3:7]<-summary(chains[,paste(sep="","N[",i,",3]")])$quantiles
}
colnames(N05)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")
write.xlsx(N05, "N05Stats_cv.xlsx")



