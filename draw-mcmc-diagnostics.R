source("tidy-functions.r")
source("tidy-smolts-data.r")
source("my-palette.r")
source("packages-and-paths.r")


source("load-simulations.r")


# Traces
par(mfrow=c(3,4),mar=c(2.5,4,4,1))
traceplot(chains[,"Ntot[1]"],main="Ntot1", cex.main=1.5)
traceplot(chains[,"Ntot[2]"],main="Ntot2", cex.main=1.5)
traceplot(chains[,"Ntot[3]"],main="Ntot3", cex.main=1.5)
traceplot(chains[,"Ntot[4]"],main="Ntot4", cex.main=1.5)
traceplot(chains[,"Ntot[5]"],main="Ntot5", cex.main=1.5)
traceplot(chains[,"Ntot[6]"],main="Ntot6", cex.main=1.5)

traceplot(chains[,"aP"],main=expression("aP"), cex.main=1.5)
traceplot(chains[,"bP"],main=expression("bP"), cex.main=1.5)
traceplot(chains[,"sdP"],main=expression("sdP"), cex.main=1.5)
traceplot(chains[,"aD"],main=expression("aD"), cex.main=1.5)
traceplot(chains[,"bD"],main=expression("bD"), cex.main=1.5)
traceplot(chains[,"cvD"],main=expression("cvD"), cex.main=1.5)

traceplot(chains[,"cvmuD"],main=expression("cvmuD"), cex.main=1.5)
traceplot(chains[,"aB"],main=expression("aB"), cex.main=1.5)
traceplot(chains[,"bB"],main=expression("bB"), cex.main=1.5)
traceplot(chains[,"sdBB"],main=expression("sdBB"), cex.main=1.5)
traceplot(chains[,"etaB"],main=expression("etaB"), cex.main=1.5)
traceplot(chains[,"eta_alphaN"],main=expression("eta_alphaN"), cex.main=1.5)

traceplot(chains[,"sum1"])
traceplot(chains[,"sum31"])
summary(chains[,"sum1"])
summary(chains[,"sum31"])

#################################
# Densities

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot(density(chains[,"aP"][[1]]), main="aP", xlim=c(-25,-10))
lines(density(chains[,"aP"][[2]]))
lines(density(chainsP[,"aP"][[1]]), lty=2)
plot(density(chains[,"bP"][[1]]), main="bP", xlim=c(0,2))
lines(density(chains[,"bP"][[2]]))
lines(density(chainsP[,"bP"][[1]]), lty=2)
plot(density(chains[,"sdP"][[1]]), main="sdP", xlim=c(1,5))
lines(density(chains[,"sdP"][[2]]))
lines(density(chainsP[,"sdP"][[1]]), lty=2)
plot(density(chains[,"eta_alphaN"][[1]]), main="eta_alphaN")
lines(density(chains[,"eta_alphaN"][[2]]))
lines(density(chainsP[,"eta_alphaN"][[1]]), lty=2)
plot(density(chains[,"aD"][[1]]), main="aD")
lines(density(chains[,"aD"][[2]]))
lines(density(chainsP[,"aD"][[1]]), lty=2)
plot(density(chains[,"bD"][[1]]), main="bD")
lines(density(chains[,"bD"][[2]]))
lines(density(chainsP[,"bD"][[1]]), lty=2)
plot(density(chains[,"aB"][[1]]), main="aB")
lines(density(chains[,"aB"][[2]]))
lines(density(chainsP[,"aB"][[1]]), lty=2)
plot(density(chains[,"bB"][[1]]), main="bB")
lines(density(chains[,"bB"][[2]]))
lines(density(chainsP[,"bB"][[1]]), lty=2)
plot(density(chains[,"sdBB"][[1]]), main="sdBB")
lines(density(chains[,"sdBB"][[2]]))
lines(density(chainsP[,"sdBB"][[1]]), lty=2)

plot(density(chains[,"etaB"][[1]]), main="etaB", xlim=c(0,65000))
lines(density(chains[,"etaB"][[2]]))
lines(density(chainsP[,"etaB"][[1]]), lty=2)
plot(density(chains[,"cvD"][[1]]), main="cvD")
lines(density(chains[,"cvD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)
plot(density(chains[,"cvmuD"][[1]]), main="cvmuD")
lines(density(chains[,"cvmuD"][[2]]))
lines(density(chainsP[,"cvmuD"][[1]]), lty=2)




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



