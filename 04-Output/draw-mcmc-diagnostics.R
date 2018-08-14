
# runjags diagnostics (object "run")

# parvimalli:
summary(run, var="aB")
summary(run, var="bB")
summary(run, var="etaB")

summary(run, var="S")

summary(run, var="K")
summary(run, var="slope")


plot(run, var="S")


summary(run, var="D")
summary(run, var="P")
summary(run, var="B")
summary(run, var="Ntot")
summary(run, var="eta_alphaN")
summary(run, var="sum")


plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="Ntot")
plot(run, var="eta_alphaN")
#plot(run, var="sum")

chains<-as.mcmc.list(run)
chains<-window(chains,start=500000)
#save(chains, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_fixedObsProp_0714_chains.RData")



gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])
gelman.diag(chains[,"Ntot[6]"])

gelman.diag(chains[,"aP"])
gelman.diag(chains[,"bP"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"cvmuD"])


# Traces
par(mfrow=c(3,3),mar=c(2.5,4,4,1))
traceplot(chains[,"Ntot[1]"],main="Ntot1", cex.main=1.5)
traceplot(chains[,"Ntot[2]"],main="Ntot2", cex.main=1.5)
traceplot(chains[,"Ntot[3]"],main="Ntot3", cex.main=1.5)
traceplot(chains[,"Ntot[4]"],main="Ntot4", cex.main=1.5)
traceplot(chains[,"Ntot[5]"],main="Ntot5", cex.main=1.5)
traceplot(chains[,"Ntot[6]"],main="Ntot6", cex.main=1.5)
traceplot(chains[,"Ntot[7]"],main="Ntot7", cex.main=1.5)
traceplot(chains[,"Ntot[8]"],main="Ntot8", cex.main=1.5)
traceplot(chains[,"Ntot[9]"],main="Ntot9", cex.main=1.5)

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

traceplot(chains[,"sums1[48]"],main=expression("sums1"))
traceplot(chains[,"sums2[61]"],main=expression("sums2"))

#summary(chains[,"sums1[48]"])
#summary(chains[,"sums2[48]"])
summary(chains[,"sdP"])

traceplot(chains[,"K"],main=expression("K"), cex.main=1.5)
traceplot(chains[,"slope"],main=expression("slope"), cex.main=1.5)
traceplot(chains[,"cvS"],main=expression("cvS"), cex.main=1.5)
traceplot(chains[,"cvmuS"],main=expression("cvmuS"), cex.main=1.5)

gelman.diag(chains)

#gd<-gelman.diag(chains)
#max(gd$psrf[,2])

#gd$psrf[,2]==1.408109



gelman.diag(chains[,"cvmuD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"bD"])

gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])


gelman.diag(chains[,"sdBB"])
gelman.diag(chains[,"eta_alphaN"])
gelman.diag(chains[,"etaB"])


#################################
# Densities

par(mfrow=c(4,3),mar=c(2.5,4,4,1))
plot(density(chains[,"aP"][[1]]), main="aP", xlim=c(-25,-10), ylim=c(0,1))
lines(density(chains[,"aP"][[2]]))
lines(density(chainsP[,"aP"][[1]]), lty=2)
plot(density(chains[,"bP"][[1]]), main="bP", xlim=c(0.7,1.2))
lines(density(chains[,"bP"][[2]]))
lines(density(chainsP[,"bP"][[1]]), lty=2)
plot(density(chains[,"sdP"][[1]]), main="sdP", xlim=c(0,4), ylim=c(0,0.8))
lines(density(chains[,"sdP"][[2]]))
lines(density(chainsP[,"sdP"][[1]]), lty=2)
plot(density(chains[,"eta_alphaN"][[1]]), main="eta_alphaN", ylim=c(0,0.06))
lines(density(chains[,"eta_alphaN"][[2]]))
lines(density(chainsP[,"eta_alphaN"][[1]]), lty=2)
plot(density(chains[,"aD"][[1]]), main="aD", ylim=c(0,3))
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
plot(density(chains[,"sdBB"][[1]]), main="sdBB", ylim=c(0,8))
lines(density(chains[,"sdBB"][[2]]))
lines(density(chainsP[,"sdBB"][[1]]), lty=2)

plot(density(chains[,"cvD"][[1]]), main="cvD", ylim=c(0,4))
lines(density(chains[,"cvD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)
plot(density(chains[,"cvmuD"][[1]]), main="cvmuD", ylim=c(0,20))
lines(density(chains[,"cvmuD"][[2]]))
lines(density(chainsP[,"cvmuD"][[1]]), lty=2)

plot(density(chains[,"K"][[1]]), main="K")
lines(density(chains[,"K"][[2]]))
lines(density(chainsP[,"K"][[1]]), lty=2)
plot(density(chains[,"slope"][[1]]), main="slope", xlim=c(0.2,0.8))
lines(density(chains[,"slope"][[2]]))
lines(density(chainsP[,"slope"][[1]]), lty=2)

plot(density(chains[,"cvS"][[1]]), main="cvS", ylim=c(0,3))
lines(density(chains[,"cvS"][[2]]))
lines(density(chainsP[,"cvS"][[1]]), lty=2)
plot(density(chains[,"cvmuS"][[1]]), main="cvmuS", ylim=c(0,4))
lines(density(chains[,"cvmuS"][[2]]))
lines(density(chainsP[,"cvmuS"][[1]]), lty=2)

plot(density(chains[,"etaB"][[1]]), main="etaB", ylim=c(0,0.0015))
lines(density(chains[,"etaB"][[2]]))
lines(density(chainsP[,"etaB"][[1]]), lty=2)


plot(density(chains[,"aB"][[1]]), main="aB", ylim=c(0,4))
lines(density(chains[,"aB"][[2]]))
lines(density(chainsP[,"aB"][[1]]), lty=2)

plot(density(chains[,"aB"][[1]]), main="aB", ylim=c(0,0.4))
lines(density(chains[,"aB"][[2]]))
lines(density(chainsP[,"aB"][[1]]), lty=2)


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






