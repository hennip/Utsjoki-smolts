source("00-Functions/packages-and-paths.R")

# runjags diagnostics (object "run")

#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_coefaB_priors_0221_run_susi5.RData"))
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_priors_0221_run_susi5.RData"))
load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_coef_priors_0221_run_susi5.RData"))
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_no_limit_priors_0221_run_susi5.RData"))
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_z_priors_0221_run_susi5.RData"))
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_free_priors_0221_run_susi5.RData"))

load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_free_pow2_priors_0221_run_susi5.RData"))

chainsP<-as.mcmc.list(run)
#chainsP<-window(chainsP,start=50000, thin=3000)
length(chainsP[,"aB_mid"][[1]])

#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_coef_0221_run_susi5.RData")) 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_coefaB_0221_run_susi5.RData")) 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_0221_run_susi5.RData")) 
load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_coef_0221_run_susi5.RData")) 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_no_limit_sdrho001_0221_run_susi5.RData")) # Huono idea, a_rho ja b_rho päivittyvät kehnosti 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_z_0221_run_susi5.RData")) 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_no_limit_0221_run_susi5.RData")) 
#load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_free_0221_run_susi5.RData")) 


load(file=paste0(pathMain,"output/utsjoki-smolts/Smolts_sides_free_pow2_0221_run_susi5.RData")) 

chains<-as.mcmc.list(run)
#chains<-window(chains,start=800000)#, thin=3000)
length(chains[,"aB_mid"][[1]])

plot(run, var="etaB")

summary(run, var="a")
summary(run, var="b")
summary(run, var="sd")
summary(run, var="D")
summary(run, var="P")
summary(run, var="B")
summary(run, var="pref")
summary(run, var="coef")

summary(run, var="Ntot")
summary(run, var="eta")
summary(run, var="fl")
summary(run, var="temp")

plot(run, var="z")
plot(run, var="coef")
plot(run, var="rho")
plot(run, var="eta")
plot(run, var="pref")
plot(run, var="a")
plot(run, var="b")
plot(run, var="sd")
plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="Ntot")
plot(run, var="fl")
plot(run, var="temp")

#summary(run, var="K")
#summary(run, var="slope")
#plot(run, var="S")


chains<-as.mcmc.list(run)
chains<-window(chains,start=350000)
#chains<-window(chains,start=700000)



gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])
gelman.diag(chains[,"Ntot[6]"])
gelman.diag(chains[,"Ntot[7]"])
gelman.diag(chains[,"Ntot[8]"])

gelman.diag(chains[,"etaB"])
gelman.diag(chains[,"aP"])
gelman.diag(chains[,"bP"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"cvmuD"])


# Traces
par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(i in 1:20){
  traceplot(chains[,paste0("Ntot[",i,"]")],main=paste0("Ntot",i), cex.main=1.5)
}

traceplot(chains[,"aP"],main=expression("aP"), cex.main=1.5)


traceplot(chains[,"aP"],main=expression("aP"), cex.main=1.5)
traceplot(chains[,"bP"],main=expression("bP"), cex.main=1.5)
traceplot(chains[,"sdP"],main=expression("sdP"), cex.main=1.5)
traceplot(chains[,"aD"],main=expression("aD"), cex.main=1.5)
traceplot(chains[,"bD"],main=expression("bD"), cex.main=1.5)
traceplot(chains[,"cvD"],main=expression("cvD"), cex.main=1.5)
traceplot(chains[,"cvmuD"],main=expression("cvmuD"), cex.main=1.5)

traceplot(chains[,"aB_mid"],main=expression("aB"), cex.main=1.5)
traceplot(chains[,"bB_mid"],main=expression("bB"), cex.main=1.5)
traceplot(chains[,"sdBB_mid"],main=expression("sdBB"), cex.main=1.5)
traceplot(chains[,"aB_side"],main=expression("aB"), cex.main=1.5)
traceplot(chains[,"bB_side"],main=expression("bB"), cex.main=1.5)
traceplot(chains[,"sdBB_side"],main=expression("sdBB"), cex.main=1.5)
traceplot(chains[,"etaB_mid"],main=expression("etaB"), cex.main=1.5)
traceplot(chains[,"etaB_side"],main=expression("etaB"), cex.main=1.5)
traceplot(chains[,"eta_alphaN"],main=expression("eta_alphaN"), cex.main=1.5)
traceplot(chains[,"a_rho"],main=expression("a rho"), cex.main=1.5)
traceplot(chains[,"b_rho"],main=expression("b rho"), cex.main=1.5)
traceplot(chains[,"sd_rho"],main=expression("sd rho"), cex.main=1.5)
traceplot(chains[,"pref"],main=expression("pref"), cex.main=1.5)


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
plot(density(chains[,"aP"][[1]]), main="aP", xlim=c(-25,-15), ylim=c(0,0.8))
lines(density(chains[,"aP"][[2]]))
lines(density(chainsP[,"aP"][[1]]), lty=2)
plot(density(chains[,"bP"][[1]]), main="bP", xlim=c(1,2))
lines(density(chains[,"bP"][[2]]))
lines(density(chainsP[,"bP"][[1]]), lty=2)
plot(density(chains[,"sdP"][[1]]), main="sdP", xlim=c(0,6), ylim=c(0,1.5))
lines(density(chains[,"sdP"][[2]]))
lines(density(chainsP[,"sdP"][[1]]), lty=2)
plot(density(chains[,"eta_alphaN"][[1]]), main="eta_alphaN", ylim=c(0,0.01))
lines(density(chains[,"eta_alphaN"][[2]]))
lines(density(chainsP[,"eta_alphaN"][[1]]), lty=2)
plot(density(chains[,"aD"][[1]]), main="aD", ylim=c(0,3))
lines(density(chains[,"aD"][[2]]))
lines(density(chainsP[,"aD"][[1]]), lty=2)
plot(density(chains[,"bD"][[1]]), main="bD", ylim=c(0,250))
lines(density(chains[,"bD"][[2]]))
lines(density(chainsP[,"bD"][[1]]), lty=2)

par(mfrow=c(4,3),mar=c(2.5,4,4,1))
plot(density(chains[,"aB_mid"][[1]]), main="aB mid", ylim=c(0,2))#c(0,.06))
lines(density(chains[,"aB_mid"][[2]]))
lines(density(chainsP[,"aB_mid"][[1]]), lty=2)
plot(density(chains[,"bB_mid"][[1]]), main="bB mid", ylim=c(0,50))#c(0,20))
lines(density(chains[,"bB_mid"][[2]]))
lines(density(chainsP[,"bB_mid"][[1]]), lty=2)
plot(density(chains[,"sdBB_mid"][[1]]), main="sdBB mid", ylim=c(0,3))
lines(density(chains[,"sdBB_mid"][[2]]))
lines(density(chainsP[,"sdBB_mid"][[1]]), lty=2)

plot(density(chains[,"aB_side"][[1]]), main="aB side", ylim=c(0,1))#c(0,.06))
lines(density(chains[,"aB_side"][[2]]))
lines(density(chainsP[,"aB_side"][[1]]), lty=2)
plot(density(chains[,"bB_side"][[1]]), main="bB side", ylim=c(0,50))#c(0,20))
lines(density(chains[,"bB_side"][[2]]))
lines(density(chainsP[,"bB_side"][[1]]), lty=2)
plot(density(chains[,"sdBB_side"][[1]]), main="sdBB side", ylim=c(0,2))
lines(density(chains[,"sdBB_side"][[2]]))
lines(density(chainsP[,"sdBB_side"][[1]]), lty=2)

plot(density(chains[,"etaB"][[1]]), main="etaB", ylim=c(0,0.002), xlim=c(0,1000))
lines(density(chains[,"etaB"][[2]]))
lines(density(chainsP[,"etaB"][[1]]), lty=2)
plot(density(chains[,"etaB_mid"][[1]]), main="etaB_mid", ylim=c(0,0.01))
lines(density(chains[,"etaB_mid"][[2]]))
lines(density(chainsP[,"etaB_mid"][[1]]), lty=2)
plot(density(chains[,"etaB_side"][[1]]), main="etaB_side", ylim=c(0,0.001))
lines(density(chains[,"etaB_side"][[2]]))
lines(density(chainsP[,"etaB_side"][[1]]), lty=2)

plot(density(chains[,"a_rho"][[1]]), main="a rho", ylim=c(0,2))#c(0,.06))
lines(density(chains[,"a_rho"][[2]]))
lines(density(chainsP[,"a_rho"][[1]]), lty=2)
plot(density(chains[,"b_rho"][[1]]), main="b rho", ylim=c(0,50))#c(0,20))
lines(density(chains[,"b_rho"][[2]]))
lines(density(chainsP[,"b_rho"][[1]]), lty=2)
plot(density(chains[,"sd_rho"][[1]]), main="sd rho", ylim=c(0,2), xlim=c(0,5))
lines(density(chains[,"sd_rho"][[2]]))
lines(density(chainsP[,"sd_rho"][[1]]), lty=2)

plot(density(chains[,"cvD"][[1]]), main="cvD", ylim=c(0,4))
lines(density(chains[,"cvD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)
plot(density(chains[,"cvmuD"][[1]]), main="cvmuD", ylim=c(0,5))
lines(density(chains[,"cvmuD"][[2]]))
lines(density(chainsP[,"cvmuD"][[1]]), lty=2)

plot(density(chains[,"z"][[1]]), main="z")
lines(density(chains[,"z"][[2]]))
lines(density(chainsP[,"z"][[1]]), lty=2)

par(mfrow=c(1,2))
 plot(density(chains[,"coef_side"][[1]]), main="coef_side", ylim=c(0,3))
 lines(density(chains[,"coef_side"][[2]]))
 lines(density(chainsP[,"coef_side"][[1]]), lty=2)

plot(density(chains[,"pref"][[1]]), main="pref", ylim=c(0,10))
lines(density(chains[,"pref"][[2]]))
lines(density(chainsP[,"pref"][[1]]), lty=2)


# plot(density(chains[,"K"][[1]]), main="K")
# lines(density(chains[,"K"][[2]]))
# lines(density(chainsP[,"K"][[1]]), lty=2)
# plot(density(chains[,"slope"][[1]]), main="slope", xlim=c(0.2,0.8))
# lines(density(chains[,"slope"][[2]]))
# lines(density(chainsP[,"slope"][[1]]), lty=2)

# plot(density(chains[,"cvS"][[1]]), main="cvS", ylim=c(0,3))
# lines(density(chains[,"cvS"][[2]]))
# lines(density(chainsP[,"cvS"][[1]]), lty=2)
# plot(density(chains[,"cvmuS"][[1]]), main="cvmuS", ylim=c(0,4))
# lines(density(chains[,"cvmuS"][[2]]))
# lines(density(chainsP[,"cvmuS"][[1]]), lty=2)




# Nämä eivät päivity prioreista, koska priorimallissa ympäristökovariaatit mukana
# plot(density(chains[,"a_temp"][[1]]), main="aB temp", ylim=c(0,6))#c(0,.06))
# lines(density(chains[,"a_temp"][[2]]))
# lines(density(chainsP[,"a_temp"][[1]]), lty=2)
# plot(density(chains[,"b_temp"][[1]]), main="bB temp", ylim=c(0,30))#c(0,20))
# lines(density(chains[,"b_temp"][[2]]))
# lines(density(chainsP[,"b_temp"][[1]]), lty=2)
# 
# 
# plot(density(chains[,"a_fl"][[1]]), main="aB fl", ylim=c(0,0.5))#c(0,.06))
# lines(density(chains[,"a_fl"][[2]]))
# lines(density(chainsP[,"a_fl"][[1]]), lty=2)
# plot(density(chains[,"b_fl[1]"][[1]]), main="bB fl", ylim=c(0,5))#c(0,20))
# lines(density(chains[,"b_fl[1]"][[2]]))
# lines(density(chainsP[,"b_fl[1]"][[1]]), lty=2)

#"a_temp", "b_temp", "a_fl", "b_fl", 
#"mu_phi_temp", "eta_phi_temp","mu_phi_fl", "eta_phi_fl",




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


aB_side<-as.data.frame(chains[,"aB_side"][[1]])
bB_side<-as.data.frame(chains[,"bB_side"][[1]])
a_rho<-as.data.frame(chains[,"a_rho"][[1]])
sdBB_mid<-as.data.frame(chains[,"sdBB_mid"][[1]])
sd_rho<-as.data.frame(chains[,"sd_rho"][[1]])

par(mfrow=c(2,3))
plot(aB_side[,1],bB_side[,1], xlab="aB_side", ylab="bB_side", col="red")
plot(aB_side[,1],a_rho[,1], xlab="aB_side", ylab="a_rho", col="red")
plot(sdBB_mid[,1],sd_rho[,1], xlab="sdBB_mid", ylab="sd_rho", col="red")


##################################################
par(mfrow=c(3,6))
for(i in 1:20){
  traceplot(chains[,paste(sep="","Ntot[",i,"]")],main=i, cex.main=1.5, col=c("black", "red"))
}  


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






