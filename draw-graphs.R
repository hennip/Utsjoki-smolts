
source("tidy-functions.r")
source("tidy-smolts-data.r")
source("my-palette.r")
source("packages-and-paths.r")


#######################
# Load simulations

load(file=paste(sep="",pathOut,"Smolts_17_04_26.RData")) # vaikuttaa lupaavalta, konvergenssi 1vrk jälkeen?

chains<-window(chains,start=100000, thin=1)

# Unupdated priors
#load(file=paste(sep="",pathOut,"Smolts_17_04_priors.RData"))
load(file=paste(sep="",pathOut,"Smolts_15_06_priors.RData"))
#chains<-chainsP

# Data
##########

# Number of smolts
ggplot(data=filter(ts,year==2005), mapping=aes(x=day,y=num_smolts))+
  geom_point()+
  geom_line()

ggplot(data = ts1,mapping = aes(x = day, y = num_smolts)) + 
  geom_point()+
  geom_line()+
  facet_grid(.~year)

# Temperature
ggplot(data = filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(mapping = aes(x = day, y = temperature, color=year), size=1.2)

# Flow
ggplot(data = filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(mapping = aes(x = day, y = flow, color=year), size=1.2)



#schools<-read.table("input/Schools_03-06.txt", header=T)
data<-list(
  #s=schools,
  flow=datF,
  nDays=61,
  nYears=6,
  Nobs=S,
  Temp=temp
)


##########
# MCMC output 

# ggs imports mcmc samples into a ggs object that can be used by ggs_* graphical functions
(chains_ggs<-ggs(chains))
#ggmcmc(chains)
#ggs_traceplot(chains)


# Number of smolts
##################################################
df<-boxplot.jags.df(chains, "Ntot",1:6)
#as.tibble(df)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity"
  )


# Smolts in 2005
df<-boxplot.jags.df2(chains, "N[","3]",1:61)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity"
  )+
  geom_line(aes(x,q50))+
  geom_point(data=filter(ts,year==2005), mapping=aes(x=day,y=num_smolts), 
             color="blue", shape=17, size=2)+
  labs(x="Day (in June-July)", y="Number of smolts")






# Prob to start migration vs. temperature
#########################################
source("sample-prob-to-migrate.r")

df<-boxplot.df(p_samp, Temp)
df.prior<-boxplot.df(p_sampP, Temp)


# In black and white
ggplot(df, aes(x))+
  #  theme_bw()+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey99")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Temperature (degrees celsius)", y="Probability", title="Probability to begin migration")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50),col="grey")

# In color
ggplot(df, aes(x))+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    col=1, fill="grey95")+
  geom_line(data=df.prior, aes(x,q50), col=1)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    col=2)+
  labs(x="Temperature (degrees celsius)", y="Probability", title="Probability to begin migration")+
  geom_line(aes(x,q50), col=2)



#################################
# Travel time to video vs flow
source("sample-travel-time-flow.r")

df<-boxplot.df(muD_samp, Flow)
df.prior<-boxplot.df(muD_sampP, Flow)

ggplot(df, aes(x))+
  geom_boxplot(data=df.prior,
               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
               stat = "identity",
               col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Flow (m3/s)", y="E(travel time) (in days)", title="Travel time to video site")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()



# Testing, parameter aD
aD<-filter(chains_ggs, Parameter=="aD" & Chain==1)
(aD<-spread(aD, key=Parameter, value=value))

y <- as.mcmc(chains[,"aD"][[1]])
tmp<-summary(y)
tmp<-summary(y,quantiles=c(0.05,0.25,0.5,0.75,0.95))

df <- data.frame(
  x = 1,
  q2.5 = tmp$quantiles[1],
  q25 = tmp$quantiles[2],
  q50 = tmp$quantiles[3],
  q75 = tmp$quantiles[4],
  q97.5 = tmp$quantiles[5]
)
ggplot(df, aes(x)) +
  geom_boxplot(
    aes(ymin = q2.5, lower = q25, middle = q50, upper = q75, ymax = q97.5),
    stat = "identity"
  )






#windows(record=T)
par(mfrow=c(3,6))
for(i in 1:61){
  traceplot(chains[,paste(sep="","N[",i,",3]")],main=i, cex.main=1.5, col=c("black", "red"))
}  

colnames(qmu)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")


ggplot(tmp)+
  geom_point(mapping = aes(x = Iteration, y = aD))

ggplot(tmp)+
  geom_boxplot()

?geom_boxplot

# Next, time series Ntot
(tmp<-filter(chains, Parameter=="Ntot[1]"))
(tmp<-filter(chains, Parameter==starts_with("Ntot")))

levels(chains$Parameter)



par(mfrow=c(3,4),mar=c(2.5,4,4,1))
traceplot(chains[,"Ntot[1]"],main="Ntot1", cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"Ntot[2]"],main="Ntot2", cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"Ntot[3]"],main="Ntot3", cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"Ntot[4]"],main="Ntot4", cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"Ntot[5]"],main="Ntot5", cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"Ntot[6]"],main="Ntot6", cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"aP"],main=expression("aP"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"bP"],main=expression("bP"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"sdP"],main=expression("sdP"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"eta_alphaN"],main=expression("eta_alphaN"), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"aD"],main=expression("aD"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"bD"],main=expression("bD"), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"cvD"],main=expression("cvD"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"cvmuD"],main=expression("cvmuD"), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"muB"],main=expression("muB"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"aB"],main=expression("aB"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"bB"],main=expression("bB"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaB"],main=expression("etaB"), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"cvBB"],main=expression("cvBB"), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"sum1"])
traceplot(chains[,"sum31"])
summary(chains[,"sum1"])
summary(chains[,"sum31"])


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
lines(density(chainsP[,"eta"][[1]]), lty=2)
plot(density(chains[,"aD"][[1]]), main="aD")
lines(density(chains[,"aD"][[2]]))
lines(density(chainsP[,"aD"][[1]]), lty=2)
plot(density(chains[,"bD"][[1]]), main="bD")
lines(density(chains[,"bD"][[2]]))
lines(density(chainsP[,"bD"][[1]]), lty=2)

plot(density(chains[,"aB"][[1]]), main="aB")
lines(density(chains[,"aB"][[2]]))
lines(density(chainsP[,"aBX"][[1]]), lty=2)
plot(density(chainsP[,"aBX"][[1]]), lty=2)
plot(density(chains[,"bB"][[1]]), main="bB")
lines(density(chains[,"bB"][[2]]))
lines(density(chainsP[,"bBX"][[1]]), lty=2)
plot(density(chainsP[,"bBX"][[1]]), lty=2)

plot(density(chains[,"cvD"][[1]]), main="cvD")
lines(density(chains[,"cvD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)

plot(density(chains[,"cvmuD"][[1]]), main="cvmuD")
lines(density(chains[,"cvmuD"][[2]]))
lines(density(chainsP[,"cvD"][[1]]), lty=2)



