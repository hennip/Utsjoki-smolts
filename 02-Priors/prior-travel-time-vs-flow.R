# Need to have faster travel time to video site compared to
#aD~dlnorm(0.9,45) # mu=2.5,cv=0.2
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2

# 8/17 Need to have yet faster travel time compared to
#aD~dlnorm(0.68,45) # mu=2,cv=0.15
#bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
# Discussion with Panu in June; travel time at lowest flow
# q95 at 14 days 
# q75 at 8-9 days
# q50 at 4-5 days
# q25 at 2-3 days

# 9/17: Let's seek for something even faster.
# cvD adds variation to this expected travel time and thus to keep 
# total travel time at 14 days at max (roughly), travel time needs to 
# be 1) restricted, 2) standardised or 3) simply reduced. Since 1) and 2)
# seem to hinder sampling badly, let's go back to reducing.
# Expert info for this matter is pretty vague, so there's not much harm done
# if expected travel time will be reduced.
# Let's seek something that has (at flow 10m3)
# q95 at 8-9 days 
# q75 at 5-6 days
# q50 at 3-4 days
# q25 at 2-3 days
# q5 at 1-2 days

#source("packages-and-paths.r")


Flow<-seq(10,150, by=10)
nF<-length(Flow)
M2<-"
  model{
  for(i in 1:n){
    muD[i]~dlnorm(log(mumuD[i])-0.5/TmuD, TmuD)
    mumuD[i]<-exp(aD-bD*Flow[i])
  }
  TmuD<-1/log(cvmuD*cvmuD+1)
  cvmuD~dunif(0.001,1)

# Priors to change!:
# ==============
#  muaD<-1.4 #1.75
#  cvaD<-0.27 # 0.27
#  mubD<-0.01
#  cvbD<-0.2
#  aD~dlnorm(log(muaD)-0.5*log(cvaD*cvaD+1),1/log(cvaD*cvaD+1)) 
#  bD~dlnorm(log(mubD)-0.5*log(cvbD*cvbD+1),1/log(cvbD*cvbD+1)) 
  
aD~dlnorm(0.3,14) # mu=1.4,cv=0.27
bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
# ==============
  
  
  # Check total travel time at 10m3 and sum over 14 days
  # ==============
  # Migration speed (in days to video site)
  # probability to be at video site in day j, if departing at day i
  # 1: day of departure
  # j: day of passing the video site
  qD[1]<-phi((log(0.5)-MD)/SD)
  
  # j>i
  for(j in 2:14){ 
  qD[j]<-phi((log(j-1+0.5)-MD)/SD)-phi((log(j-1-0.5)-MD)/SD)
  }
  
  MD<-log(muD[1])-0.5/TD
  cvD~dunif(0.001,2)
  
  TD<-1/log(cvD*cvD+1)
  SD<-1/sqrt(TD)
  sums<-sum(qD[1:14])

}"
#muaD<-1.4 
#cvaD<-0.27 
#log(muaD)-0.5*log(cvaD*cvaD+1)
#1/log(cvaD*cvaD+1) 
#mubD<-0.01
#cvbD<-0.2
#log(mubD)-0.5*log(cvbD*cvbD+1)
#1/log(cvbD*cvbD+1) 



cat(M2,file="prior_traveltime.txt")

data<-list(Flow=Flow, n=length(Flow))

system.time(jm<-jags.model('prior_traveltime.txt',
                           n.adapt=100,data=data,n.chains=2))

system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "sums",
                                    "muD"
                                  ),
                                  n.iter=50000,
                                  thin=1))
chainsP2<-chains1


df<-boxplot.jags.df(chainsP2,"muD", Flow)

ggplot(df, aes(x))+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Flow (m3/s)", y="E(travel time) (in days)", title="Travel time to video site")+
  geom_line(aes(x,q50))+
  coord_cartesian(ylim=c(0,20))+
  theme_bw()

summary(chainsP2, quantiles = c(0.05,0.25,0.50,0.75,0.95))



