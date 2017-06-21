
source("tidy-functions.r")
source("tidy-smolts-data.r")
source("my-palette.r")
source("packages-and-paths.r")

source("load-simulations.r")


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



##########
# MCMC output 

# ggs imports mcmc samples into a ggs object that can be used by ggs_* graphical functions
(chains_ggs<-ggs(chains))
#ggmcmc(chains)
#ggs_traceplot(chains_ggs)


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
y<-c("2003","2004","2005","2006","2008","2014")
for(i in 1:6){
  df<-boxplot.jags.df2(chains, "N[",paste(sep="", i,"]"),1:61)
  df<-mutate(df, year=y[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-as.tibble(df2)
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","year"))
df<-full_join(df2,ts, by=NULL)


View(df)


ggplot(df, aes(day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity"
  )+
  geom_line(aes(day,q50))+
#  facet_grid(.~year)
  facet_wrap(~year,drop=T)+
  geom_point(mapping=aes(day,num_smolts), 
             color="blue", shape=17, size=2)+
  labs(x="Day (in June-July)", y="Number of smolts")



#df<-boxplot.jags.df2(chains, "N[","3]",1:61)
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
  traceplot(chains[,paste(sep="","N[",i,",3]")],main=i, cex.main=1.5)
}  

colnames(qmu)<-c("mean","sd","2.5%","25%","50%","75%","97.5%")



# Next, time series Ntot
(tmp<-filter(chains, Parameter=="Ntot[1]"))
(tmp<-filter(chains, Parameter==starts_with("Ntot")))

levels(chains$Parameter)



