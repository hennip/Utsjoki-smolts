

years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005:2009,2014) # 6 years of data for testing  
years<-c(2005:2011,2013,2014) # 9 years, total time series so far (2012 missing)

#years<-c(2006,2008) # 2 years of data for testing- schools model  
#years<-c(2005,2006,2008) # 3 years of data for testing- schools model  
n_days<-61
df<-smolts_data_to_jags(dat_all,years, n_days) # 61: only june & july


# Number of smolts
##################################################

# Annual totals
Year<-years
df<-boxplot.jags.df(chains, "Ntot",Year)
#chains2<-chainsP
df2<-boxplot.jags.df(chains2, "Ntot",Year)

Ntot<-c()
for(i in 1:length(years)){
  Ntot[i]<-sum(filter(dat_all, Year==years[i])$smolts, na.rm=T) 
}
df<-df%>%
  mutate(Ntot)%>%
  mutate(x2=parse_factor(x, levels=NULL))

df2<-df2%>%
  mutate(Ntot)%>%
  mutate(x2=parse_factor(x, levels=NULL))

ggplot(df, aes(x2))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Year", y="Number of smolts", title="Annual size of the smolt run")+
  geom_point(aes(x=x2, y=Ntot))+
  coord_cartesian(ylim=c(0,40000))+
  theme_bw()
  geom_boxplot(data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity", col="grey")
  
summary(as.mcmc(chains[,"Ntot[1]"][[1]]))$statistics[1]/Ntot[1]
summary(as.mcmc(chains[,"Ntot[2]"][[1]]))$statistics[1]/Ntot[2]
summary(as.mcmc(chains[,"Ntot[3]"][[1]]))$statistics[1]/Ntot[3]
summary(as.mcmc(chains[,"Ntot[4]"][[1]]))$statistics[1]/Ntot[4]
summary(as.mcmc(chains[,"Ntot[5]"][[1]]))

summary(as.mcmc(chains[,"Ntot[1]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[1]
summary(as.mcmc(chains[,"Ntot[2]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[2]
summary(as.mcmc(chains[,"Ntot[3]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[3]
summary(as.mcmc(chains[,"Ntot[4]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[4]


summary(as.mcmc(chainsP[,"Ntot[1]"][[1]]))$statistics[1]/Ntot[1]
summary(as.mcmc(chainsP[,"Ntot[2]"][[1]]))$statistics[1]/Ntot[2]
summary(as.mcmc(chainsP[,"Ntot[3]"][[1]]))$statistics[1]/Ntot[3]
summary(as.mcmc(chainsP[,"Ntot[4]"][[1]]))$statistics[1]/Ntot[4]

gelman.diag(chains[,"N[25,5]"])
gelman.diag(chains[,"N[26,5]"])
gelman.diag(chains[,"N[27,5]"])
gelman.diag(chains[,"N[28,5]"])
gelman.diag(chains[,"N[29,5]"])
gelman.diag(chains[,"N[30,5]"])
gelman.diag(chains[,"N[31,5]"])
gelman.diag(chains[,"N[32,5]"])
gelman.diag(chains[,"N[33,5]"])
gelman.diag(chains[,"N[34,5]"])




# Daily numbers
for(i in 1:length(years)){
  df<-boxplot.jags.df2(chains, "N[",str_c(i,"]"),1:n_days)%>%
    mutate(Year=years[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","Year"))

df<-df2%>%
  left_join(dat_all)%>%
  select(Day,Month, Year,day, smolts, q50, everything())


ggplot(df, aes(day))+
  geom_line(aes(day,q50))+
 geom_line(aes(day,smolts), col="grey50")+
 # geom_line(aes(day,meanTemp*100), col="red")+
 # geom_line(aes(day,flow*10), col="blue")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  facet_wrap(~Year)+
  geom_point(mapping=aes(day,smolts), col="grey50")+
  labs(x="Day (in June-July)", y="Number of smolts")+
  coord_cartesian(ylim=c(0,3700))+
  theme_bw()


#########################################
# 2005 proportion missed (first 23 days)

tmp<-0
tmp2<-0
for(i in 1:23){
  tmp<-chains[,str_c("N[",i,",1]")][[1]]+tmp
  tmp2<-chains2[,str_c("N[",i,",1]")][[1]]+tmp2
}
summary(tmp)
summary(tmp2)

summary(tmp/chains[,"Ntot[1]"][[1]])
summary(tmp2/chains2[,"Ntot[1]"][[1]])


# Prob to start migration vs. temperature
#########################################
source("04-Output/sample-prob-to-migrate.r")

df<-boxplot.df(p_samp, Temp)
df.prior<-boxplot.df(p_sampP, Temp)


# In black and white
ggplot(df, aes(x))+
    theme_bw()+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
    #stat = "identity",fill=rgb(1,0,0,0.1))+
  labs(x="Temperature (degrees celsius)", y="Probability", title="Probability to begin migration")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50),col="grey")


#################################
# Travel time to video vs flow

# Travel time at minimum flow
FLOW<-10
fn<-c(1:14)
source("04-Output/sample-travel-time-flow.r")

df<-boxplot.df(qD_cumul_samp, fn)
df.prior<-boxplot.df(qD_cumul_sampP, fn)

plot1<-ggplot(df, aes(x))+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey98")+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Days", y="Probability", title="Travel time to video site with flow=10m3/s on the day of departure")+
  geom_line(aes(x,q50))+
  #coord_cartesian(xlim=c(1:14), ylim=c(0,0.8))+
  coord_cartesian(xlim=c(1:14), ylim=c(0,1))+
  theme_bw()

# Travel time at maximum flow
FLOW<-100
fn<-c(1:14)
source("04-Output/sample-travel-time-flow.r")

df<-boxplot.df(qD_cumul_samp, fn)
df.prior<-boxplot.df(qD_cumul_sampP, fn)

plot2<-ggplot(df, aes(x))+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey98")+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Days", y="Probability", title="Travel time to video site with flow=100m3/s on the day of departure")+
  geom_line(aes(x,q50))+
  coord_cartesian(xlim=c(1:14), ylim=c(0,1))+
  theme_bw()

grid.arrange(plot1, plot2, nrow=2)



#################################
# Observation probability vs flow
source("04-Output/sample-obs-prob-flow.r")

df<-boxplot.df(muB_samp, Flow)
df.prior<-boxplot.df(muB_sampP, Flow)

ggplot(df, aes(x))+
  geom_boxplot(data=df.prior,
               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
               stat = "identity",
               col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Flow (m3/s)", y="Probability", title="Probability to observe a smolt at given flow")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))


#################################
# Daily passage vs school size
source("04-Output/sample-passage-vs-school-size.r")

df<-boxplot.df(muS_samp, Ntrue)
df.prior<-boxplot.df(muS_sampP, Ntrue)


ggplot(df, aes(x))+
  geom_boxplot(data=df.prior,
               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
               stat = "identity",
               col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Daily passage (number of smolts)", y="School size", title="Expected school size at given daily passage")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,30))



