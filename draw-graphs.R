
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
#(chains_ggs<-ggs(chains))
#ggmcmc(chains)
#ggs_traceplot(chains_ggs)


# Number of smolts
##################################################
Year<-c("2003","2004","2005","2006","2008","2014")
df<-boxplot.jags.df(chains, "Ntot",Year)
#as.tibble(df)
Ntot<-c()
for(i in 1:6){
  Ntot[i]<-sum(filter(ts, year==Year[i])$num_smolts, na.rm=T) 
}
df<-mutate(df, Ntot)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Year", y="Number of smolts", title="Annual size of the smolt run")+
  geom_point(aes(x=Year, y=Ntot))+
  coord_cartesian(ylim=c(0,38000))


# daily number
y<-c("2003","2004","2005","2006","2008","2014")
for(i in 1:6){
  df<-boxplot.jags.df2(chains, "N[",paste(sep="", i,"]"),1:61)
  df<-mutate(df, year=y[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-as.tibble(df2)
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","year"))
df<-full_join(df2,ts, by=NULL)
df<-select(df,day, year, num_smolts, q50, everything())

#View(df)

df1<-filter(df, year=="2003"| year=="2004")
df1<-filter(df, year=="2005"| year=="2006")
df1<-filter(df, year=="2008"| year=="2014")

ggplot(df1, aes(day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  geom_line(aes(day,q50))+
  facet_grid(.~year)+
  geom_point(mapping=aes(day,num_smolts), color="blue", shape=17, size=2)+
  geom_line(aes(day,num_smolts), col="blue")+
  labs(x="Day (in June-July)", y="Number of smolts")
#  coord_cartesian(ylim=c(0,3800))




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


#################################
# Observation probability vs flow
source("sample-obs-prob-flow.r")

df<-boxplot.df(muB_samp, Flow)
df.prior<-boxplot.df(muB_sampP, Flow)

ggplot(df, aes(x))+
  geom_boxplot(data=df.prior,
               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
               stat = "identity",
               col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Flow (m3/s)", y="Probability", title="Probability to observe a smolt at given flow")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))



