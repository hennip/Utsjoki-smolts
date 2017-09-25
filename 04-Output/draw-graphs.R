
#source("00-Functions/tidy-functions.r")
#source("00-Functions/my-palette.r")
#source("00-Functions/packages-and-paths.r")
#source("01-Data/tidy-smolts-data.r")

#source("load-simulations.r")



#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
#n_days<-61
#df<-smolts_data_to_jags(years, n_days) # 61: only june & july

# Number of smolts
##################################################
df<-boxplot.jags.df(chains, "Ntot",Year)

Ntot<-c()
for(i in 1:length(years)){
  Ntot[i]<-sum(filter(dat_all, Year==years[i])$smolts, na.rm=T) 
}
df<-mutate(df, Ntot)

ggplot(df, aes(x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(x="Year", y="Number of smolts", title="Annual size of the smolt run")+
  geom_point(aes(x=Year, y=Ntot))+
  coord_cartesian(ylim=c(0,38000))


# Daily numbers
for(i in 1:length(years)){
  df<-boxplot.jags.df2(chains, "N[",paste(sep="", i,"]"),1:n_days)
  df<-mutate(df, Year=years[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-as.tibble(df2)
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","Year"))


#df<-full_join(df2,dat_all, by=NULL)
df<-df2%>%
  left_join(dat_all)%>%
  select(Day,Month, Year,day, smolts, q50, everything())

#View(df)

ggplot(df, aes(day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  geom_line(aes(day,q50))+
#  facet_grid(.~Year)+
  facet_wrap(~Year)+
  geom_point(mapping=aes(day,smolts), color="blue", shape=17, size=2)+
  geom_line(aes(day,smolts), col="blue")+
  labs(x="Day (in June-July)", y="Number of smolts")
#  coord_cartesian(ylim=c(0,3800))




# Prob to start migration vs. temperature
#########################################
source("04-Output/sample-prob-to-migrate.r")

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
source("04-Output/sample-travel-time-flow.r")

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
    stat = "identity")+
  labs(x="Flow (m3/s)", y="Probability", title="Probability to observe a smolt at given flow")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))



