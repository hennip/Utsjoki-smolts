

# years<-c(2005,2006,2008) # 3 years of data for testing- schools model  

years<-c(2002:2014) # 13 years, total time series

years<-c(2005:2009,2014) # 6 years of data for testing  
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
  mutate(x2=as.factor(x))

df2<-df2%>%
  mutate(Ntot)%>%
  mutate(x2=as.factor(x))

ggplot(df, aes(x2))+
  geom_boxplot(data=df2,
    aes(ymin = q5/1000, lower = q25/1000, middle = q50/1000, upper = q75/1000, ymax = q95/1000),
    stat = "identity", col="grey", fill="grey95")+
  labs(x="Year", y="Number of smolts (in 1000's)", title="Annual size of the smolt run")+
  coord_cartesian(ylim=c(0,40))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5/1000, lower = q25/1000, middle = q50/1000, upper = q75/1000, ymax = q95/1000),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_point(aes(x=x2, y=Ntot/1000), size=3)+
  theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))



# ARKTIKO poster
ggplot(df, aes(x2))+
  labs(x="", y="Number of migratory juveniles in Utsjoki (in 1000's)", title="")+
  coord_cartesian(ylim=c(7,35))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5/1000, lower = q25/1000, middle = q50/1000, upper = q75/1000, ymax = q95/1000),
    stat = "identity",fill=rgb(1,1,1,0.6), size=1)+
  geom_point(aes(x=x2, y=Ntot/1000), size=3)+
  theme(title = element_text(size=15), axis.text = element_text(size=17), 
        strip.text = element_text(size=15))

  


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

df_tmp<-df%>%
  filter((Year=="2007" & day<25) | (Year=="2014" & day<41 & day>35))
#mutate(incl=ifelse((Year=="2007" & day<25) | (Year=="2014" & day<41 & day>35), 0, 1))
#View(df_tmp)

ggplot(df, aes(day))+
  geom_line(aes(day,q50))+
  geom_line(aes(day,smolts), col="grey50")+
  # geom_line(aes(day,meanTemp*100), col="red")+
 # geom_line(aes(day,flow*10), col="blue")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  facet_wrap(~Year)+
  geom_point(mapping=aes(day,smolts), col="grey10")+
  geom_point(data=df_tmp, mapping=aes(day,smolts), shape=21, fill="grey60", col="grey60")+
  labs(x="Day (in June-July)", y="Daily number of smolts")+
  coord_cartesian(ylim=c(0,3500))+
  theme_bw()+
  theme(title = element_text(size=15), axis.text = element_text(size=12), 
        strip.text = element_text(size=15))

# For priors only, see model-arrival-priors.R


# Poster
df<-filter(df, Year==2007)
ggplot(df, aes(day))+
  geom_line(aes(day,q50))+
  geom_line(aes(day,smolts), col="grey50")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  facet_wrap(~Year)+
  geom_point(mapping=aes(day,smolts), col="grey50")+
  geom_point(filter(df, day<25),mapping=aes(day,smolts), col="red")+
  labs(x="Day (in June-July)", y="Number of smolts")+
  theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))


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
  labs(x=expression(Temperature~(degree*C)), y="Probability", 
       title="Probability to begin migration at given temperature")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50),col="grey")#+
  #theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))
  


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
  labs(x="Days", y="Cumulative proportion", 
       title=expression("Cumulative travel time to video site with discharge 10m"^{3}*"/s on the day of departure"))+
#         "Cumulative travel time to video site with flow velocity 10m3/s on the day of departure")+
  geom_line(aes(x,q50))+
  coord_cartesian(xlim=c(1:14), ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
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
  labs(x="Days", y="Cumulative proportion", 
       title=expression("Cumulative travel time to video site with discharge 100m"^{3}*"/s on the day of departure"))+
  geom_line(aes(x,q50))+
  coord_cartesian(xlim=c(1:14), ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
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
  labs(x=expression("Discharge (m"^{3}*"/s)"), y="Probability", title="Probability that a smolt is observed")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))+
scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
#  theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))

# filter(df, x==5 |x==8 |x==10 |x==15 | x==20 |x==50 |x==60|x==80)
# x        q5       q25       q50       q75       q95
# 1  5 0.7590412 0.8271434 0.8551095 0.8733315 0.8874915
# 2 10 0.7169385 0.8001522 0.8374087 0.8624795 0.8827453
# 3 15 0.6613586 0.7650941 0.8135906 0.8475497 0.8752510
# 4 20 0.6116740 0.7252465 0.7842389 0.8265176 0.8645508
# 5 50 0.3621924 0.4219654 0.4834368 0.5622097 0.6796654
# 6 60 0.3310110 0.3643165 0.4046579 0.4612673 0.5722501
# 7 80 0.3072731 0.3159281 0.3274754 0.3466719 0.3956391


#################################
# Daily passage vs school size
source("04-Output/sample-passage-vs-school-size.r")

df<-boxplot.df(muS_samp, Ntrue)
df.prior<-boxplot.df(muS_sampP, Ntrue)


ggplot(df, aes(x))+
#  geom_boxplot(data=df.prior,
#               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#               stat = "identity", #size=5,
 #              col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6)#, size=5
    )+
  labs(x="Daily passage (number of smolts)", y="School size", title="Expected school size at given daily passage")+
  geom_line(aes(x,q50))+
#  geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0,50), xlim=c(0,1000))+
  geom_point(data=dat_all3,aes(x=smolts, y=schools))



#########################################
# Predictions on missing counts

# 2005 missing (first 23 days)
# ------------------------------
tmp<-0
tmp2<-0
for(i in 1:23){
  tmp<-chains[,str_c("N[",i,",1]")][[1]]+tmp
#  tmp2<-chains2[,str_c("N[",i,",1]")][[1]]+tmp2
}
summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))
#summary(tmp2, quantiles=c(0.05,0.25,0.5,0.75,0.95))

summary(tmp/chains[,"Ntot[1]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))
#summary(tmp2/chains2[,"Ntot[1]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))

# 2007 missing (first 24 days, 17% from beginning)
# ------------------------------

tmp<-0
tmp2<-0
for(i in 1:24){
  tmp<-chains[,str_c("N[",i,",3]")][[1]]+tmp
#  tmp2<-chains2[,str_c("N[",i,",3]")][[1]]+tmp2
}
summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))
#summary(tmp2, quantiles=c(0.05,0.25,0.5,0.75,0.95))

summary(tmp/chains[,"Ntot[3]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))
#summary(tmp2/chains2[,"Ntot[3]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))

m<-filter(dat_all, Year=="2007", day<25) 
nm<-summarise(m, sum(smolts));nm # sum of missing counts
nm/Ntot[3] # proportion of missing out of total count

x07<-c()
for( i in 1: length(tmp)){
  x07[i]<-ifelse(tmp[i]-nm>0,1,0)
  
}
mean(x07) # probability that the annual predicted number is higher than the sum of missed counts



# 2014 missing (peak +-2 days, 17% from beginning)
# ------------------------------

tmp<-0
tmp2<-0
for(i in 36:40){
  tmp<-chains[,str_c("N[",i,",6]")][[1]]+tmp
  #tmp2<-chains2[,str_c("N[",i,",6]")][[1]]+tmp2
}

m<-filter(dat_all, Year=="2014", day<41, day>35)
nm<-summarise(m, sum(smolts));nm # sum of missing counts
nm/Ntot[6] # proportion of missing out of total count

x14<-c()
for( i in 1: length(tmp)){
  x14[i]<-ifelse(tmp[i]-nm>0,1,0)
  
}
mean(x14)

summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(tmp/chains[,"Ntot[6]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))

#summary(tmp2, quantiles=c(0.05,0.25,0.5,0.75,0.95))
#summary(tmp2/chains2[,"Ntot[6]"][[1]], quantiles=c(0.05,0.25,0.5,0.75,0.95))


summary(chains[,str_c("N[",40,",6]")][[1]])



# ------------------------------
Mean<-c()
Mean[1]<-summary(as.mcmc(chains[,"Ntot[1]"][[1]]))$statistics[1]
Mean[2]<-summary(as.mcmc(chains[,"Ntot[2]"][[1]]))$statistics[1]
Mean[3]<-summary(as.mcmc(chains[,"Ntot[3]"][[1]]))$statistics[1]
Mean[4]<-summary(as.mcmc(chains[,"Ntot[4]"][[1]]))$statistics[1]
Mean[5]<-summary(as.mcmc(chains[,"Ntot[5]"][[1]]))$statistics[1]
Mean[6]<-summary(as.mcmc(chains[,"Ntot[6]"][[1]]))$statistics[1]

unobs<-c()
for(i in 1:6){
 unobs[i]<-(Mean[i]-Ntot[i])/Mean[i]
}
unobs

Quants<-list()
Quants[[1]]<-summary(as.mcmc(chains[,"Ntot[1]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
Quants[[2]]<-summary(as.mcmc(chains[,"Ntot[2]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
Quants[[3]]<-summary(as.mcmc(chains[,"Ntot[3]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
Quants[[4]]<-summary(as.mcmc(chains[,"Ntot[4]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
Quants[[5]]<-summary(as.mcmc(chains[,"Ntot[5]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
Quants[[6]]<-summary(as.mcmc(chains[,"Ntot[6]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles

X<-list()
for(i in 1:6){
  X[[i]]<-(Quants[[i]]-Ntot[i])/Quants[[i]]
}

summary(as.mcmc(chains[,"Ntot[1]"][[1]]))$statistics[1]/Ntot[1]
summary(as.mcmc(chains[,"Ntot[2]"][[1]]))$statistics[1]/Ntot[2]
summary(as.mcmc(chains[,"Ntot[3]"][[1]]))$statistics[1]/Ntot[3]
summary(as.mcmc(chains[,"Ntot[4]"][[1]]))$statistics[1]/Ntot[4]
summary(as.mcmc(chains[,"Ntot[5]"][[1]]))$statistics[1]/Ntot[5]
summary(as.mcmc(chains[,"Ntot[6]"][[1]]))$statistics[1]/Ntot[6]

summary(as.mcmc(chains2[,"Ntot[1]"][[1]]))$statistics[1]/Ntot[1]
summary(as.mcmc(chains2[,"Ntot[2]"][[1]]))$statistics[1]/Ntot[2]
summary(as.mcmc(chains2[,"Ntot[3]"][[1]]))$statistics[1]/Ntot[3]
summary(as.mcmc(chains2[,"Ntot[4]"][[1]]))$statistics[1]/Ntot[4]
summary(as.mcmc(chains2[,"Ntot[5]"][[1]]))$statistics[1]/Ntot[5]
summary(as.mcmc(chains2[,"Ntot[6]"][[1]]))$statistics[1]/Ntot[6]


summary(as.mcmc(chains2[,"Ntot[1]"][[1]]))$quantiles/Ntot[1]
summary(as.mcmc(chains2[,"Ntot[2]"][[1]]))$quantiles/Ntot[2]
summary(as.mcmc(chains2[,"Ntot[3]"][[1]]))$quantiles/Ntot[3]
summary(as.mcmc(chains2[,"Ntot[4]"][[1]]))$quantiles/Ntot[4]
summary(as.mcmc(chains2[,"Ntot[5]"][[1]]))$quantiles/Ntot[5]
summary(as.mcmc(chains2[,"Ntot[6]"][[1]]))$quantiles/Ntot[6]


summary(as.mcmc(chains[,"Ntot[1]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[1]
summary(as.mcmc(chains[,"Ntot[2]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[2]
summary(as.mcmc(chains[,"Ntot[3]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[3]
summary(as.mcmc(chains[,"Ntot[4]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[4]
summary(as.mcmc(chains[,"Ntot[5]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[5]
summary(as.mcmc(chains[,"Ntot[6]"][[1]]), quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles/Ntot[6]



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

summary(as.mcmc(chains[,"Ntot[1]"][[1]]))
