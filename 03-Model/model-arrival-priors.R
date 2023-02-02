#
# Remove data, keep covariates and fix abundance.
# Is the timing of the run reasonable a priori?



#source("00-Functions/packages-and-paths.r")

modelName<-"Smolts_etaB_wideB"


#modelName<-"Smolts_fixedNumber" # to make a prior model on arrival dist

#modelName<-"Smolts_etaB"
#modelName<-"Smolts_etaB_sdP"
#modelName<-"Smolts_etaStarB_sdP"



Mname<-str_c("03-Model/",modelName, ".txt")

# Select years
#years<-c(2005:2006,2007,2008,2014) # 4 years of data plus simulated 2007  
#years<-c(2005:2006,2008,2014) # 4 years of data for testing  
years<-c(2005:2009,2014) # 6 years to study
#years<-c(2005:2011,2013,2014) # 2012 temp data missing
dataName<-"0714"
compName<-"laptop"#"ould017"


n_days<-61
dat<-dat_all3 # 2007 first 17% missing, 2014 +- 2 days from the peak missing

df<-smolts_data_to_jags(dat,years, n_days) # 61: only june & july


data<-list(
  #s=df$Schools,
  flow=df$Flow,
  #  Nobs=df$Smolts,                     
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years)
)

inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               #                    aB=2,bB=0.03),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears)))#,
               #                    aB=2,bB=0.03)
)

var_names<-c(
  #  "Ntot",
  "N",
  "aD","bD","cvD","cvmuD",
  "K","slope","cvS", "cvmuS",
  #"sums1","sums2",
  
  "aP","bP","sdP",
  "etaB","aB","bB","sdBB",
  "eta_alphaN"
)

t1<-Sys.time()
run1 <- run.jags(Mname, 
                 monitor= var_names,data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =10, 
                 modules = "mix",keep.jags.files="priors",sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)

run<-run1
save(run, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_run_",compName,".RData"))

t1<-Sys.time()
run2 <- extend.jags(run1, combine=F, sample=5000, thin=10, keep.jags.files="priors")
t2<-Sys.time()
difftime(t2,t1)

run<-run2
save(run, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_run_",compName,".RData"))

summary(run, var="D")
summary(run, var="P")
summary(run, var="N")
summary(run, var="Ntot")
summary(run, var="eta_alphaN")
summary(run, var="sum")


plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="sums1[61]")
plot(run, var="eta_alphaN")

chainsP<-as.mcmc.list(run)
#chainsP<-window(chainsP,start=1000000)
save(chainsP, file=str_c(pathOut,"Priors_",modelName,"_",dataName,"_chains.RData"))


# Daily numbers
for(i in 1:length(years)){
  df<-boxplot.jags.df2(chainsP, "N[",str_c(i,"]"),1:n_days)%>%
    mutate(Year=years[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","Year"))

df<-df2%>%
  left_join(dat)%>%
  select(Day,Month, Year,day, smolts, q50, everything())%>%
  mutate(q5=q5/100000, q25=q25/100000,q50=q50/100000,q75=q75/100000,q95=q95/100000)


ggplot(df, aes(day, group=day))+
  geom_line(aes(day,q50))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  facet_wrap(~Year)+
  labs(x="Day (in June-July)", y="Proportion of the smolt run")+
  theme_bw()+
  theme(title = element_text(size=15), axis.text = element_text(size=12), 
        strip.text = element_text(size=15))+
  coord_cartesian(ylim=c(0,1.5))



