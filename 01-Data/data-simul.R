# Replace 07 data with simulated. Add NA's where desired.

#Nsimul<-readRDS(file="N_simul.rds")

#Nsimul[1:14]<-rep(NA,14)
#Nsimul[36:40]<-rep(NA,5)

#df07<-select(filter(dat_all, Year==2007), -smolts, -schools)
#df_tmp<-cbind(Nsimul,1:92, rep(NA, 92)) 
#colnames(df_tmp)<-c("smolts","day", "schools")
#df_tmp<-as.tibble(df_tmp)

#df07<-full_join(df07, df_tmp)
#dat_all2<-full_join(filter(dat_all, Year!=2007), df07)
#View(dat_all2)


# Scenario first 15% missed ( days 1-24 =17%)
df07.1<-select(filter(dat_all, Year==2007), -smolts, -schools)
df07.2<-select(filter(dat_all, Year==2007), smolts, schools)
#View(df07.2)

df07.2[1:24,1]<-rep(NA,24)
df07.2[1:24,2]<-rep(NA,24)
df07.2<-cbind(df07.2,1:92) 
colnames(df07.2)<-c("smolts", "schools", "day")
df07.2<-as.tibble(df07.2)

df07<-full_join(df07.1, df07.2)
#dat_all3<-full_join(filter(dat_all, Year!=2007), df07)

df14.1<-select(filter(dat_all, Year==2014), -smolts, -schools)
df14.2<-select(filter(dat_all, Year==2014), smolts, schools)
#View(df14.2)

# scenario +-2 days from the peak missing
#704+ 698+ 1106+ 940+ 637 =4085
df14.2[36:40,1]<-rep(NA,5)
df14.2[36:40,2]<-rep(NA,5)
df14.2<-cbind(df14.2,1:92) 
colnames(df14.2)<-c("smolts", "schools", "day")
df14.2<-as.tibble(df14.2)

df14<-full_join(df14.1, df14.2)
df0714<-full_join(df07,df14)


dat_all3<-full_join(filter(dat_all, Year!=2007 & Year!=2014), df0714)
#View(dat_all3)
