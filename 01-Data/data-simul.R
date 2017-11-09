# Replace 07 data with simulated. Add NA's where desired.

Nsimul<-readRDS(file="N_simul.rds")

df07<-select(filter(dat_all, Year==2007), -smolts, -schools)
df_tmp<-cbind(Nsimul,1:92, rep(NA, 92)) 
colnames(df_tmp)<-c("smolts","day", "schools")
df_tmp<-as.tibble(df_tmp)

df07<-full_join(df07, df_tmp)
dat_all2<-full_join(filter(dat_all, Year!=2007), df07)
View(dat_all2)


