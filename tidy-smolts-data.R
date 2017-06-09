# Tidy data from Utsjoki smolt count (video data)
################################################


library(tidyverse)

path<-"H:/Projects/ISAMA/prg/Utsjoki_smolts/"

# Smolts
S<-read.table(paste(sep="",path,"input/Smolts_6years.txt"),header=T)
S2<-cbind(S, 1:61)
colnames(S2)<-c(2003:2006,2008,2014, "day")
(S2<-as.tibble(S2))

# re-arrange  
ts1<- S2%>%
  gather(key="year", value="num_smolts", `2003`:`2014`)
# put year first
ts1<-select(ts1, year, everything()) 

# Temperature
temp1<-read.table(paste(sep="",path,"input/Temperature_4years.txt"),header=T)
source(paste(sep="",path,"model/UtsjokiWaterTemperature2005.r"))
source(paste(sep="",path,"model/UtsjokiWaterTemperature2014.r"))
temp<-cbind(temp1[,1:2],round(DailyTemp,1), temp1[,3],temp1[,4],round(DailyTemp14,1))
temp<-cbind(temp,1:61)
colnames(temp)<-c(2003:2006,2008,2014, "day")
(temp<-as.tibble(temp))

tt1<- temp%>%
  gather(key="year", value="temperature", `2003`:`2014`)
tt1

# Join datasets togeher (natural join)
ts2<-full_join(ts1,tt1, by=NULL)

# Flow
flow<-read.table(paste(sep="",path,"input/FlowPatoniva_03-14.txt"), header=T)
datF<-cbind(flow[,1:4], flow[,6], flow[,12])
flow<-cbind(flow,1:61)
colnames(flow)<-c(2003:2014, "day")
(tf1<-as.tibble(flow))

tf1<- tf1%>%
  gather(key="year", value="flow", `2003`:`2014`)
tf1

# Join datasets togeher (natural join)
ts<-full_join(ts2,tf1, by=NULL)
filter(ts, is.na(flow)==T) 
#View(ts)


