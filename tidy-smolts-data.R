# Tidy data from Utsjoki smolt count (video data)
################################################


library(tidyverse)

# Smolts
############
#S<-read.table(paste(sep="",pathIn,"Smolts_6years_2008weekendsOff.txt"),header=T)
#S<-read_tsv(paste(sep="",pathIn,"Smolts_6years.txt"))
S<-read_tsv(paste(sep="",pathIn,"Smolts_6years_2008weekendsOff.txt"))
# Leave S as such, goes to JAGS model as input
day<-tibble(Day=1:61)
S2<-bind_cols(S, day)
rename(S2, N03=2003)

,N04=`2004`,N05=`2005`,N06=`2006`,N08=`2008`,N14=`2014`)

colnames(S2)<-c(2003:2006,2008,2014, "day")

# re-arrange  
ts1<- S2%>%
  gather(key="year", value="num_smolts", `2003`:`2014`)
# put year first
ts1<-select(ts1, year, everything()) 

# Temperature
#temp1<-read.table(paste(sep="",pathIn,"Temperature_4years.txt"),header=T)
temp1<-read_tsv(paste(sep="",pathIn,"Temperature_4years.txt"))
source("data-water-temperature.r")
datT<-cbind(temp1[,1:2],round(DailyTemp,1), temp1[,3],temp1[,4],round(DailyTemp14,1))
temp2<-cbind(datT,1:61)
colnames(temp2)<-c(2003:2006,2008,2014, "day")
(temp2<-as.tibble(temp2))

tt1<- temp2%>%
  gather(key="year", value="temperature", `2003`:`2014`)
tt1

# Join datasets togeher (natural join)
(ts2<-full_join(ts1,tt1, by=NULL))

# Flow
flow<-read.table(paste(sep="",pathIn,"FlowPatoniva_03-14.txt"), header=T)
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
