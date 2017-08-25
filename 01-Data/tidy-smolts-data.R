# Tidy data from Utsjoki smolt count (video data)
################################################


library(tidyverse)

# Smolts
############
#S<-read.table(paste(sep="",pathIn,"Smolts_6years_2008weekendsOff.txt"),header=T)
S_all<-read_tsv(paste(sep="",pathIn,"Smolts_6years.txt"))
S<-read_tsv(paste(sep="",pathIn,"Smolts_6years_2008weekendsOff.txt"))
# Leave S as such, goes to JAGS model as input

#day<-tibble(Day=1:61)
#S2<-bind_cols(S, day)
S2<-mutate(S_all, day=1:61) # same as previous but with one row

#tmp<-dplyr::rename(S2, N03=2003) # should work but does not
S2<-setNames(S2,c(2003:2006,2008,2014, "day"))

# re-arrange  
ts1<- S2%>%
  gather(key="year", value="num_smolts", `2003`:`2014`)
# put year first
ts1<-select(ts1, year, everything())

# Temperature
temp1<-read_tsv(paste(sep="",pathIn,"Temperature_4years.txt"))
source("01-Data/data-water-temperature.r")

datT<-bind_cols(temp1[,1:2],round(DailyTemp05,1), temp1[,3],temp1[,4],round(DailyTemp14,1))
temp2<-mutate(datT, day=1:61)
temp2<-setNames(temp2,c(2003:2006,2008,2014, "day"))

tt1<- temp2%>%
  gather(key="year", value="temperature", `2003`:`2014`)
tt1

# Join smolt and temp datasets togeher (natural join)
(ts2<-full_join(ts1,tt1, by=NULL))

# Flow
flow<-read_tsv(paste(sep="",pathIn,"FlowPatoniva_03-14.txt"))
datF<-select(flow, flow03:flow06, flow08, flow14)

tf1<-mutate(flow,day=1:61)
tf1<-setNames(tf1,c(2003:2014, "day"))

tf1<- tf1%>%
  gather(key="year", value="flow", `2003`:`2014`)
tf1

# Join datasets together (natural join)
ts<-full_join(ts2,tf1, by=NULL)
filter(ts, is.na(flow)==T) 
#View(ts)
