# Calculate daily mean temperatures


dat1<-read.table(paste(sep="", pathIn,"Temperature_2005.txt"),header=T)
dat1
mean(subset(dat1,day==21 & month==6)$temperature)


day2<-c()
for(i in 1:length(dat1$temperature)){
  ifelse(dat1$month[i]==7,day2[i]<-dat1$day[i]+30,day2[i]<-dat1$day[i])
}
dat1$day2<-day2

DailyTemp<-c()
for(t in 1:61){
  apu<-0
  n<-0
  for(i in 1:length(dat1$temperature)){
    if(dat1$day2[i]==t){apu<-apu+dat1$temperature[i]; n<-n+1}
  }
  DailyTemp[t]<-apu/n
}
cbind(1:61,DailyTemp)


# 2014
dat1<-read.table(paste(sep="", pathIn,"Temperature_2014.txt"),header=T)
dat1

day2<-c()
for(i in 1:length(dat1$temperature)){
  ifelse(dat1$month[i]==7,day2[i]<-dat1$day[i]+30,day2[i]<-dat1$day[i])
}
dat1$day2<-day2

DailyTemp14<-c()
for(t in 1:61){
  apu<-0
  n<-0
  for(i in 1:length(dat1$temperature)){
    if(dat1$day2[i]==t){apu<-apu+dat1$temperature[i]; n<-n+1}
  }
  DailyTemp14[t]<-apu/n
}
cbind(1:61,DailyTemp14)






