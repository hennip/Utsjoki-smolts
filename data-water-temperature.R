# Calculate daily mean temperatures

#2005
# Use comment to leave out rows that start with #, as there were some points
# in which the temp logger was not functioning ok
(dat1<-read_tsv(paste(sep="", pathIn,"Temperature_2005.txt"), comment = "#"))
#dat1$month 
#filter(dat1, day==21, month==6)
#View(dat1)

dat1.june<-mutate(filter(dat1, month==6), day2=day)
dat1.july<-mutate(filter(dat1, month==7), day2=day+30)
dat1<-bind_rows(dat1.june,dat1.july)

DailyTemp<-c()
for(i in 1:61){
  DailyTemp[i]<-mean(filter(dat1, day2==i)$temperature)
}
DailyTemp05<-as.tibble(DailyTemp)
#View(mutate(DailyTemp05,day=1:61))


# 2014
(dat14<-read_tsv(paste(sep="", pathIn,"Temperature_2014.txt"), comment = "#"))

dat14.june<-mutate(filter(dat14, month==6), day2=day)
dat14.july<-mutate(filter(dat14, month==7), day2=day+30)
dat14<-bind_rows(dat1.june,dat14.july)

DailyTemp<-c()
for(i in 1:61){
  DailyTemp[i]<-mean(filter(dat14, day2==i)$temperature)
}
DailyTemp14<-as.tibble(DailyTemp)
#View(mutate(DailyTemp14,day=1:61))

