
source("01-Data/data-smolts-covariates.r")

# Data
##########

# Number of smolts
#ggplot(filter(dat_all, Year==2005), aes(x=day, y=smolts))+
#  geom_point()+
#  geom_line()

## ---- draw-some-data-smolts ----
ggplot(dat_all,aes(x = day, y = smolts)) + 
  geom_point()+
  geom_line()+
facet_wrap(~Year)

# Temperature
## ---- draw-some-data-temp ----
ggplot(dat_all)+
  #= filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(aes(x = day, y = meanTemp, color=as.factor(Year)), size=1.2)

# Flow
## ---- draw-some-data-flow ----
ggplot(dat_all)+
  #= filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(aes(x = day, y = flow, color=as.factor(Year)), size=1.2)+
  coord_cartesian(ylim=c(0,160))

#View(dat_all)


