
#source("00-Functions/tidy-functions.r")
#source("00-Functions/my-palette.r")
#source("00-Functions/packages-and-paths.r")
#source("01-Data/tidy-smolts-data.r")

# Data
##########

# Number of smolts
ggplot(data=filter(ts,year==2005), mapping=aes(x=day,y=num_smolts))+
  geom_point()+
  geom_line()

## ---- draw-some-data-smolts ----
ggplot(data = ts1,mapping = aes(x = day, y = num_smolts)) + 
  geom_point()+
  geom_line()+
  facet_grid(.~year)

# Temperature
## ---- draw-some-data-temp ----
ggplot(data = filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(mapping = aes(x = day, y = temperature, color=year), size=1.2)

# Flow
## ---- draw-some-data-flow ----
ggplot(data = filter(ts, year==2003:2006 | year==2008 | year==2014)) + 
  geom_line(mapping = aes(x = day, y = flow, color=year), size=1.2)

