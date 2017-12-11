
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
dat<-filter(dat_all, Year==2005:2006 | Year==2008 |Year==2014)
#dat<-dat_all
plot1 <- 
  ggplot(dat)+
  geom_line(aes(x = day, y = meanTemp, color=as.factor(Year)), size=1.1)+
#  scale_color_manual(values=c(grey.colors(4, start = 0.3, end = 0.7, gamma = 2.2, alpha = NULL)))
  scale_colour_grey()+
  labs(x="Day (June-July)", y=expression(Temperature~(degree*C)), color="Year")+ 
theme_bw()+
    theme(legend.position="none") # removes legend

# Flow
## ---- draw-some-data-flow ----
dat<-filter(dat_all, Year==2005:2006 | Year==2008 |Year==2014)
#dat<-dat_all
plot2 <- 
  ggplot(dat)+
  geom_line(aes(x = day, y = flow, color=as.factor(Year)), size=1.2)+
  scale_colour_grey()+
  coord_cartesian(ylim=c(0,160))+
  labs(x="Day (June-July)", y=expression("Flow  (m"^{3}*"/s)"), color="Year")+
  theme_bw()

grid.arrange(plot1, plot2, ncol=2)


#View(dat_all)


