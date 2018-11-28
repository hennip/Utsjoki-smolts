
source("01-Data/data-smolts-covariates.r")


# Data
##########

# Number of smolts
#ggplot(filter(dat_all, Year==2005), aes(x=day, y=smolts))+
#  geom_point()+
#  geom_line()

# number of observed smolts vs average school size per day
ggplot(dat_all)+
  geom_point(aes(x=smolts, y=schools))



## ---- draw-some-data-smolts ----
ggplot(dat_all,aes(x = day, y = smolts)) + 
  geom_point()+
  geom_line()+
facet_wrap(~Year)


# Temperature
## ---- draw-some-data-temp ----
dat<-filter(dat_all, (Year<2010 & Year>2004) |Year==2014)%>%
mutate(Year=as.factor(Year))

#dat<-dat_all
plot1 <- 
  ggplot(dat)+
  geom_line(aes(x = day, y = meanTemp, color=Year, linetype=Year), size=1.1)+
  scale_linetype_manual(values=rep(c("solid","longdash"),3))+
  scale_color_manual(values = rep(c("black","grey50", "grey75"),2))+
  #  scale_color_manual(values=c(grey.colors(4, start = 0.3, end = 0.7, gamma = 2.2, alpha = NULL)))
  #scale_colour_grey()+
  labs(x="Day (June-July)", y=expression(Temperature~(degree*C)), color="Year")+ 
  theme_bw()+
  theme(legend.position="none")+ # removes legend
  theme(title = element_text(size=15), axis.text = element_text(size=12), 
        strip.text = element_text(size=15))

# Flow
## ---- draw-some-data-flow ----
dat<-filter(dat_all, (Year<2010 & Year>2004) |Year==2014)%>%
  mutate(Year=as.factor(Year))
#dat<-dat_all
plot2 <-
  ggplot(dat)+
  geom_line(aes(x = day, y = flow, color=Year, linetype=Year), size=1.1)+
  scale_linetype_manual(values=rep(c("solid","longdash"),3))+
  scale_color_manual(values = rep(c("black","grey50", "grey75"),2))+
  #scale_colour_grey()+
  coord_cartesian(ylim=c(0,160))+
  labs(x="Day (June-July)", y=expression("Discharge (m"^{3}*"/s)"), color="Year")+
  theme_bw()+
  theme(legend.position = c(0.7,0.75), legend.key.width = unit(1.5,"cm") )+
  theme(title = element_text(size=15), axis.text = element_text(size=12), 
      strip.text = element_text(size=15))

#windows()
grid.arrange(plot1, plot2, ncol=2)

ggplot(mort3, aes(x = year, y = BCmort, col = State, linetype = State)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_manual(values = c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3"))) +
  opts(title = "BC mortality") +
  theme_bw()


#View(dat_all)


