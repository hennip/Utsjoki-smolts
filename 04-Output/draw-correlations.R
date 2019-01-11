
#source("00-Functions/packages-and-paths.r")
# load simulations

chainsX<-chains
chainsX<-chainsP

aB<-chainsX[, "aB"]
bB<-chainsX[, "bB"]
aP<-chainsX[, "aP"]
bP<-chainsX[, "bP"]
aD<-chainsX[, "aD"]
bD<-chainsX[, "bD"]
sdBB<-chainsX[, "sdBB"]
Ntot1<-chainsX[, "Ntot[1]"] 
Ntot2<-chainsX[, "Ntot[2]"] 
Ntot3<-chainsX[, "Ntot[3]"] 
Ntot4<-chainsX[, "Ntot[4]"] 
Ntot5<-chainsX[, "Ntot[5]"] 
Ntot6<-chainsX[, "Ntot[6]"] 

aB<-c(aB[[1]], aB[[2]])
bB<-c(bB[[1]], bB[[2]])
sdBB<-c(sdBB[[1]], sdBB[[2]])
aP<-c(aP[[1]], aP[[2]])
bP<-c(bP[[1]], bP[[2]])
aD<-c(aD[[1]], aD[[2]])
bD<-c(bD[[1]], bD[[2]])

Ntot1<-c(Ntot1[[1]], Ntot1[[2]])
Ntot2<-c(Ntot2[[1]], Ntot2[[2]])
Ntot3<-c(Ntot3[[1]], Ntot3[[2]])
Ntot4<-c(Ntot4[[1]], Ntot4[[2]])
Ntot5<-c(Ntot5[[1]], Ntot5[[2]])
Ntot6<-c(Ntot6[[1]], Ntot6[[2]])


df1<-tibble(aB, bB, sdBB, aP, bP, aD, bD,
           Ntot1, Ntot2, Ntot3, Ntot4, Ntot5, Ntot6)

col1<-rgb(0,0,1,0.1)
col2<-rgb(1,0,0,0.1)

windows(record = T)
plot1<-ggplot(df1)+geom_point(aes(x=aB, y=bB), col=col1)+
  geom_point(data=df2,aes(x=aB, y=bB), col=col2)

plot2<-ggplot(df1)+geom_point(aes(x=aB, y=sdBB), col=col1)+
  geom_point(data=df2,aes(x=aB, y=sdBB), col=col2)

plot3<-ggplot(df1)+geom_point(aes(x=bB, y=sdBB), col=col1)+
  geom_point(data=df2,aes(x=bB, y=sdBB), col=col2)

grid.arrange(plot1, plot2, plot3, nrow=1, ncol=3)


windows(record = T)
plot1<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot1), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot1), col=col2)
plot2<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot2), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot2), col=col2)
plot3<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot3), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot3), col=col2)
plot4<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot4), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot4), col=col2)
plot5<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot5), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot5), col=col2)
plot6<-ggplot(df1)+geom_point(aes(x=aB, y=Ntot6), col=col1)+
  geom_point(data=df2,aes(x=aB, y=Ntot6), col=col2)

plot12<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot1), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot1), col=col2)
plot22<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot2), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot2), col=col2)
plot32<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot3), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot3), col=col2)
plot42<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot4), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot4), col=col2)
plot52<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot5), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot5), col=col2)
plot62<-ggplot(df1)+geom_point(aes(x=bB, y=Ntot6), col=col1)+
  geom_point(data=df2,aes(x=bB, y=Ntot6), col=col2)

#windows()
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=2, ncol=3)
grid.arrange(plot12, plot22, plot32, plot42, plot52, plot62, nrow=2, ncol=3)

plot1<-ggplot(df1)+geom_point(aes(x=aB, y=aP), col=col1)+
  geom_point(data=df2,aes(x=aB, y=aP), col=col2)
plot2<-ggplot(df1)+geom_point(aes(x=aB, y=bP), col=col1)+
  geom_point(data=df2,aes(x=aB, y=bP), col=col2)
plot3<-ggplot(df1)+geom_point(aes(x=bB, y=aP), col=col1)+
  geom_point(data=df2,aes(x=bB, y=aP), col=col2)
plot4<-ggplot(df1)+geom_point(aes(x=bB, y=bP), col=col1)+
  geom_point(data=df2,aes(x=bB, y=bP), col=col2)

plot5<-ggplot(df1)+geom_point(aes(x=aB, y=aD), col=col1)+
  geom_point(data=df2,aes(x=aB, y=aD), col=col2)
plot6<-ggplot(df1)+geom_point(aes(x=aB, y=bD), col=col1)+
  geom_point(data=df2,aes(x=aB, y=bD), col=col2)
plot7<-ggplot(df1)+geom_point(aes(x=bB, y=aD), col=col1)+
  geom_point(data=df2,aes(x=bB, y=aD), col=col2)
plot8<-ggplot(df1)+geom_point(aes(x=bB, y=bD), col=col1)+
  geom_point(data=df2,aes(x=bB, y=bD), col=col2)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, nrow=2, ncol=4)


