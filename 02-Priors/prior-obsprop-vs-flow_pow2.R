
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

# Panun expert-info 16.6.17:
# Mahdolliset arvot välillä [0.9,0.3], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.75-0.9), 
# tästä verkkaisesti laskee (ei romahda) minimiin, jossa kuitenkin 
# suuri epävarmuus (0.3-0.6), jakauma ennemmin tasainen kuin huipukas.
# Virtaamilla 50-60 m3/s olosuhteet havaita ovat selkeästi huonot.
# Kun virtaama nousee 10m3/s->60m3/s, veden korkeus nousee metrin.

# Panun expert-info 10.11.22:
# Sivu-uomassa mahdolliset arvot välillä [0.95,0.45], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.95-0.9), 
# tästä lähtee laskemaan kun virtaama kasvaa. 
# Samankaltainen käyrä kuin keskiosan hav tn:llä
# mutta havaittavuus pysyy pitempään korkeana

# Yhdistetään aikaisemmat prior-obsprop-vs-flow - tiedostot ja poistetaan ylimääräiset
# -> yksi perustiedosto, gitissä versiot

Flow<-seq(0,100, by=0.3)
nF<-length(Flow)

# Nämä kun alarajaa ei ole
a<-3.5
b<--0.08
c<--0.0005
# Alaraja 0.3
#a<-2.5
#b<-0.15
mu<-c();P<-c();p<-c();sd<-c()
sd<-.5
for(i in 1:nF){
  mu[i]<-a+b*Flow[i]+c*Flow[i]^2
  
  P[i]<-rnorm(1,mu[i],sd)
  #p[i]<-0.6*(exp(P[i])/(1+exp(P[i])))+0.3
  p[i]<-0.9*(exp(P[i])/(1+exp(P[i]))) # ei alarajaa
}

tF<-as_tibble(cbind(Flow,p))

ggplot(tF) + 
  geom_point(aes(Flow, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b," c=",c))

# ================================================

Flow<-seq(0,100, by=2)
nF<-length(Flow)

M2<-"
model{
for(i in 1:n){
p[i]<-0.9*p2[i]
#p[i]<-(0.9-z)*p2[i]+z

logit(p2[i])<-P[i]
P[i]~dnorm(mu[i],tau)
mu[i]<-a-b*Flow[i]-c*pow(Flow[i],2)


mu_side[i]<-a_side-b_side*Flow[i]-c_side*pow(Flow[i],2)
P_side[i]~dnorm(mu_side[i],tau_side)
p_side[i]<-0.5*p2_side[i]+0.45
#p_side[i]<-0.6*p2_side[i]+0.3 # only to compare curves with the same limits
logit(p2_side[i])<-P_side[i]

}
tau<-1/pow(sd,2)
tau_side<-1/pow(sd_side,2)
coef_side~dbeta(2,2)T(0.02,0.98) # for a_side

a~dnorm(3.5,10)

#b~dlnorm(-4,10)
b~dlnorm(log(0.08)-0.5/10,10)
# b~dlnorm(log(0.08)-0.5/tau_b,tau_b)
# cv_b<-0.2
# tau_b<-1/log(cv_b*cv_b+1)

c~dlnorm(log(0.0005)-0.5/10,10)
#c~dlnorm(log(0.0005)-0.5/tau_c,tau_c)
#tau_c<-1/log(cv_c*cv_c+1)
#cv_c<-0.2

sd~dlnorm(0.01,20)



#Sivuoma vapaasti päivittyvillä prioreilla
a_side = a*(coef_side*0.5+1)
b_side~dlnorm(-2.5,10)
c_side~dlnorm(-7.5,10)
sd_side~dlnorm(0.01,20)


}"

cat(M2,file="prior-obs.txt")

data<-list( 
  Flow=Flow, n=nF
)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p", "p_side"#, "z"
                                  ),
                                  n.iter=5000,
                                  thin=1))

#summary(chains1[,"z"])

df<-boxplot.jags.df(chains1,"p",Flow)
df<-as_tibble(df)
df<-filter(df, x>=0)
df_side<-boxplot.jags.df(chains1,"p_side",Flow)
df_side<-as_tibble(df_side)
d_sidef<-filter(df_side, x>=0)

ggplot(df, aes(x, group=x))+
  geom_boxplot(
    data=df_side,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  coord_cartesian(ylim=c(0,1), xlim=c(0,100))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  labs(x="Flow", y="Probability to be observed", title="Obs prop mid stream/ side stream")



filter(df, x==10 |x==15 | x==20 |x==50 |x==60|x==80)

