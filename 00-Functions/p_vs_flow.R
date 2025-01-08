#################################
#  probability X vs flow


p_vs_flow<-function(C, a, b, sd, upr, lwr, Z){
  
#C<-chains
#a<-"aB_side"  
#b<-"bB_side"  
#sdX<-"sdBB_side"
# Z== T or F
#  upr<-0.6
#  lwr<-0.3

Flow<-seq(0,100, by=2)
nF<-length(Flow)

# pick chains for comparison
c1<-C[[1]]
#c2<-chainsP[[1]]

# Posterior

n_samp<-length(c1[,a])
a_samp<-c1[,a]
b_samp<-c1[,b]
sd_samp<-c1[,sd]

if(Z==T){z<-c1[,"z"]}

BB_samp<-array(NA, dim=c(n_samp,nF))
muB_samp<-array(NA, dim=c(n_samp,nF))
for(j in 1:n_samp){
  for(i in 1:nF){
    BB_samp[j,i]<-rnorm(1,a_samp[j]-b_samp[j]*Flow[i], sd_samp[j])
    if(Z==T){
      muB_samp[j,i]<-(0.9-z[j])*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+z[j]
    }else{
      muB_samp[j,i]<-upr*(exp(BB_samp[j,i])/(1+exp(BB_samp[j,i])))+lwr}
  }
}

df<-boxplot.df(muB_samp, Flow)

return(df)

}

#p_vs_flow(chains, "aB_side", "bB_side", "sdBB_side", 0.6, 0.3)
