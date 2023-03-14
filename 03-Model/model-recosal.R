#N: lohia syönnöksellä vuoden 1 alussa
NT[1]<-100000 # Tenon lohet
NO[1]<-500000 # Muut pohjoisnorjan villit lohet

# Rannikkokalastus sekakantakalastusta, oletetaan sama HR
CC[1]<-(NT[1]+NO[1])*HR # Rannikkosaalis
HR<-0.25
# Fc pitäisi rakentaa niin että kun NO:n määrä vähenee, 
# HR NT:lle kasvaisi
Fc<-f(HR)
# M:ään rakentuisi hitaasti kasvava kuolevuus
# -> 
M<-0.2
survc<- exp(-(Fc*M*3/12))
survr<- exp(-(Fr*M*5/12))
survo<- exp(-(M*4/12))

n_imm<-NT[1]*survc*(1-mat)
n_mat<-NT[1]*survc*mat

n_sp<-n_mat1*survr

eggs<-n_sp*fem_prop*fec

recr<-log(eggs/(alpha+beta*eggs))
# SR:käyrän pitäisi olla loivahko (steepness ei liian iso)
alpha<-1/slope
beta<-1/K

nps[i]<-recr[i-5]*Mps[i]

for(i in 2:10){
  nps[i]<-recr[i-5]*Mps[i]
  NO[i]<-NO[i-1]*0.95
  NT[i]<-n_imm1*survo+nps[i-1]
}

