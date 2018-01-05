load mix
model in "model.txt"
data in "data.txt"
compile, nchains(1)
parameters in "inits2.txt", chain(1)
initialize
adapt 5000
update 500000
monitor aD, thin(400)
monitor bD, thin(400)
monitor cvD, thin(400)
monitor cvmuD, thin(400)
monitor sums1, thin(400)
monitor sums2, thin(400)
monitor aP, thin(400)
monitor bP, thin(400)
monitor sdP, thin(400)
monitor etaB, thin(400)
monitor aB, thin(400)
monitor bB, thin(400)
monitor sdBB, thin(400)
monitor eta_alphaN, thin(400)
monitor Ntot, thin(400)
monitor N, thin(400)
update 800000
parameters to "out2.Rdump", chain(1)
coda *, stem(sim.2/CODA)
samplers to sim.2/samplers.csv
update 0
model clear
exit
