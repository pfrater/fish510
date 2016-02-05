# a dynamic biomass model
B0<-150
K<-1000
r<-1
MSY<-r*K/4   # Maximum sustainable yield
BMSY<-K/2    # Biomass corresponding to maximum sustainable yield

p<-0.2       # Proportion of biomass caught

Bvec<-NULL
Yvec<-NULL
prevB<-B0

for(y in 1:100){  # Run for 100 years
    #  Y<-250         # Constant yield
    Y<-p*prevB      # Constant effort
    nextB<-prevB+r*prevB*(1-prevB/K)-Y
    Bvec<-c(Bvec,prevB)
    Yvec<-c(Yvec,Y)
    prevB<-nextB
}

Blevels<-0:K
SYlevels<-r*Blevels*(1-Blevels/K)
plot(Blevels,SYlevels,type='l',lwd=2,col="blue")
lines(Bvec,Yvec,lwd=2,col="red",type='b')