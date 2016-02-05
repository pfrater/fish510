B0<-1000
K<-1000
r<-1
Y<-200

B<-B0
prevB<-B0
nextB<-prevB+r*prevB*(1-prevB/K)-Y
B<-c(B,nextB)

prevB<-nextB
nextB<-prevB+r*prevB*(1-prevB/K)-Y
B<-c(B,nextB)

prevB<-nextB
nextB<-prevB+r*prevB*(1-prevB/K)-Y
B<-c(B,nextB)

prevB<-nextB
nextB<-prevB+r*prevB*(1-prevB/K)-Y
B<-c(B,nextB)

prevB<-nextB
nextB<-prevB+r*prevB*(1-prevB/K)-Y
B<-c(B,nextB)

Blevels<-0:1000
EYlevels<-r*Blevels*(1-Blevels/K)
plot(Blevels,EYlevels,type='l',lwd=2,col="blue")
lines(B,rep(Y,length(B)),lwd=2,col="red")