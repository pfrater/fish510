# example for an overfished stock

B0<-1000
K<-1000
r<-1

B<-0.95*B0
nextB<-B
Bseq<-NULL
Yseq<-NULL

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)

prevB<-nextB
SY<-r*prevB*(1-prevB/K)
Y<-1.5*SY
nextB<-prevB+SY-Y
Bseq<-c(Bseq,prevB)
Yseq<-c(Yseq,Y)


Blevels<-0:1000
EYlevels<-r*Blevels*(1-Blevels/K)
plot(Blevels,EYlevels,type='l',lwd=3,col="blue",
     ylim=c(0,max(c(EYlevels,Yseq))),xlab="B",ylab="Y")
lines(Bseq,Yseq,lwd=3,col="red",type='b')