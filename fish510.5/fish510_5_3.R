## R code for exercises found in fish510.5.3
alpha<-0.0005
K<-20000

Linf<-160
k<-0.1
beta<-3
cond<-0.02
ages<-1:14
t0<-0
la<-Linf*(1-exp(-k*(ages-t0)))            # Mean length at age
la<-round(la,2)
wa<-cond*la**beta/1000                    # Mean weight at age in kg
wa<-round(wa,2)

s50<-5                                    # Age at 50% selection
sa<-round(1/(1+exp(-1.1*(ages-s50))),2)   # Selection at age
p50<-5.5
pa<-round(1/(1+exp(-2*(ages-p50))),2)   # Proportion mature at age

M<-0.2
source('spawningAndYieldFunctions.r')
Fvec<-(0:150/100)

sr<-sapply(Fvec,srfun,M,sa,wa,pa)
plot(Fvec,sr,type='l',xlab="F", ylab="S devided by R (kg)")




## calculate and plot out the yield per recruit along with the spawning stock biomass per recruit
alpha<-0.0005
K<-20000

Linf<-160
k<-0.1
beta<-3
cond<-0.02
ages<-1:14
t0<-0
la<-Linf*(1-exp(-k*(ages-t0)))            # Mean length at age
la<-round(la,2)
wa<-cond*la**beta/1000                    # Mean weight at age in kg
wa<-round(wa,2)

s50<-5                                    # Age at 50% selection
sa<-round(1/(1+exp(-1.1*(ages-s50))),2)   # Selection at age
p50<-5.5
pa<-round(1/(1+exp(-2*(ages-p50))),2)   # Proportion mature at age

M<-0.2

Fvec<-seq(0,1.5,0.01)
yr<-sapply(Fvec,yrfun,M,sa,wa)
sr<-sapply(Fvec,srfun,M,sa,wa,pa)

par(mar=c(4.1,4.1,4.1,4.1))
plot(Fvec,yr,axes=F,type='l',
     xlab="Fishing mortality",
     ylab="Y/R",col="red",lwd=2)
axis(2)
axis(1)
par(new=T)
plot(Fvec,sr,ylab="",axes=F,type='l',xlab="",col="green",lwd=2)
axis(4)
mtext("S/R",side=4)



## calculates the 
Fvec<-(-1:150)/100 
n<-length(Fvec) 
nm1<-n-1 
nm2<-n-2 
a<-Fvec[1:nm2] 
b<-Fvec[3:n] 
x<-Fvec[2:nm1] 
yr<-sapply(Fvec,yrfun,M,sa,wa) 
Dyr <- (yr[3:n]-yr[1:nm2])/(b-a) 
D0<-Dyr[1] 
F0.1<-min(Fvec[Dyr<=D0*0.1])