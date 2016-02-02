## code corresponding to examples in fish510.5

# simulating a stock
linf <- 160;
k <- 0.1;
beta <- 3;
cond <- 0.02;
ages <- 1:14;
t0 <- 0;
la <- linf*(1-exp(-k*(ages-t0)));

# mean length at ages
la <- round(la, 2)
wa <- cond*la**beta/100

# mean weight at age in kg
wa <- round(wa,2)
s50 <- 5

# age at 50% selection
sa <- round(1/(1+exp(-1.1*(ages-s50))), digits=2)

# selection at age
p50 <- 5.5
pa <- round(1/(1 + exp(-2*(ages-p50))), digits=2)

# proportion mature at age
m <- 0.2

### first year yield and stock size
fmult <- 0.5
fmort <- fmult*sa[1]
z <- fmort + m[1]
r <- 1000
n1 <- r
c1 <- (fmort/z)*(1-exp(-z))*n1
w1 <- wa[1]
y1 <- w1*c1
n2 <- n1*exp(-z)

# computing the second year
# the equations are exactly the same for the second year, so I'm just going to write a function to do it instead

# this doesn't work yet
stocksize <- function(r, fmult, ages, sa, wa, m) {
    n.i <- r;
    n <- NULL; w <- NULL; c <- NULL; y <- NULL;
    for (i in 1:ages) {
        fmort <- fmult*sa[i]
        z <- fmort + m[i]
        n.i <- n
        c.i <- (fmort/z)*(1-exp(-z))*n.i
        w.i <- wa[i]
        y.i <- w.i*c.i
        n.i <- n.i*exp(-z)
        n <- c(n, n.i)
        c <- c(c,c.i)
        w <- c(w, w.i)
        y <- c(y, y.i)
    }
    return(data.frame(age=ages, pop=n, catch=c, weight=w, yield=y))
}

# using a multiplier (fmult) of 1
M <- 0.2
Fmult<-1 
Fmort<-Fmult*sa 
Z<-Fmort+M 
prop<-(Fmort/Z)*(1-exp(-Z)) 
Ztemp<-c(0,Z[1:(length(Z)-1)]) 
cumZ<-exp(-cumsum(Ztemp)) 
C<-prop*cumZ 
Y<-sum(w*C) 
cbind(Fmort,Z,C=round(C*1000))


# using a multiplier (fmult) of 0.5
Fmult<-0.5 
Fmort<-Fmult*sa 
Z<-Fmort+M 
prop<-(Fmort/Z)*(1-exp(-Z)) 
Ztemp<-c(0,Z[1:(length(Z)-1)]) 
cumZ<-cumsum(Ztemp) 
C<-prop*exp(-cumZ) 
Y<-sum(w*C) 
names(Fmort)<-as.character(3:14) 
cbind(Fmort,Z,C=round(C*1000))

### putting it all together
# this computes theoretical length, weight, catch, yield, and eventually yield per recruit
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

yrfun<-function(Fmult,M,sa,wa){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    prop<-(Fmort/Z)*(1-exp(-Z))
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-exp(-cumsum(Ztemp))
    C<-prop*cumZ
    Y<-sum(wa*C)
    return(Y)
}
Fvec<-(0:150/100)
yr<-sapply(Fvec,yrfun,M,sa,wa)
plot(Fvec,yr,type='l',xlab="F", ylab="Y divided by R (kg)")


# setting up a vector of cumulative Z-values
# this is from 510.5.2.7; this is supposed to compute yield per recruit, but I'm not exactly sure how
Fmult <- 1
M <- 0.2
Fmort<-Fmult*sa 
Z<-Fmort+M 
prop<-(Fmort/Z)*(1-exp(-Z)) 
Ztemp<-c(0,Z[1:(length(Z)-1)]) 
cumZ<-cumsum(Ztemp) 
C<-prop*exp(-cumZ) 
Y<-sum(wa*C)
cbind(Fmort,Z,C=round(C*1000))

# viewing the results of reduced effort
Fmult<-0.5 
Fmort<-Fmult*sa 
Z<-Fmort+M 
prop<-(Fmort/Z)*(1-exp(-Z)) 
Ztemp<-c(0,Z[1:(length(Z)-1)]) 
cumZ<-cumsum(Ztemp) 
C<-prop*exp(-cumZ) 
Y<-sum(wa*C) 
names(Fmort)<-as.character(3:14) 
cbind(Fmort,Z,C=round(C*1000))


# looking at yield per recruit across different fishing mortalities
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

yrfun<-function(Fmult,M,sa,wa){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    prop<-(Fmort/Z)*(1-exp(-Z))
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-exp(-cumsum(Ztemp))
    C<-prop*cumZ
    Y<-sum(wa*C)
    return(Y)
}

Fvec<-(0:150/100)
yr<-sapply(Fvec,yrfun,M,sa,wa)
plot(Fvec,yr,type='l',xlab="F", ylab="Y divided by R (kg)")



## Yield per recruit and age composition
# Input some data
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.5')
mm<-read.table("base95.dat",as.is=T,check.names=F)
mm<-as.data.frame(t(mm))
natmort<-mm$M
wt.at.age<-mm$w
selpat<-mm$sa
# Define the yield-per-recruit function
yrfun<-function(Fmult,M=natmort,sa=selpat,wa=wt.at.age){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    prop<-(Fmort/Z)*(1-exp(-Z))
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-cumsum(Ztemp)
    C<-prop*exp(-cumZ)
    Y<-sum(wa*C)
    return(Y)
}

# Set up the data for a Y/R plot
Fvec<-seq(0,1.5,0.01)
yr<-sapply(Fvec,yrfun,natmort,selpat,wt.at.age)

# Do histograms of #s and/or bio by age onto a Y/R-plot
# The Y/R-plot

par(fig=c(0,1,0,0.8))
plot(Fvec,yr,type='l',xlab="F", ylab="Y/R (kg)",lwd=2,ylim=c(0,2.5))


# Add points for a low and a high fishing mortality
Flomult<-0.25
Fhimult<-1
yrFlomult<-yrfun(Flomult)
yrFhimult<-yrfun(Fhimult)
points(Flomult,yrFlomult,col="green",pch=19)
arrows(Flomult,yrFlomult,0.35,1.1)
points(Fhimult,yrFhimult,col="green",pch=19)
arrows(Fhimult,yrFhimult,1.15,2.25)


# Histogram for lo F
par(fig=c(0.15,.65,0.1,.6),new=T)
Fmort<-Flomult*selpat
Z<-Fmort+natmort
prop<-(Fmort/Z)*(1-exp(-Z))
Ztemp<-c(0,Z[1:(length(Z)-1)])
cumZ<-cumsum(Ztemp)
C<-prop*exp(-cumZ)
Y<-sum(wt.at.age*C)
names(Fmort)<-as.character(3:14)
#cbind(Fmort,Z,C=round(C*1000))
names(C)<-as.character(3:14)
barplot(C,space=0,main="C-no, F=0.25",axes=F,ylim=c(0,.16))

# Histogram for hi F
par(fig=c(0.5,1,0.5,1),new=T)
Fmort<-Fhimult*selpat
Z<-Fmort+M
prop<-(Fmort/Z)*(1-exp(-Z))
Ztemp<-c(0,Z[1:(length(Z)-1)])
cumZ<-cumsum(Ztemp)
C<-prop*exp(-cumZ)
Y<-sum(wt.at.age*C)
names(Fmort)<-as.character(3:14)
#cbind(Fmort,Z,C=round(C*1000))
names(C)<-as.character(3:14)
barplot(C,space=0,main="C-no, F=1.0",axes=F,ylim=c(0,.16))



## simulated yield per recruit analysis
source("simstock.r") 
source("yr.r") 
Fmort<-(0:150)/100 
YR<-sapply(Fmort,yrfun,M,sa,wa) 
Fmax<-Fmort[YR==max(YR)] 
YRmax<-max(YR) 
plot(Fmort,YR,type='l',xlab="Fishing mortality", ylab="Y/R (kg)") 
points(Fmax,YRmax) 
lines(c(Fmax,Fmax,-0.1),c(-0.1,YRmax,YRmax)) 
text(Fmax,YRmax+0.05, paste("Fmax=",Fmax," YRmax=",round(YRmax,2)))   
Fvec<-(-1:150)/100 
n<-length(Fvec) 
nm1<-n-1 
nm2<-n-2 
a<-Fvec[1:nm2] 
b<-Fvec[3:n] 
x<-Fvec[2:nm1] 
yr<-sapply(Fvec,yrfun,M,sa,wa) 
Dyr<-(yr[3:n]-yr[1:nm2])/(b-a) 
D0<-Dyr[1] 
F0.1<-min(x[Dyr<=D0*0.1]) 
YR0.1<-yr[Fvec==F0.1] 
lines(c(F0.1,F0.1,-0.1),c(-0.1,YR0.1,YR0.1)) 
text(F0.1,YR0.1+0.05,  paste("F0.1=",F0.1," YR0.1=",round(YR0.1,2)),pos=2)   
title("Yield per recruit analysis for a simulated stock")



## fishing at F_0.1
Fvec<-(-1:150)/100 
n<-length(Fvec) 
nm1<-n-1 
nm2<-n-2 
a<-Fvec[1:nm2] 
b<-Fvec[3:n] 
x<-Fvec[2:nm1] 
yr<-sapply(Fvec,yrfun,M,sa,wa) 
Dyr<-(yr[3:n]-yr[1:nm2])/(b-a) 
D0<-Dyr[1] 
F0.1<-min(Fvec[Dyr<=D0*0.1])



