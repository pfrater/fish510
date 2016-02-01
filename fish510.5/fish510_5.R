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





