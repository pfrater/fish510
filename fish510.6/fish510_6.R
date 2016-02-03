## R exercises for fish510.6
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.6')
# using data on cod to calculate a stock recruitment curve
sr <- read.table('sr.dat', header=T, as.is=T, check.names=F)
r <- sr$R
s <- sr$S


plot(R~S, data=sr, xlim=c(0,max(sr$S)))
bev.holt <- function(s, alpha, k) {
    r <- (alpha*s) / (1 + (s/k))
    return(r)
}
sse.bev.holt <- function(vals) {
    alpha <- vals[1]
    k <- vals[2]
    s <- sr$S
    r.hat <- bev.holt(s, alpha, k);
    r <- sr$R
    sse <- sum((r - r.hat)^2)
    return(sse)
}
nl.mod <- nls(R ~ bev.holt(S, alpha, k), data=sr, start=list(alpha=0.001, k=10))
curve(bev.holt(s=x, 10.28, 18.36), add=T, col='red')

r.hats <- bev.holt(s, alpha=10.28, k=18.36)
plot(sr$R ~ r.hats)

vals <- c(10.28, 18.36)
r.mins <- nlm(sse.bev.holt, p=c(10.28, 18.36))

plot(r~s, xlab='Spawning stock biomass', ylab='Number of recruits', xlim=c(0,max(s)))
curve(bev.holt(s=x, 10.28, 18.36), add=T, col='red')
curve(bev.holt(s=x, r.mins$estimate[1], r.mins$estimate[2]), add=T, col='blue')




# data requirements: data is needed to compute production and replacement curves
# can either use real data or simulate data
Linf<-160
k<-0.1
beta<-3
cond<-0.02
ages<-1:14
t0<-0
la<-Linf*(1-exp(-k*(ages-t0)))            # Mean length at age
la<-round(la,2)

selF1<-0
selF2<-0.25
selF3<-0.35
selF4<-1.1

wa<-cond*la**beta/1000                    # Mean weight at age in kg
wa<-round(wa,2)
s50<-5                                    # Age at 50% selection
sa<-round(1/(1+exp(-1.1*(ages-s50))),2)   # Selection at age
p50<-5.5
pa<-round(1/(1+exp(-2*(ages-p50))),2)   # Proportion mature at age

M<-0.2
par(mfrow=c(2,2))
plot(ages,la,type='l',lwd=3, xlab="Age (years)", ylab="Length (cm)", ylim=c(0,max(la)))
plot(ages,wa,type='l',lwd=3, xlab="Age (years)", ylab="Weight (kg)")
plot(ages,sa,type='l',lwd=3, xlab="Age (years)", ylab="Selection")
plot(ages,pa,type='l',lwd=3, xlab="Age (years)", ylab="Proportion mature")

source('srfun.r')

# parameters for a stock-recruit relationship
alpha <- 0.5
K <- 20000

# parameters can also be simulated by fixing the collapse fishing mortality (Fcrash) and compute the corresponding alpha
alpha <- 1/(srfun(selF4, M, sa, wa, pa))
K <- 20000

# then compute the production and replacement curves and look at effects of fishing mortality
srange <- 0:2000*100
rhat <- alpha*srange/(1+(srange/K))

plot(rhat~srange, type='l', xlab='S (000 t)', ylab='R (millions)')

sr1 <- srfun(selF1, M=M, sa=sa, wa=wa, pa=pa)
sr2 <- srfun(selF2, M=M, sa=sa, wa=wa, pa=pa)
sr3 <- srfun(selF3, M=M, sa=sa, wa=wa, pa=pa)
sr4 <- srfun(selF4, M=M, sa=sa, wa=wa, pa=pa)

lines(srange, srange/sr1)
lines(srange, srange/sr2)
lines(srange, srange/sr3)
lines(srange, srange/sr4)


# I'm curious about what the beverton-holton stock equation looks like without fishing pressure
srange <- 0:2000*100
alpha <- 0.5
K <- 20000
curve(bev.holt(x, alpha, K), 0, max(srange))
s <- 1000;
r <- 100;
pop <- 100;
temp.r <- 100;
temp.s <- s;
while (s < max(srange)) {
    new.r <- bev.holt(s, alpha, K);
    lines(x=c(s,s), y=c(temp.r, new.r));
    sr <- srfun(1.5, M, sa, wa, pa)
    s <- new.r*sr;
    if (temp.s == s) break
    lines(srange, srange/sr)
    lines(x=c(temp.s, s), y=c(new.r, new.r));
    temp.r <- new.r;
    temp.s <- s;
    pop <- c(pop, new.r);
}


# computing spawning stock biomass and yield from the recruitment curves
source('yrfun.r')
alpha <- 1/srfun(selF4, M, sa, wa, pa)
K <- 20000

sr<-srfun(selF2,M,sa,wa,pa)
yr<-yrfun(selF2,M,sa,wa)
S<-K*(alpha*sr-1)
R<-alpha*S/(1+S/K)
Y<-yr*R


alpha<-1/srfun(selF4,M,sa,wa,pa) # Make this Fcrash
K<-20000
Frange<-(0:(selF4*100))/100 # Range for plotting
sr<-sapply(Frange,srfun,M,sa,wa,pa) # Compute biomass per recruit for all F
yr<-sapply(Frange,yrfun,M,sa,wa) # Compute yield per recruit for all F
Srange<-K*(alpha*sr-1) # Compute equilibrium SSB
Srange<-ifelse(Srange>0,Srange,0) # set to zero if negative
Rrange<-alpha*Srange/(1+Srange/K) # Compute equilibrium recruitment
Yrange<-yr*Rrange # Compute the equilibrium yield
par(mfrow=c(2,2)) #
plot(Frange,yr,type='l',lwd=3) # Plot yield per recruit vs F
plot(Frange,sr,type='l',lwd=3) # Plot biomass per recruit vs F
plot(Frange,Srange,type='l',lwd=3) # Plot equilibrium SSB
plot(Frange,Yrange,type='l',lwd=3) # Plot equilibrium yield


