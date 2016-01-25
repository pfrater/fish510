## R code from fish510.3 - modeling length at age and length distributions

english=F
source('fishData.R')
l<-tapply(m$le,m$aldur,mean)
n<-tapply(m$le,m$aldur,length)
l<-l[n>=5]         # only include data if we have more than 4 values
a<-as.numeric(names(l))
plot(a,l,xlim=c(0,12),ylim=c(0,100),xlab="Age",ylab="Llength (cm)")

# modeling fish length as a function of time
# the von Bertalanffy model
t <- 0:25
k <- 0.5
t0 <- 0
linf <- 120
lhat <- linf*(1 - exp(-k*(t - t0)));
plot(t,lhat,ylim=c(0,120),type='l',lwd=2,xlab="Age",ylab="Theoretical length at age")
text(median(t),median(lhat),paste("K=",K," linf=",linf," t0=",t0),pos=4)
# next curve
k<-0.1
t0<-0
Linf<-120
lhat<-linf*(1-exp(-k*(t-t0)))
lines(t,lhat,col="blue",lwd=2)
text(median(t),median(lhat),paste("K=",k," linf=",linf," t0=",t0),pos=4)
# next curve
K<-0.1
t0<-0
linf<-80
lhat<-linf*(1-exp(-k*(t-t0)))
lines(t,lhat,col="red",lwd=2)
text(median(t),median(lhat),paste("K=",k," linf=",linf," t0=",t0),pos=4)


### computing a sum of squares for the von Bertalanffy model against actual data
# first we must put the above model as a function
vonb <- function(b) {
    linf <- b[1];
    k <- b[2];
    t0 <- b[3];
    lhat <- linf*(1-exp(-k*(t-t0)))
}

# set up function
sse.vonb <- function(b) {
    lhat <- vonb(b);
    s <- sum((l-lhat)^2);
    return(s)
}

t <- 2:6
b <- c(linf=120, k=0.16, t0=2)

plot(le~aldur, data=fish, col='red', cex=0.7)
points(vonb(b) ~ t, type='l')
sse.vonb(b)

# figuring out the correct parameters with 
fm <- nlm(sse.vonb, c(100, 0.1, 0))

# now plotting using the output from the above minimization
plot(a,l)
vb.par <- c(linf=fm$estimate[1], k=fm$estimate[2], t0=fm$estimate[3])
lhat <- vonb(vb.par)
lines(a, lhat)


## fish 510.3.3 - models of length distribution











