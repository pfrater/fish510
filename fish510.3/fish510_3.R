## R code from fish510.3 - modeling length at age and length distributions
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode')

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


## fish 510.3.3.8 - models of length distribution
source('functions.r')
source('init.r') 
fm <- nlm(sseprop, p0)
p1 <- fm$estimate
p1 <- abs(p1)
pi <- c(p1, 1-sum(p1))
fit <- rep(0,130) 
for (lgrp in 1:130) {
    fit[lgrp] <- sum(pi*(pnorm((lgrp+0.5-mu)/sigma)-pnorm((lgrp-0.5-mu)/sigma)))
}
plot(lgrps,dat, type='b', lwd=2, xlab='Length (cm)', ylab='Frequency')
lines(lgrps,fit)



### fish510.3.5 - modelling length-weight relationships
m <- read.table('http://www.hafro.is/~gunnar/unu/codsample.dat', header=T)
l <- m$le
w <- m$osl
plot(w~l)
x <- log(l)
y <- log(w)
fit <- lm(y~x)
summary(fit)
plot(y~x)
abline(fit)
e <- resid(fit)
hist(e)
plot(l,e)



### fish510.3.6 - modeling the development of a length distribution
## an updating length distribution
# this is used to predict how fish 'should' move into the length distribution of the next age class
par(mfrow=c(2,3))
lsample <- floor((rnorm(1000)*5+25)/1)*1
l <- tapply(lsample, lsample, length)
lens <- as.numeric(names(l))
k <- 0.2
linf <- 120
vonb <- function(k, linf, ages) {
    return(linf*(1-exp(-k*ages)))
}
inversevonb <- function(k, linf, lengths) {
    return(-log(1-lengths/linf)/k)
}
n <- 40
finalLscale <- min(lens):(max(lens+n))
tempL <- rep(0.001, length(finalLscale))
names(tempL) <- as.character(finalLscale)
tempL[names(l)] <- l
barplot(tempL, space=0, main='Example pop freq at length \n single cohort',xlab='Length',ylab='Frequency')
ages <- 0:15
lengrp22 <- 22
freq22 <- l[as.character(lengrp22)]
deltal22 <- vonb(k, linf, inversevonb(k, linf, 22)+1)-22
plot(ages, vonb(k, linf, ages), type='l', main='von B length at age', ylab='Length (cm)', xlab='Age')

xvalues <- c(0, inversevonb(k, linf, 22), inversevonb(k, linf,22)+1, inversevonb(k,linf, 22)+1)
yvalues <- c(22,22,22,vonb(k, linf, inversevonb(k, linf,22)+1))
lines(xvalues, yvalues, type='l')
text(max(xvalues)+1, 22+deltal22/2, paste('Deltal=', round(deltal22), 'cm at l=22cm'), pos=4)

# set up binomial update method - just for length group 22
p <- deltal22/n
updatefreq <- dbinom(0:n, n, p)
names(updatefreq) <- as.character(0:n)
barplot(updatefreq, space=0, main='Updating length group 22 \n Binomial: n=40&n*p=Deltal',
        xlab='Length increment', ylab='Probability')

# now update the entire length distribution
deltal <- vonb(k, linf, inversevonb(k, linf, lens)+1) - lens
plot(lens, deltal, type='l', main='Mean growth by length group', xlab='Initial length (cm)')

pvec <- deltal/n
popfreq <- rep(0, length(finalLscale))
names(popfreq) <- as.character(finalLscale)

for (i in 1:length(lens)) {
    p <- pvec[i]
    initL <- lens[i]
    initfreq <- l[i]
    updatemat <- dbinom(0:n, n, p)
    finalL <- initL+(0:n)
    finalfreq <- initfreq*updatemat
    names(finalfreq) <- as.character(finalL)
    popfreq[names(finalfreq)] <- popfreq[names(finalfreq)] + finalfreq
}
barplot(popfreq, space=0, main='Updated length distribution', xlab='Length', ylab='Frequency')
plot(finalLscale, popfreq, type='l', lwd=2, col='blue')
lines(finalLscale, tempL, type='l', lwd=2, col='red')



######################################################
## end of tutorial practicals
######################################################
# write a von Bertalanffy function to predict length at age with specified parameters
vonb <- function(b) {
    linf <- b[1];
    k <- b[2];
    t0 <- b[3];
    len <- linf*(1-exp(-k*(age-t0)));
    return(len)
}
lengths <- vonb(linf=160, k=0.09, age=2:14)
plot(lengths~age, data=lengths, type='l')

lengths2 <- vonb(linf=120, k=0.05, age=2:14)
lines(lengths~age, data=lengths2, type='l', col='red')

lengths3 <- vonb(linf=80, k=0.09, age=2:14)
lines(lengths~age, data=lengths3, type='l', col='blue')

# function to produce sum of squared errors between vonb prediction and data
vb.sse <- function(b) {
    lhat <- vonb(b);
    sse <- sum((l-lhat)^2);
    return(sse)
}

### estimating a vonb equation from given data
L <- c(18, 24, 29, 32, 35)
age <- c(1.5, 2.5, 3.5, 4.5, 5.5)
l.t <- L[1:4]
l.t1 <- L[2:5]
length.mod <- lm(l.t1~l.t)
length.coef <- coef(length.mod)

pred.lengths <- vonb(linf=(length.coef[1] / (1-length.coef[2])), k= (-log(length.coef[2])), age=time)
plot(L~time)
points(lengths~age, data=pred.lengths, type='l')

est <- nlm(vb.sse, c(100, 0.1, 0))
pred.lengths <- vonb(est$estimate)
plot(L~time)
points(lengths~age, data=pred.lengths, type='l')

### models of length distribution
# viewing simple gaussian distribution of lengths
le <- seq(15,30,1)
plot(le, dnorm(le, 23, 1.4), lwd=2)
lines(le, dnorm(le, 23, 1.4))

# assuming the length group is -0.5 to +0.5 of the mean
# we use the cumulative distribution of the density instead
plot(le, pnorm((le-23)/1.4), type='o')
leA <- seq(15.5, 30.5, 1)
leB <- seq(14.5, 29.5, 1)
rbind(leA, leB)

d2 <- pnorm((leA-23)/1.4) - pnorm((leB-23)/1.4)
plot(le,d2, type='o')

# one way to do it is to look at peaks in proportion of lengths ~ lengths and specify age classes
# another is to use a von Bertalanffy growth curve to see what length fish 'should' be at certain age
age <- 1:8
mu <- vonb(c(60, 0.18, 0))
sdev <- rep(2.4, length(age))
le <- 1:60
leA <- seq(1.5, 60.5, 1)
leB <- seq(0.5, 59.5, 1)

n <- 1
m <- 0.3
for (i in age) {
    n <- c(n, exp(-m)*n[i])
}

ntot <- sum(n)
p <- n/ntot

# calculating length distributions for each age
ldist <- NULL
for (i in age) {
    d2 <- (pnorm((leA-mu[i])/sdev[i]) - pnorm((leB-mu[i])/sdev[i]))*p[i];
    ldist <- rbind(ldist, d2)
}
matplot(le, t(ldist), type='l')
plot(le, apply(ldist, 2, sum), type='l')





