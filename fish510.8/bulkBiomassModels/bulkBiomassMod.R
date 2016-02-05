# exercises for fish510.8 - statistical stock assessment methods - bulk biomass models
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.8')
library(ggplot2)
# an equilibrium model
r <- 0.25
K <- 1000
Blev <- (0:100)*10
ylev <- r*Blev*(1-Blev/K)
plot(Blev, ylev, 'l')

# similar example can be seen in source('stockprod.r')

yrs<-1980:1998 # Annual catch data 
Y<-c(2398,2520,2603,2672,2459,2385,2564,2712,2240,1866,1692,2157,2230,2381,2238,1027,1633,1228,1411) #Annual abundance index 
I<-c(45.5,51.8,51.5,47.8,45.6,56.4,61.3,52.6,39.9,36.0,40.0,42.1,51.3,51.4,38.0,27.0,35.2,31.3,38.9)
ggplot(data=data.frame(cbind(I, yrs)), aes(x=I, y=yrs)) + geom_point() + geom_smooth(fill=NA)


######################################
# Simple initialisation: Assume low r and complete depletion
r<-0.05
B0<-sum(Y)
K<-B0
q<-mean(I)/B0 # Make sure the q is of the appropriate magnitude

beta0<-log(c(K,B0,r,q))

years <- 1980:1998

# initial values, setup, and minimization for bulk biomass model
source('bulkBMinitVals.R')
result <- ssefcn(beta, yrs, Y, I)
fit <- nlm(ssefcn, beta, yrs=yrs, Y=Y, I=I)

# modifying the model
# start by making B0 to K
beta0 <- log(c(K, r, q))
result0 <- ssefcn(beta0, yrs, Y, I, plot.it=T)
fit <- nlm(ssefcn, beta0, yrs, Y=Y, I=Y)
beta <- fit$estimate
result <- ssefcn(beta, yrs, Y=Y, I=I)


# estimation in bulk production models
source("bulkEstim.r", chdir=T)
ssefcn(beta,yrs,Y,I)
nlm(ssefcn,beta,yrs=yrs,Y=Y,I=I)

######################################
# case study with nephrops
######################################
source('bulkEstim.R')
dat <- read.table('nephrops.dat')
U <- dat$UTOT
Y <- dat$YTOT
yrs <- as.numeric(dimnames(dat)[[1]])

# initialize
r <- 0.05
B0 <- sum(Y)
K <- B0
q <- mean(U)/B0

beta0 <- log(c(K,r,q))

result0 <- ssefcn(beta0, yrs, Y, U, plot.it=F)
fit <- nlm(ssefcn, beta0, yrs, Y=Y, I=U)
beta <- fit$estimate
result <- ssefcn(beta, yrs, Y, U, plot.it=F)

result0<-ssefcn(beta0,yrs,Y,U,plot.it=F)
fit<-nlm(ssefcn,beta0,yrs,Y=Y,I=U)
beta<-fit$estimate
#cat("The result is:",exp(beta),"\n")
result<-ssefcn(beta,yrs,Y,U,plot.it=F)
#cat("The minimum SSE is ",result,"\n")

K<-exp(beta[1])
r<-exp(beta[2])
q<-exp(beta[3])
Btraj<-rep(NA,length(yrs))
Btraj[1]<-K
for(i in 1:(length(yrs)-1)){
    SY<-r*Btraj[i]*(1-Btraj[i]/K)
    Btraj[i+1]<-Btraj[i]+SY-Y[i]
    Btraj[i+1]<-ifelse(Btraj[i+1]>0,Btraj[i+1],0.001*Btraj[i])
}

par(mfcol=c(2,2))
plot(yrs,U,type='l',ylim=c(0,70),ylab="Index")
plot(yrs,Y,type='l',ylim=c(0,3000),ylab="Landings")
plot(U,Y,type='l',xlim=c(0,70),ylim=c(0,3000))
plot(Btraj,Y,ylim=c(0,3000),xlim=c(0,K))
Bseq<-seq(0,round(K),1000)
lines(Bseq,r*Bseq*(1-Bseq/K))








