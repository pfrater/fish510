# Read this in with
# source("tst.r")
# Test this with
# ssefcn(beta,yrs,Y,I)
# Run this with
#nlm(ssefcn,beta,yrs=yrs,Y=Y,I=I)


# Annual catch data
Y<-c(2398,2520,2603,2672,2459,2385,2564,
     2712,2240,1866,1692,2157,2230,2381,
     2238,1027,1633,1228,1411)
# Annual abundance index
I<-c(45.5,51.8,51.5,47.8,45.6,56.4,61.3,
     52.6,39.9,36.0,40.0,42.1,51.3,
     51.4,38.0,27.0,35.2,31.3,38.9)

q<-2
K<-700
B0<-K
r<-0.7
B<-B0

######################################
# Simple initialisation:  Assume low r and complete depletion
#
r<-0.05
B0<-sum(Y)
K<-B0
q<-mean(I)/B0            # Make sure the q is of the appropriate magnitude
#q<-mean(I)/mean(Y)
######################################

######################################
# A different initialisation:  Assume equilibrium at (3/2)MSY with
#                              B0=(3/2)MSY=Y=(3/4)K
#B0<-mean(Y) 
#K<-(4/3)*B0  # or 2*B0 if start at MSY
#r<-4         # or 2
# *** Depletion is better
#q<-mean(I)/mean(Y)
######################################


beta<-log(c(K,B0,r,q))

ssefcn<-function(beta,yrs,Y,I, plot.it=F){
    betatmp<-exp(beta)
    yrs<-1:length(Y)
    K<-betatmp[1]
    B0<-betatmp[2]
    #B0<-K
    r<-betatmp[3]
    q<-betatmp[4]
    B<-B0
    Btraj<-NULL
    Ihat<-NULL
    for(i in yrs){
        SY<-r*B*(1-B/K)
        #cat(K,B0,r,q,Y[i],SY,B,"\n")
        #if(SY<0)SY<-0
        Btraj<-c(Btraj,B)
        Ihat<-c(Ihat,q*B)
        B1<-B+SY-Y[i]
        B<-ifelse(B1>0,B1,0.001*B)
    }
    SSE<-sum((log(I)-log(Ihat))^2)
    if (plot.it==T) {
        plot(yrs,I,ylim=c(0,max(I)))
        lines(yrs,Ihat,col='blue')
    }
    #cat("SSE=",SSE,"\n")
    return(SSE)
}

result<-ssefcn(beta,yrs,Y,I)