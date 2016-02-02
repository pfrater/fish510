# function to calculate spawning stock
srfun<-function(Fmult,M,sa,wa,pa){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-exp(-cumsum(Ztemp))
    S<-sum(wa*pa*cumZ)
    return(S)
}

# function to compute yield
yrfun<-function(Fmult,M,sa,wa){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    prop<-(Fmort/Z)*(1-exp(-Z))
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-cumsum(Ztemp)
    C<-prop*exp(-cumZ)
    Y<-sum(wa*C)
    return(Y)
}