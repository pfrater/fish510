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