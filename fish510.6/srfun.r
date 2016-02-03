# function to compute spawning biomass per recruit

srfun<-function(Fmult,M,sa,wa,pa){
    Fmort<-Fmult*sa
    Z<-Fmort+M
    Ztemp<-c(0,Z[1:(length(Z)-1)])
    cumZ<-exp(-cumsum(Ztemp))
    S<-sum(wa*pa*cumZ)
    return(S)
}