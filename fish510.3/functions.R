## fish 510.3.3.8 - models of length distribution

# function to estimate the sum of squared errors to estimate proportion of fish in each length category
sseprop <- function(pvec) {
    pi <- c(pvec, 1-sum(pvec));
    pi <- abs(pi);
    pi <- pi/sum(pi);
    fit <- rep(0,130);
    for (lgrp in 1:130) {
        fit[lgrp] <- sum(pi*(pnorm((lgrp+0.5-mu) / sigma) - pnorm((lgrp-0.5-mu)/sigma)))
    }
    sse <- sum((dat-fit)^2);
    return(sse)
}

# function to estimate the sum of squared errors to estimate standard deviation of length at age
ssesigma <- function(sigmavec) {
    sigma <- c(sigmavec[1], sigmavec[2], sigmavec[3], rep(sigmavec[4], length(mu)-3));
    fit <- rep(0, 130);
    for (lgrp in 1:130) {
        fit[lgrp] <- sum(pi*(pnorm((lgrp+0.5-mu)/sigma) - pnorm((lgrp-0.50-mu) / sigma)));
    }
    sse <- sum((dat-fit)^2);
    return(sse)
}

# function to estimate the sum of squared errors to estimate mean length at age
ssemu <- function(muvec) {
    fit <- rep(0, 130);
    for (lgrp in 1:130) {
        fit[lgrp] <- sum(pi*(pnorm((lgrp+0.5-muvec)/sigma) - pnorm((lgrp-0.50-muvec) / sigma)));
    }
    sse <- 1e6*sum((dat-fit)^2);
    return(sse)
}

# estimating length at age using a von Bertalanffy growth curve model
# the following estimates the sum of squared errors for vonb model
ssevonB<-function(beta) {   
    Linf<-exp(beta[1])   
    k<-beta[2]   
    muvec<-Linf*(1-exp(-k*(1:11)))   
    fit<-rep(0,130)   
    for(lgrp in 1:130){     
        fit[lgrp]<-sum(pi*(pnorm((lgrp+0.5-muvec)/sigma)-pnorm((lgrp-0.5-muvec)/sigma)))
    }   
    sse<-sum((dat-fit)^2)   
    plot(lgrps,dat,type='b',lwd=2)   
    lines(lgrps,fit,type='l',lwd=2,col="red")   
    cat("SSE=",sse,"\n")   
    return(sse)
} 





