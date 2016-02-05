#ssefcn.r  
ssefcn<-function(parameters,printit=F){    
    Fvec<-exp(parameters[1:totyrs])    
    Rvec<-exp(parameters[(totyrs+1):(2*totyrs)])    
    q    <-exp(parameters[2*totyrs+1])    
    alpha<-exp(parameters[2*totyrs+2])    
    K    <-exp(parameters[2*totyrs+3])    
    totyrs<-length(Rvec)    
    proj<-aspm(Fvec,Rvec)    
    Yhat<-proj$Yhat    
    Bhat<-proj$Bhat   
    Ihat<-q*Bhat    
    Rhat<-alpha*Bhat/(1+Bhat/K)     
    CVR  <-0.8    
    CVF  <-1000    
    CVY  <-0.1    
    CVI  <-0.5     
    SSEY<-sum((log(Y)-log(Yhat))^2)    
    SSEI<-sum((log(I)-log(Ihat))^2)    
    SSEF<-sum((log(Fvec[2:totyrs])-log(Fvec[1:(totyrs-1)]))^2)    
    SSER<-sum((log(Rvec)-log(Rhat))^2)     
    SSE<- (1/CVI^2)*SSEI +(1/CVY^2)*SSEY +(1/CVF^2)*SSEF +(1/CVR^2)*SSER    
    SSEvec<-c(SSEI,SSEY,SSEF,SSER)    
    names(SSEvec)<-c("SSEI","SSEY","SSEF","SSER")    
    if(printit){      
        print(round(SSEvec,3))    
    }    
    return(SSE)  
}
