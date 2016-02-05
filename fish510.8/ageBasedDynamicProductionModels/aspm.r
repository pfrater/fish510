# aspm.r # aspm: Performs forward simulation of populations, given values of parameters 
aspm<-function(Fvec,Rvec,totyrs,M,w){   
    Yhat<-NULL   
    Bhat<-NULL   
    Rtemp<-Rvec[1]   
    #N0<-Ninit          # First start-of-year stock size - fixed   
    #N0<-Rtemp*exp(-cumsum(c(0,M[1:(length(M)-1)])))  # initial start-of-year stock size - equil.   
    N0<-aveRecr*exp(-cumsum(c(0,M))[1:g]) # better: first only true ages   
    N0<-c(N0,aveRecr*exp(-g*M[g+1])/(1-exp(-M[g+1])))# then append the plus group   
    for(year in 1:totyrs){     
        baseF<-Fvec[year]              # Base fishing mortality     
        F0<-baseF*selpat               # Fishing mortality during the year     
        C<-(F0/(F0+M))*(1-exp(-(F0+M)))*N0     
        Ytemp<-sum(C*w)                    # Total landings     
        Ntemp<-N0*exp(-F0-M)           # Forw proj - gives ages 2:(g+1)    
        Noldest<-Ntemp[g]+Ntemp[g+1]   # Allow for plus-group     
        #print(Ytemp)     
        Yhat<-c(Yhat,Ytemp)     
        Bhat<-c(Bhat,sum(N0*w))     
        Rtemp<-Rvec[year]     
        N1<-c(Rtemp,Ntemp[1:(g-1)],Noldest)# Set up stock at end of year     
        N0<-N1                         # Prepare for next round   
    }   
    return(list(Yhat=Yhat,Bhat=Bhat)) 
}
