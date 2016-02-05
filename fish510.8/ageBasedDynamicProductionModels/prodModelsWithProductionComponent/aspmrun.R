
#filename="aspmrun.r"  
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.8/ageBasedDynamicProductionModels/prodModelsWithProductionComponent')
source("aspminit.r")  
source("ssefcn.r")  
source("aspm.r")  
fm<-nlm(ssefcn,parameters,iterlim=500)   
parameters<-fm$estimate  
Fvec <-exp(parameters[1:totyrs])  
Rvec <-exp(parameters[(totyrs+1):(2*totyrs)])  
q    <-exp(parameters[2*totyrs+1])  
alpha<-exp(parameters[2*totyrs+2])  
K    <-exp(parameters[2*totyrs+3])  
ssefcn(fm$estimate,printit=T)  
fmopt<-aspm(Fvec,Rvec) 
Yhat<-fmopt$Yhat 
Bhat<-fmopt$Bhat 
Ihat<-q*Bhat  
time<-1:totyrs 
par(mfrow=c(2,3)) 
plot(time,Y) 
lines(time,Yhat) 
plot(time,I) 
lines(time,Ihat) 
barplot(Rvec,names.arg=time,xlab="Year") 
title("Annual recruitment") 
plot(time,Fvec,type='b',ylim=c(0,1)) 
plot(time,Bhat,type='b') 
Brange<-(0:160)*100 
Rrange<-alpha*Brange/(1+Brange/K) 
plot(Bhat,Rvec,ylim=c(0,max(Rvec)),xlim=c(0,max(Bhat)))   
lines(Brange,Rrange)
