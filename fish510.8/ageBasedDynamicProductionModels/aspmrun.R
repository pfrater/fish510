# running the age structured dynamic stock model
# source('aspmrun.r')
source('aspminit.r')
source('aspm.r')
source('ssefcn.r')

# get a better initial value 
fit0 <- aspm(Fvec, Rvec, totyrs, M, w)
q <- mean(I) / mean(fit0$Bhat)
parameters.0 <- c(log(Fvec), log(Rvec), log(q)) # initial values
fm <- nlm(ssefcn, parameters.0, Y=Y, I=I, M=M, w=w, iterlim=500)
parameters <- fm$estimate
Fvec <- exp(parameters[1:totyrs])
Rvec <- exp(parameters[(totyrs+1):(2*totyrs)])
q <- exp(parameters[2*totyrs+1])
fmopt <- aspm(Fvec, Rvec, totyrs, M, w)
Yhat <- fmopt$Yhat
Bhat <- fmopt$Bhat
Ihat <- q*Bhat

time <- 1:totyrs
par(mfrow=c(2,3))
plot(yrs, Y, xlab='Year', ylim=c(0,max(Y)))
lines(yrs, Yhat)
plot(yrs, I, xlab='Year', ylim=c(0,max(I)))
lines(yrs, Ihat)
plot(yrs, Rvec, xlab='Year', ylab='Recruitment', ylim=c(0, max(Rvec)))
plot(yrs, Fvec, xlab='Year', ylab='F', ylim=c(0,max(Fvec)))
plot(yrs, Bhat, xlab='Year', ylab='Biomass', ylim=c(0,max(Bhat)))












