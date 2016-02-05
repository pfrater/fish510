# Fish510.8.6.8 -   age-structured dynamic stock-production models
yrs <- 1980:1998

# annual catch data
Y <- c(2398,2520,2603,2672,2459,2385,2564, 2712,2240,1866,1692,2157,2230,2381,2238,1027,1633,1228,1411)

# annual abundance index
I <- c(45.5,51.8,51.5,47.8,45.6,56.4,61.3,52.6,39.9,36.0,40.0,42.1,51.3,51.4,38.0,27.0,35.2,31.3,38.9)

wts <- c(8,14,23,34,46,60,75,89,104,119,131,145,159,175) #wts from MRI report 2011

g <- 4
totyrs <- length(Y)

M <- rep(0.3, (g+1))
aveRecr <- 100
selpat <- c(0.1,0.333,0.667,1,1)
q <- 1
Fvec <- rep(0.5,totyrs)

Zplus <- (g:(g+9))*M[g+1]
Nplus <- aveRecr*exp(-Zplus)
wtplus <- sum(Nplus*wts[(g+1):(g+10)])/sum(Nplus)
w <- c(wts[1:g], wtplus)

# Initial stock size: assume equilibrium
Ninit <- aveRecr*exp(-cumsum(c(0,M))[1:g])
Ninit <- c(Ninit, aveRecr*exp(-g*M[1:g])/(1-exp(-M[g+1])))
Rvec <- rep(aveRecr,totyrs)
parameters <- c(q, Fvec, log(Rvec))















