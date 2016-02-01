# fish510.4 - performing cohort analysis on example data

setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.4')
cnum <- read.table('nsplaice.canum', header=T, as.is=T, check.names=F)
cnum <- as.matrix(cnum)

# example 1
# suppose we assume there is a single 11 year old fish at the end of 1995, so a 12 year old in 1996
m <- 0.15
n12 <- 1
c11 <- cnum['1995', '11']
# we can backcalculate the numbers to obtain the previous year's population
n11 <- (n12*exp(m/2)+c11)*exp(m/2)


# example 2
# doing the same but for multiple age classes
c95 <- cnum['1995', ]
n96<- c(0,60000,60000,60000,40000,40000,40000,10000,10000,10000,1)
names(n96) <- as.character(3:12)
n95 <- (n96*exp(m/2)+c95)*exp(m/2)
names(n95) <- as.character(2:11)


# back to the plaice example
m <- 0.15
c95 <- cnum['1995', ]
n96 <- c(0,60000,60000,60000,40000,40000,40000,10000,10000,10000,1)
names(n96) <- as.character(2:12)
n1 <- n96[as.character(3:12)]
n0 <- as.vector(n1*exp(m/2)+c95)*exp(m/2)
names(n0) <- (n1*exp(m/2)+c95)*exp(m/2)
names(n0) <- as.character(2:11)
n95 <- unlist(c(n0,1))

c94 <- cnum['1994', ]
n1 <- n95[as.character(3:12)]
n0 <- (n1*exp(m/2)+c94)*exp(m/2)
names(n0) <- as.character(2:11)
n94 <- unlist(c(n0,1))

nmat <- rbind(n94, n95, n96)

ages <- 2:12
plot(n94~ages, col='black', type='b')
points(n95~ages, col='blue', type='b')
points(n96~ages, col='red', type='b')

## of course, doing this for every year is too much work
# functions.r contains an algorithm that does it for us
source('cohortN.r')
cnum <- read.table('nsplaice.canum', header=T, as.is=T, check.names=F)
cnum <- as.matrix(cnum)
nmat <- cohortN(cnum, m=0.15, nlast=1:ncol(cnum), noldest=1:(nrow(cnum)+1))
years <- as.integer(rownames(cnum))
m <- 0.15

# based on the above backcalculations we can now calculate fishing mortality
# simple calculation of one age class in single year
log(nmat[2,3]/nmat[3,4]) - m

# now doing the two age classes across all years in matrix
a <- ncol(nmat); y <- nrow(nmat)
f.mat <- log(nmat[1:(y-1),1:(a-1)]/nmat[2:y,2:a])-m
fbar.y <- apply(f.mat[1:(y-1),], 1, mean)
plot(fbar.y~years[1:(y-2)], type='l')

# we can get at selection pattern for fish of ages and years by dividing F.ay by fbar.y
selpat <- f.mat/fbar.y

### repeating the procedure with the assumption that we leave half the fish
source('cohortN.r')
cnum <- read.table('base.dat', header=T, as.is=T, check.names=F)
cnum <- as.matrix(cnum)
m <- 0.15
a <- ncol(cnum)
y <- nrow(cnum)
noldest <- mean(cnum[,a])
noldest <- rep(noldest, y+1)
nlast <- apply(cnum, 2, mean)
nmat <- cohortN(cnum, m, nlast, noldest)
fmat <- log(nmat[1:y,1:a]/nmat[2:(y+1), 2:(a+1)])-m
selages <- as.character(3:5)
fbar.y <- apply(fmat[,selages], 1, mean)
selpat <- fmat/fbar.y
sa <- apply(selpat[as.character(1988:1993),], 2, mean)
ages <- as.numeric(names(sa))
plot(sa~ages, xlab='Age', ylab='Selection at Age', type='l', ylim=c(0,1.5))
















