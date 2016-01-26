## this code initializes the values fotr the function found in fish510.3.3.8
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.3')
lmat <- read.table('base30.dat', header=T) 
le <- lmat$le
freq <- lmat$freq
na <- 11
p0 <- rep(1/na, na-1)

# make sure the zero cells are also represented
lgrps <- 1:130
dat <- rep(0,130)
dat[le] <- freq/sum(freq) # the data vector

pi <- c(p0, 1-sum(p0))
mu <- c(12,24,35, 45, 55, 65, 75, 85, 95, 105, 115)
sigma <- c(2.5, 2.2, 3, 3, 3,4,4,4,4,4,4)







