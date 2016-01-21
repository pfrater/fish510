# read in data
dat<-read.table("http://www.hi.is/~gunnar/kennsla/alsm/data/set103.dat",header=T)


# set up table that includes zero-frequency lengths
ldst <- table(dat$le)
full <- rep(0,120)
names(full) <- 1:120
lens <- names(ldst)
full[lens] <- ldst

# simulation of the effect of different sampling schemes
n <- 25
numred <- 10
numgreen <- 30
green <- rep('G', numgreen)
red <- rep('R', numred)
bowl <- sample(c(green, red))
outcome <- sample(bowl, n, replace=T)

# recoding r and g to 1 and 0
x <- sum(ifelse(outcome=='R', 1, 0))
phat <- x/n

# simulating B number of times
B <- 1000
phat <- rep(NA, B)
for (b in 1:B) {
  outcome <- sample(bowl, n, replace=T);
  x <- sum(ifelse(outcome=='R', 1,0));
  phat[b] <- x/n
}
hist(phat)

# comparing simulated results to actual theory
truep <- numred / (numred + numgreen)
truevar <- truep*(1-truep)/n
var(phat)



# example: consider an evaluation of whether the confidence interval for a proportion has the correct coverage probability
n <- 1200
x <- 350
phat <- x/n

p <- 0.4
alpha <- 0.5
zstar <- qnorm(1-alpha/2)
B <- 10000
success <- rep(NA, B)

for (i in 1:B) {
    smpl <- ifelse(runif(n)<p, 1,0);
    x <- sum(smpl);
    phat <- x/n;
    se <- sqrt(phat*(1-phat)/n);
    lo <- phat-zstar*se;
    up <- phat+zstar*se;
    success[b] <- ifelse(lo<p & p < up, 1,0)
}
    
cat('Coverage probability of the confidence interval \n')
print(table(success)/B)



################################################
# end of lesson exercises
################################################
fish <- read.table('http://www.hi.is/~gunnar/kennsla/alsm/data/set111.dat',header=T)

names(fish) <- c('number', 'spp', 'year', 'month', 'stat.rectangle', 'date', 'depth','gear', 'length', 'sex', 'maturation', 'age', 'live.wt','gutted.wt', 'liver.wt')


# length distribution
length.groups <- as.vector(names(table(fish$length)))
n.in.length.groups <- as.vector(table(fish$length))
plot(n.in.length.groups~length.groups, type='l')
length.pct <- n.in.length.groups / length(fish$length)
plot(length.pct ~ length.groups, type='l')




################################################
# practicals
le <- fish$length
hist(le)
table(le)
le5 <- floor(le/5)*5+2.5
length.dist <- as.vector(table(le5))














