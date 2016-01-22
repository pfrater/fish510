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


# compute/plot the mean length at age data
age.length <- matrix(table(fish$length, fish$age), ncol=length(unique(fish$age)), dimnames=list(sort(unique(fish$length)), sort(unique(fish$age))))
ld.mult <- matrix(as.numeric(rep(dimnames(age.length)[[1]], length(unique(fish$age)))), ncol=ncol(age.length))
mean.la <- apply(age.length*ld.mult,2, sum) / apply(age.length, 2, sum)
age <- sort(unique(fish$age))
plot(mean.la~age)
    
# fit model to mean length at age and to raw data
mn.mod <- lm(mean.la~age)
coef(mn.mod)

raw.mod <- lm(length~age, data=fish)
coef(raw.mod)

# plotting weight against length
plot(live.wt~length, data=fish)
plot(log(live.wt)~log(length), data=fish)

w <- function(alpha, l, beta) alpha*(l^beta)
plot(live.wt~length, data=fish); curve(w(0.004317883, x, 3.1834), add=T)


# compute the age-length key (alk)
plot(fish$length~fish$age)
age <- fish
age$le5 <- floor(age$length/5)*5+2.5
age.vec <- min(age$age):max(age$age)
le.vec <- seq(min(age$le5), max(age$le5), 5)
alt <- matrix(rep(0,length(age.vec)*length(le.vec)), ncol=length(age.vec))
dimnames(alt) <- list(le.vec, age.vec)
alt.tmp <- table(age$le5, age$age)
alt[dimnames(alt.tmp)[[1]], dimnames(alt.tmp)[[2]]] <- alt.tmp

# calculate the length and age distributions from simple operations of age-length matrix
ld.tmp <- apply(alt, 1, sum)
ld.mat <- matrix(rep(ld.tmp, ncol(alt)), byrow=F, ncol=ncol(alt))
alk <- round(alt/ld.tmp, 2) # alk stands for age-length key



################################################
# practicals

# create a length-age distribution for certain categories
le <- fish$length
hist(le)
table(le)
le5 <- floor(le/5)*5+2.5
length.dist <- as.vector(table(le5))

# fill in the gaps
rnames <- seq(min(le5), max(le5), 5)
ldist <- rep(0, length(rnames))
names(ldist) <- paste(rnames)
tmp <- table(le5)
ldist[names(tmp)] <- tmp

plot(as.numeric(names(ldist)), ldist, type='l')
barplot(ldist)

# length distributions: matrix of length and count data
le <- c(seq(20,40,5), 25, 35)
num <- c(4,7,9,6,2,5,3)
temp <- data.frame(le=le, num=num)
temp.tab <- tapply(temp$num, temp$le, sum)

# setting up an age-length key from the fish data
# first we need to determine the age classes
plot(fish$length~fish$age)
age <- fish
age$le5 <- floor(age$length/5)*5+2.5
age.vec <- min(age$age):max(age$age)
le.vec <- seq(min(age$le5), max(age$le5), 5)
alt <- matrix(rep(0,length(age.vec)*length(le.vec)), ncol=length(age.vec))
dimnames(alt) <- list(le.vec, age.vec)
alt.tmp <- table(age$le5, age$age)
alt[dimnames(alt.tmp)[[1]], dimnames(alt.tmp)[[2]]] <- alt.tmp

# calculate the length and age distributions from simple operations of age-length matrix
ld.tmp <- apply(alt, 1, sum)
ld.mat <- matrix(rep(ld.tmp, ncol(alt)), byrow=F, ncol=ncol(alt))
alk <- round(alt/ld.tmp, 2) # alk stands for age-length key


# create length distribution
ld.5 <- table(age$le5)
ld <- as.vector(ld.5)

# create the age-length distribution by multiplying the alk by the length distribution
ald <- alk*ld
# ald.test <- alk*ld.mat
# all(ald==ald.test)

# calculate mean length in each age/length cell
lengths <- matrix(rep(as.numeric(dimnames(ald)[[1]]), ncol(ald)), ncol=ncol(ald))
mean.la <- apply(ald*lengths, 2, sum) / apply(ald,2,sum)

plot(mean.la~c(2:9), cex=1.5)
points(fish$length~fish$age, pch=3, col='blue', cex=0.8)









