## R exercises for fish510.6
setwd('C:/Users/pfrater/Documents/classes/fish510/rCode/fish510.6')
# using data on cod to calculate a stock recruitment curve
sr <- read.table('sr.dat', header=T, as.is=T, check.names=F)
r <- sr$R
s <- sr$S


plot(R~S, data=sr, xlim=c(0,max(sr$S)))
bev.holt <- function(s, alpha, k) {
    r <- (alpha*s) / (1 + (s/k))
}
sse.bev.holt <- function(vals) {
    alpha <- vals[1]
    k <- vals[2]
    r.hat <- bev.holt(s, alpha, k);
    sse <- sum((data$R - r.hat)^2)
    return(sse)
}
nl.mod <- nls(R ~ bev.holt(S, alpha, k), data=sr, start=list(alpha=0.001, k=10))
curve(bev.holt(x, 10.28, 18.36), add=T, col='red')

r.hats <- bev.holt(sr$S, alpha=10.28, k=18.36)
plot(sr$R ~ r.hats)

r.mins <- nlm(sse.bev.holt, c(alpha=10.28, k=18.36))

summary(lm())














