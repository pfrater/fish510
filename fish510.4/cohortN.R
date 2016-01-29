## functions to accompany fish510_4.R

# cohortN: cohort analysis based on assumed oldest and last numbers
#   cnum:       Y*A matrix of catch-in-numbers
#   nlast:      length A vector of numbers in the last year
#   noldest:    length y+1 vector of numbers age A+1

cohortN <- function(cnum, m, nlast, noldest) {
    a <- ncol(cnum);
    Y <- nrow(cnum);
    nmat <- c(nlast, noldest[Y+1]);
    n1 <- nlast;
    for (y in Y:1) {
        cline <- cnum[y,];
        ntmp <- c(n1[2:a], noldest[y+1]);
        n0 <- (ntmp*exp(m/2)+cline)*exp(m/2);
        n0 <- c(n0, noldest[y]);
        nmat <- rbind(n0,nmat);
        n1 <- n0
    }
    return(nmat)
}