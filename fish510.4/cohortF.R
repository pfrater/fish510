## function cohortF to accompany fish510_4.R

# cohortF: cohort analysis based on fishing mortalities
#   cnum:       Y*A matrix of catch-in-numbers
#   nlast:      length A vector of numbers in the last year
#   noldest:    length y+1 vector of numbers age A+1

cohortN <- function(cnum, fterm, selpat, foldest) {
    a <- ncol(cnum);
    Y <- nrow(cnum);
    years <- rownames(cnum);
    flast <- fterm*selpat
    ages <- as.numeric(dimnames(cnum)[[2]]);
    years <- as.numeric(dimnames(cnum)[[1]]);
    
    cline <- cnum[Y,]
    n1 <- cline / ((flast/flast + m)*(1-exp(-(flast+m))))
    nmat <- n1
    for (y in (Y-1):1) {
        cline <- cnum[y,];
        psi <- ((foldest[y]/foldest[y]+m)*(1-exp(-(foldest[y]+m))));
        ntopage <- cnum[y,a]/psi
        n0 <- (n1[2:a]*exp(m/2)+cline[1:(a-1)])*exp(m/2);
        n0 <- c(n0, ntopage)
        names(n0) <- ages
        nmat <- rbind(n0, nmat)
        n1 <- n0;
    }
    dimnames(nmat) <- list(years=years, ages=ages)
    return(nmat)
}