## fish510.2

## examples from reading

# fish510.1.5 - generating curve for development of a yearclass
# simple version
ages <- 0:10
N0 <- 1000
z <- 0.5
N <- N0*exp(-z*ages)

# complex version
N <- 1000
N0 <- N
z <- 0.5
ages <- 1:10

for (a in ages) {
    N1 <- N0*exp(-z);
    N <- c(N, N1);
    N0 <- N1
}

plot(N[1:length(ages)]~ages, type='b', main='Development of a yearclass', xlab='Age', ylab='Numbers')
    
# apparently it is simpler to use cumsum
nvec <- exp(-c(0,cumsum(z)))*N0
nvec <- nvec[1:length(z)]


# development of a yearclass biome
w <- c(1.41, 1.98, 2.72, 3.73, 4.85, 6.1, 7.28, 8.34, 9.32, 10.01, 11.57)
m <- c(0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20)
n <- 1000;
n0 <- n;
b <- NULL;
for (a in 1:length(w)) {
    n1 <- n0*exp(-m[a]);
    n <- c(n, n1);
    n0 <- n1;
    b <- c(b, n0*w[a + 1]);
}
plot(1:length(w), b, xlab='Age', ylab='g', main='Development of Saithe yearclass', type='l', ylim=c(0,2500))

# achieving same as above with cumsum()
n0 <- 1000
nvec <- exp(-c(0, cumsum(m)))*n0
nvec <- nvec[1:length(w)]


################
## fish 510.2.2
# catch curve analysis
ages<-3:14
ycl74<- c( 2.61, 16.29, 13.77, 15.12, 12.71, 7.67, 2.05, 0.51, 0.22, 0.10, 0.05, 0.03)
ycl75<-c( 6.00, 28.43, 32.50, 23.25, 14.01, 8.38, 2.68, 1.06, 0.39, 0.14, 0.11, 0.01)
ycl76<- c( 7.19, 28.53, 39.20, 28.35, 17.38, 7.34, 2.26, 0.81, 0.25, 0.16, 0.04, 0.02)
ycl77<-c(4.35, 13.30, 24.46, 18.94, 8.08, 4.20, 1.77, 0.59, 0.31, 0.09, 0.02, 0.01)
ycl78<-c(2.12, 20.81, 24.31, 15.33, 8.71, 4.44, 1.46, 0.46, 0.11, 0.05,0.01, 0.01)
mat<-cbind(ycl74,ycl75,ycl76,ycl77,ycl78)
matplot(ages,mat,type='l',lty=1:5,col=1:5,lwd=2,xlab="Age",ylab="Millions",main="Catch of cohorts, in numbers")
legend(10,20,74:78,lty=1:5,
       col=1:5,lwd=2)

# catch curve analysis - log-scale
matplot(ages,mat,type='l',lty=1:5,col=1:5,lwd=2,xlab="Age",ylab="Millions",main="Catch of cohorts, in numbers - log scale",log="y")
legend(10,20,74:78,lty=1:5,col=1:5,lwd=2)


# catch curve analysis - differences in catches on log scale by age
fage<-4:(length(ages)-1)
lage<-5:length(ages)
diff<-log(mat[fage,1:5])-log(mat[lage,1:5])
matplot(ages[fage],diff,type='l',lty=1:5,col=1:5,lwd=2,xlab="Age",ylab="Millions",main="Differenced catch of cohorts, in log-numbers")
legend(10,20,74:78,lty=1:5,col=1:5,lwd=2)

# catch curve analysis - log axis
fage<-4:(length(ages)-1)
lage<-5:length(ages)
lnc<-log(mat[fage,])
matplot(ages[fage],lnc,type='l',lty=1:5,col=1:5,lwd=2,xlab="Age",ylab="log-numbers",main="Catch of cohorts")
legend(10,20,74:78,lty=1:5,col=1:5,lwd=2)


# catch curve analysis by estimating Z
fage<-5:(length(ages)-1)
lage<-6:length(ages)
lnc<-apply(log(mat[fage,]),1,mean)
rage<-ages[fage]
plot(rage,lnc,xlab="Age",ylab="log-numbers",main="Catch of cohorts on log-scale",type='p')
abline(lm(lnc~rage))







