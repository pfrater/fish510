setwd('C:/Users/pfrater/Documents/classes/fish510/rCode')

# sourcing code from previous lectures
source('fish510.6/fish510_6.r')
source('fish510.6/srfun.r')
source('fish510.6/yrfun.r')

# exercises for fish510.7 - prediction of stock and catch
w <- c(447,756,1092,1294,1448,1685,2188,2534) #weight at age of catches in 2010
ws<-c(175,442,757,1129,1304,1583,1865,2107) #weight at age in stock in 2011
p<-c(0.04,0.18,0.43,0.82,0.82,0.84,0.90,0.97) #proportion mature at age
n2010<-c(22.5,91.0,26.9,16.8,19.4,41.2,4.2,2.1) #stock numbers at age beginning in 2010

Recr <- 100 # future recruitment
c2010 <- c(0.121, 6.032, 7.061, 4.806, 6.766, 17.503, 1.874, 0.882)
ages <- 2:9
A <- length(ages) - 1 #number of true ages
Fmort <- c(0.006,0.076,0.342,0.380,0.487,0.635,0.690,0.766)
natmort <- rep(0.2, 8)


# simulating initial conditions
Fhist <- selF3
Zhist <- Fhist*sa + M
srhist <- srfun(Fhist, M, sa, wa, pa)
Shist <- K*(alpha*srhist-1)
Rhist <- alpha*Shist / (1+Shist/K)
Nhist <- Rhist*exp(cumsum(-c(0, Zhist[1:(length(Zhist)-1)])))
barplot(Nhist, names.arg=ages)


# carrying forward stock numbers
inpdat <- cbind(w, ws, p, Fmort, natmort, n2010, c2010)
ages <- 2:9
dimnames(inpdat)[[1]] <- ages

# simple forward projection
n.temp <- n2010*exp(-(Fmort + natmort))
n2011 <- c(Recr, n.temp[1:A-1], n.temp[A] + n.temp[A+1])

N0 <- Nhist
Fmort <- 0.25
Z <- Fmort*sa + M

# computing the catch
canum2011 <- (Fmort/(Fmort + natmort))*(1-exp(-(Fmort+natmort)))*n2011
Y2011<- sum(canum2011*w)

# projecting stock numbers and biomass
S <- sum(wa*pa*N0)
N1 <- N0*exp(-Z)

Recr <- (alpha*S / (1 + (S/K)))
N0 <- c(R, N1[1:(length(N1) - 1)])

n.temp<-n2010*exp(-(Fmort+natmort)) 
n2011<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2011<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2011 
Y2011<-sum(c2011*w) 
n.temp<-n2011*exp(-(Fmort+natmort)) 
n2012<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2012<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2012 
Y2012<-sum(c2012*w) 
n.temp<-n2012*exp(-(Fmort+natmort)) 
n2013<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2013<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2013 
Y2013<-sum(c2013*w) 
n.temp<-n2013*exp(-(Fmort+natmort)) 
n2014<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2014<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2014 
Y2014<-sum(c2014*w)  
n.temp<-n2014*exp(-(Fmort+natmort)) 
n2015<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2015<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2015 
Y2015<-sum(c2015*w)  
n.temp<-n2015*exp(-(Fmort+natmort)) 
n2016<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2016<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2016 
Y2016<-sum(c2016*w)  
n.temp<-n2016*exp(-(Fmort+natmort)) 
n2017<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2017<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2017 
Y2017<-sum(c2017*w)  
n.temp<-n2017*exp(-(Fmort+natmort)) 
n2018<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2018<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2018 
Y2018<-sum(c2018*w)  
n.temp<-n2018*exp(-(Fmort+natmort)) 
n2019<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2019<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2019 
Y2019<-sum(c2019*w)  
n.temp<-n2019*exp(-(Fmort+natmort)) 
n2020<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1]) 
c2020<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n2020 
Y2020<-sum(c2020*w)


c(Y2011,Y2012,Y2013,Y2014,Y2015,Y2016,Y2017,Y2018,Y2019,Y2020)
nmat<-rbind(n2010,n2011,n2012,n2013,n2014,n2015,n2016,n2017,n2018,n2019,n2020)


## the following code does the same thing as above, but with a for loop instead
w<-c(447, 756, 1092, 1294, 1448, 1685, 2188, 2534) 
ws<-c(175, 442, 757, 1129, 1304, 1583, 1865, 2107) 
p<-c(0.04, 0.18, 0.43, 0.82, 0.82, 0.84, 0.90, 0.97) 
n2010<-c(22.5, 91.0, 26.9, 16.8, 19.4, 41.2, 4.2, 2.1) 
Recr<-100 
c2010<-c(0.121, 6.032, 7.061, 4.806, 6.766, 17.503, 1.874, 0.882) 
ages<-2:9 
A<-length(ages)-1 
Fmort<-c(0.006, 0.076, 0.342, 0.380, 0.487, 0.635, 0.690, 0.766) 
natmort<-rep(0.2,8) 
n.init<-n2010 
Yvec<-NULL 
nmat<-NULL 
years<-2011:2020  
for(y in years){   
    n.temp<-n.init*exp(-(Fmort+natmort))   
    n.next<-c(Recr,n.temp[1:(A-1)],n.temp[A]+n.temp[A+1])   
    c.no<-(Fmort/(Fmort+natmort))*(1-exp(-(Fmort+natmort)))*n.next   
    Y<-sum(c.no*w)   
    n.init<-n.next   
    Yvec<-c(Yvec,Y)   
    nmat<-rbind(nmat,n.init) 
} 
names(Yvec)<-years 
dimnames(nmat)<-list(Year=years,Age=ages) 
print(Yvec) 
print(nmat)



