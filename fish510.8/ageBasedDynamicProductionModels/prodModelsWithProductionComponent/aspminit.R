# aspminit.r   
## Set up initial values for age-structured dynamic stock-production models  
## Set up data  
# Annual catch data  
Y<-c(2398,2520,2603,2672,2459,2385,2564,2712,2240,1866,1692,2157,2230,2381,2238,1027,1633,1228,1411)  
# Annual abundance index  
I<-c(45.5,51.8,51.5,47.8,45.6,56.4,61.3,52.6,39.9,36.0,40.0,42.1,51.3,51.4,38.0,27.0,35.2,31.3,38.9)   
# Number of ages and years  
g<-4                          
# Number of true age groups  
totyrs<-19 # Number of years   

# Initialize values of all parameters  
# Some will later be estimated - others simply fixed at assumed values  
Ninit<-c(100,50,25,10,5)       # Initial values for initial stock size  
Rvec<-rep(100,totyrs)          # Initial values of annual recruitment  
selpat<-c(0.1,0.333,0.667,1,1) # Selection pattern-Might estimate  
w<-c(100,250,325,375,400)      # Weight at age  M<-rep(0.3,(g+1))              
# Natural mortality  
q<-1                           # Initial value of catchability  
Fvec<-rep(0.5,totyrs)          # Annual fishing mortality  

# Beverton-Holt parameters: 
alpha<-0.006 
K<-5000  

# Put the initial values of parameters to be estimated into a vector  
parameters<-log(c(Fvec,Rvec,q,alpha,K))     # Initial values of all parameters
