Linf<-160
k<-0.1
beta<-3
cond<-0.02
A<-14
ages<-1:A
t0<-0
la<-Linf*(1-exp(-k*(ages-t0)))            # Mean length at age
la<-round(la,2)
wa<-cond*la**beta/1000                    # Mean weight at age in kg
wa<-round(wa,2)
s50<-5                                    # Age at 50% selection
sa<-round(1/(1+exp(-1.1*(ages-s50))),2)   # Selection at age
p50<-5.5
pa<-round(1/(1+exp(-2*(ages-p50))),2)   # Proportion mature at age
M<-rep(0.2,A)