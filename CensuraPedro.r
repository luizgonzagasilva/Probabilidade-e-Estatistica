library(maxLik)  
require(VGAM) 
library(Rlab)
require(parallel)
require(doParallel)

n <- 50
maux <- 30
pmu <- 0.5 
pomega <- 2  
ppi<-0.4 

out<-NULL
a1<-rbern(n,ppi)
n1<-n-sum(a1)
t<-c()
y<-c()
for (i in 1:n) {
  if (a1[i]==1) {y[i]<-1.000e+54}   ##Gera cm fracoa de cura
  if (a1[i]==0) {y[i]<-1/rgengamma.stacy(1, scale = (sqrt(pomega/pmu)), d = 2, k = pmu)} 
}

delta<-rep(1,n) # Gera vetor de tamanho n com valores 1
cax<-runif(n,0,maux)
for (i in 1:n) {
  if (y[i]<=cax[i]) {t[i]<-y[i] ;delta[i]<-1}
  if (y[i]>cax[i]) {t[i]<-cax[i] ;delta[i]<-0}
}

d<-sum(delta)
pcena<-(1-(d/n))

