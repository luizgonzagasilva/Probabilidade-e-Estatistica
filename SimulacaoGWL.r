library(GoFKernel)
library(maxLik)  
require(VGAM) 
library(Rlab)
require(parallel)
require(doParallel)



set.seed(2019)

k<-1
nini<-50
nmax<-50
jumpn<-10
NN<-(nmax-nini)/jumpn +1


# iteracoes --------------------------------------------------------------
FF <- function(k){

  jumpn<-10
  n<-k*jumpn+1000
  ############ Declarando as variaveis e alguns parametros ##############
  pAlpha = 1.3
  pLambda = 1.2
  pPhi = 0.4
  pPi = 0.5
  
  B <- 100       # Qtd de estimativas que serão calculados
  maux<-30
  set.seed(2000)
  o<-0           # Contador de iterações (sem NaN)
  ite<-0         # Contador de iterações (total)
  
  # funcao incompleta upper/lower gamma -----------------------------------
  ugamma<-function(a,b){
    (pgamma(a,b, lower=FALSE)*gamma(b))
  }
  
  lgamma<-function(a,b){
    (pgamma(a,b, lower=TRUE)*gamma(b))
  }
  
  
  ###################### CDF ##################
  func_F <- function(t){
    Alpha = pAlpha
    Lambda = pLambda
    Phi = pPhi
    Pi = pPi
    aux <- (1-Pi)*(lgamma((Lambda*t)^Alpha,Phi)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/((Lambda+Phi)*gamma(Phi))
    return(aux)
  }
  
  F.inv = inverse(func_F, lower = 0, upper = 30)
  
  ###################### Log da Verossimilhança ##################
  loglike<-function(theta) {
    Alpha<-theta[1]
    Lambda<-theta[2]
    Phi<-theta[3]
    Pi<-theta[4]
    aux<- d*log(1-Pi)+d*log(Alpha)+d*Alpha*Phi*log(Lambda)-n*log(Lambda+Phi)-n*log(gamma(Phi))+(Alpha*Phi-1)*sum(delta*log(t))+sum(delta*log(Lambda+(Lambda*t)^Alpha))-(Lambda^Alpha)*sum(delta*(t^Alpha)) + sum( (1-delta)*log(Pi*(Lambda+Phi)*gamma(Phi) + (1-Pi)*(ugamma(((Lambda*t)^Alpha), Phi)*(Lambda+Phi)+(Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))) )
    return(aux)
  } 
  
  ########################### Calculando ########################
  ### Comando try() faz com que mesmo que o programa encontre erro ele não pare de executar ###
  Theta <- c()
  while(o<B) {
    
    out<-NULL
    a1<-rbern(n,pPi)
    n1<-n-sum(a1)
    t<-c()
    y<-c()
    for (i in 1:n) {
      if (a1[i]==1) {y[i]<-1.000e+54}   ##Gera cm fracoa de cura
      if (a1[i]==0) {y[i]<-F.inv(runif(1, 0, 1-pPi))} 
    }
    
    delta<-rep(1,n) # Gera vetor de tamanho n com valores 1
    cax<-runif(n,0,maux)
    for (i in 1:n) {
      if (y[i]<=cax[i]) {t[i]<-y[i] ;delta[i]<-1}
      if (y[i]>cax[i]) {t[i]<-cax[i] ;delta[i]<-0}
    }
    
    #hist(t, breaks=50, prob=TRUE)
    
    out <- c()
    d<-sum(delta)
    pcena<-(1-(d/n))
    res  <- try(maxLik(loglike, start=c(pAlpha, pLambda, pPhi, pPi),iterlim = 1000))
    out<- try(res$estimate)
    
    voalpha<-try(solve(-res$hessian)[1,1])
    volambda<-try(solve(-res$hessian)[2,2])
    vophi<-try(solve(-res$hessian)[3,3])
    vopi<-try(solve(-res$hessian)[4,4])
    
    # verificar se o método conseguiu encontrar o EMV
    if(is.double(voalpha) & voalpha!="NaN" & volambda!="NaN" & vophi!="NaN" & vopi!="NaN")
      if(out[1]>0 & out[2]>0 & out[3]>0 & out[4]>0 & out[4]<1){
        o<-o+1
        Theta <- rbind(Theta, out)
     }
    
    ite<-ite+1
    
  }
  
  return(Theta)

}


number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores=4)
estimates<-foreach(k = 1:NN,  .multicombine=TRUE,
.packages = c("maxLik","Rlab","VGAM","GoFKernel")) %dopar% 
FF(k) 

summary(estimates[[1]])
