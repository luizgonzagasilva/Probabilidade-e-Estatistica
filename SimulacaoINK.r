library(maxLik)  
require(VGAM) 
library(Rlab)
require(parallel)
require(doParallel)


set.seed(2019)

n <- 100
nini<-50
nmax<-100
jumpn<-10
NN<-(nmax-nini)/jumpn +1

###################### DF ##################
func_f <- function(t){
  mu = pmu
  omega =pomega
  pi = ppi
  aux <- 2*(1-pi)/gamma(mu)*(mu/omega)^mu*t^(-2*mu-1)*exp(-mu/(omega*t^2))
}

plot(t, func_f(t), type="l", col="red", lwd=2, xlab="time (t)", ylab="f(t)", main="DF")


FF <- function(k){
  
  jumpn<-10
  n<-k*jumpn+40
  ############ Declarando as variaveis e alguns parametros ##############
  pmu <- 0.5       # Parametro c para os valores com fracao de cura
  pomega <- 2     # Parametro omega para gerar valores na WL
  ppi<-0.4        # Parametro theta para gerar valores na WL
  B <- 1000        # Qtd de estimativas que serão calculados
  maux<-30
  set.seed(2019)
  o<-0           # Contador de iterações
  ite<-0         # Contador de iterações
  
  ######### tamanho das matrizes que armazenarao estimativas######
  
  emv <- matrix(nrow=B,ncol=3)
  pc1<-rep(0,times=B)
  pc2<-rep(0,times=B)
  pc3<-rep(0,times=B)
  pcens<-c()
  
  igamma<-function(a,b){
    (pgamma(a,b, lower=TRUE)*gamma(b))
  }
  

  ###################### Log da Verossimilhança##################
  loglike<-function(theta) {
    mu<-theta[1]
    omega<-theta[2]
    p<-theta[3]
    aux<- (d*log(1-p))-n*lgamma(mu)+d*mu*log(mu)-d*mu*log(omega)-((2*mu+1)*sum(delta*log(t)))+d*log(2)-(sum(mu*delta/(omega*(t^2))))+sum((1-delta)*log(p*gamma(mu)+(1-p)*igamma((mu/(omega*(t^2))),mu)))
    return(aux)
  } 
  
  ################## Calculando #######################
  ### Comando try() faz com que mesmo que o programa encontre erro ele não pare de executar ###
  Theta <- c()
  while(o<B) {
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
    res<- try(maxLik(loglike, start=c(0.1,0.1,0.1),iterlim = 10000))
    out<- try(res$estimate)
    vomu<-try(solve(-res$hessian)[1,1])
    voomega<-try(solve(-res$hessian)[2,2])
    vopi<-try(solve(-res$hessian)[3,3])
    
    #verificar se o método conseguiu encontrar o EMV, em alguns casos ele não conseguia e o programa parava.
    if(is.double(vomu) & vomu!="NaN" & voomega!="NaN")
      if(is.double(out[1]) & vomu>0 & voomega>0 &  vopi>0 & out[3]>0 & out[1]<40 & out[2]<40){
        if(out[1]>0 & out[2]>0 & out[3]<1 & out[1]!="NaN" & out[2]!="NaN" & out[3]!="NaN" ) {
          o<-o+1
          Theta <- rbind(Theta, out)
        }
      }
    
    ite<-ite+1
  }
  

  return(Theta)
  
}

#FF(3)

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores=4)
estimates<-foreach(k = 1:NN,  .multicombine=TRUE,
.packages = c("maxLik","Rlab","VGAM")) %dopar% 
FF(k) 



