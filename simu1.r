#rm(list=ls(all=TRUE))
library(maxLik)  
require(VGAM) 
library(Rlab)
require(parallel)
require(doParallel)


set.seed(2019)

n <- 100
nini<-50
nmax<-400
jumpn<-10
NN<-(nmax-nini)/jumpn +1


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
      res  <- try(maxLik(loglike, start=c(pmu,pomega,ppi),iterlim = 10000))
      out<- try(res$estimate)
      vomu<-try(solve(-res$hessian)[1,1])
      voomega<-try(solve(-res$hessian)[2,2])
      vopi<-try(solve(-res$hessian)[3,3])
      #verificar se o método conseguiu encontrar o EMV, em alguns casos ele não conseguia e o programa parava.
      if(is.double(vomu) & vomu!="NaN" & voomega!="NaN")
      if(is.double(out[1]) & vomu>0 & voomega>0 &  vopi>0 & out[3]>0 & out[1]<40 & out[2]<40){
         if(out[1]>0 & out[2]>0 & out[3]<1 & out[1]!="NaN" & out[2]!="NaN" & out[3]!="NaN" ) {
            o<-o+1
            #Calcula a Probabilidade de Cobertura
            cc<-qnorm(0.975,0,1)
            iimu<-out[1]-cc*(vomu^0.5)  ; ismu<-out[1]+cc*(vomu^0.5) ;   
            iiomega<-out[2]-cc*(voomega^0.5)  ; isomega<-out[2]+cc*(voomega^0.5) ;
            iipi<-out[3]-cc*(vopi^0.5)  ; ispi<-out[3]+cc*(vopi^0.5) ;
            if(iimu<=pmu & ismu>= pmu) pc1[o]<-1
            if(iiomega<=pomega & isomega>= pomega) pc2[o]<-1 
            if(iipi<=ppi & ispi>= ppi) pc3[o]<-1   
            emv[o,] <- out
            cat(o,"    ",ite+1, "\n")
            pcens[o]<-pcena  
          }
       }
       ite<-ite+1
   }
   
   mremu<-(sum(emv[,1]/pmu)/B)
   mreomega<- (sum(emv[,2]/pomega)/B)
   mrepi<- (sum(emv[,3]/ppi)/B)
   
   msemu<- (sum((emv[,1]-pmu)^2)/B)
   mseomega<-  (sum((emv[,2]-pomega)^2)/B)
   msepi<- (sum((emv[,3]-ppi)^2)/B)
   
   cobmu<-sum(pc1)/B
   cobomega <- sum(pc2)/B
   cobpi<- sum(pc3)/B
   
   return(list(mremu=mremu ,
               msemu=msemu ,
               cobmu=cobmu ,
               mreomega=mreomega ,
               mseomega=mseomega ,
               cobomega=cobomega ,
               mrepi=mrepi ,
               msepi=msepi ,   
               cobpi=cobpi,
               res = res
            )
   ) 

}

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores=4)
estimates<-foreach(k = 1:NN,  .multicombine=TRUE,
.packages = c("maxLik","Rlab","VGAM")) %dopar% 
FF(k) 


mremu <- matrix(nrow=NN,ncol=1)
msemu <- matrix(nrow=NN,ncol=1)
cobmu <- matrix(nrow=NN,ncol=1)
mreomega <- matrix(nrow=NN,ncol=1)
mseomega <- matrix(nrow=NN,ncol=1)
cobomega <- matrix(nrow=NN,ncol=1)
mrepi <- matrix(nrow=NN,ncol=1)
msepi <- matrix(nrow=NN,ncol=1)
cobpi <- matrix(nrow=NN,ncol=1)
 
for( i in 1: NN) {
   mremu[i,]<-estimates[[i]]$mremu
   msemu[i,]<-estimates[[i]]$msemu
   cobmu[i,]<-estimates[[i]]$cobmu
   
   mreomega[i,]<-estimates[[i]]$mreomega
   mseomega[i,]<-estimates[[i]]$mseomega
   cobomega[i,]<-estimates[[i]]$cobomega
   
   mrepi[i,]<-estimates[[i]]$mrepi
   msepi[i,]<-estimates[[i]]$msepi
   cobpi[i,]<-estimates[[i]]$cobpi
}

 
save.image("simula1.RData") 


