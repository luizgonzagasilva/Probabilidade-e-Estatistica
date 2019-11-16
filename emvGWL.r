library(GoFKernel)
library(maxLik)  
require(VGAM) 
library(Rlab)
require(parallel)
require(doParallel)


# ----------------------   1. Base Geral    ---------------
#                                                         #
dados =  read_delim("Bases/PANCAN_clinicalMatrix", "\t", escape_double = FALSE, trim_ws = TRUE)

nrow(dados)

length(names(dados))

# Remove paper publication data
dados[c('_PANCAN_CNA_PANCAN_K8',
        '_PANCAN_Cluster_Cluster_PANCAN',
        '_PANCAN_DNAMethyl_PANCAN',
        '_PANCAN_RPPA_PANCAN_K8',
        '_PANCAN_UNC_RNAseq_PANCAN_K16',
        '_PANCAN_miRNA_PANCAN',
        '_PANCAN_mutation_PANCAN')] <- NULL

# Remove IDÃ‚Â´s relation with other datasets
dados[c('_GENOMIC_ID_TCGA_PANCAN_gistic2',
        '_GENOMIC_ID_TCGA_PANCAN_paradigm',
        '_GENOMIC_ID_TCGA_PANCAN_mutation_bcmgenelevel',
        '_GENOMIC_ID_TCGA_PANCAN_miRNA_HiSeq_gene',
        '_GENOMIC_ID_HiSeqV2_exon_PANCAN',
        '_GENOMIC_ID_HiSeqV2_PANCAN',
        '_GENOMIC_ID_TCGA_PANCAN_mutation',
        '_GENOMIC_ID_TCGA_PANCAN_HumanMethylation27',
        '_GENOMIC_ID_TCGA_PANCAN_mutation_ucsc_mafgenelevel',
        '_GENOMIC_ID_TCGA_PANCAN_mutation_broadgenelevel',
        '_GENOMIC_ID_TCGA_PANCAN_mutation_wustlgenelevel',
        '_GENOMIC_ID_TCGA_PANCAN_gistic2_threshold',
        '_GENOMIC_ID_TCGA_PANCAN_HumanMethylation450',
        '_GENOMIC_ID_TCGA_PANCAN_mutation_bcgscgenelevel',
        '_GENOMIC_ID_TCGA_PANCAN_miRNA_GA_gene')] <- NULL

# Remove Patient ID / Sample type ID / Cohort ID / SampleID / INTEGRATION
# Motivo: IDÃ‚Â´s
dados[c("_PATIENT", "_cohort", "sample_type_id", "sampleID", "_INTEGRATION")] <- NULL

# Remove  _RFS, _RFS_UNIT _RFS_IND and _TIME_TO_EVENT
# Motivo: Mesmo significado das variaveis OSÃ‚Â´s
dados[c("_RFS", "_RFS_UNIT", "_RFS_IND", "_OS_UNIT",
        "_TIME_TO_EVENT", "_TIME_TO_EVENT_UNIT")] <- NULL

# Remove OS IND
# Motivo: Duplicado do EVENT
# 12400 valores iguais
sum(dados$`_OS_IND`[!is.na(dados$`_OS_IND`)] == dados$`_EVENT`[!is.na(dados$`_EVENT`)])
# 410 NAÃ‚Â´s
table(is.na(dados$`_OS_IND`))

dados$`_OS_IND` <- NULL

# 8 VariÃƒÂ¡veis Finais
length(names(dados))


names(dados) <- c('event','os','disease','site','age_ini','gender','sample_type')



# dados do problema
dados = dados[!is.na(dados$os), ]
dados = dados[dados$os>0, ]
dados$t = dados$os/365

c = dados$t
delta = dados$event

n = length(c)
d = sum(delta)

summary(dados)

hist(c,freq=FALSE)



jumpn<-10
n<-length(c)


# dados iniciais ----------------------------------------------------------
set.seed(2019)

k<-1
nini<-50
nmax<-50
jumpn<-10
NN<-(nmax-nini)/jumpn +1

# iteracoes --------------------------------------------------------------
FF <- function(k, delta, c){
  
  
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
      if (a1[i]==0) {y[i]<-c[i]} 
    }
    
    
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
FF(k, delta, c) 

summary(estimates[[1]])
