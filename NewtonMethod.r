# !diagnostics off
library(dplyr)
library(tidyverse)
library(LindleyR)
library(GoFKernel)

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

t = dados$t
delta = dados$event

n = length(t)
d = sum(delta)

summary(dados)



# Q() regularized gamma function and G() --------------------------------------------
Q <- function(a,b){
 pgamma(b, a, lower=FALSE)*gamma(a)/gamma(a)
}

G <- function(a,b,c){
  b^(a*c)*exp(-b^c)/pgamma(b^c, a, lower=FALSE)
}



# calculo numerico derivada PSI() -------------------------------------------

PSI1 <- function(a,b,c, h){
  (log(pgamma(b^c, a, lower=FALSE))*gamma(a) - log(pgamma(b^c, a+h, lower=FALSE))*gamma(a+h))/h
}

PSI2 <- function(a,b,c, h){
  (log(pgamma(b^c, a, lower=FALSE))*gamma(a) - log(pgamma((b+h)^c, a, lower=FALSE))*gamma(a))/h
}

PSI3 <- function(a,b,c, h){
  (log(pgamma(b^c, a, lower=FALSE))*gamma(a) - log(pgamma(b^(c+h), a, lower=FALSE))*gamma(a))/h
}




# Newton Method --------------------

NewtonMethod <- function(x) {
  
  Alpha = x[1]
  Lambda = x[2]
  Phi = x[3]
  Pi = x[4]

  # funcoes auxiliar --------------------------------------------------------
  
  Q_ = Q(Phi,(Lambda*t)^Alpha)
  G_ = G(Phi,Lambda*t,Alpha)
  PSI1_ = PSI1(Phi,Lambda*t,Alpha, 0.1)
  PSI2_ = PSI2(Phi,Lambda*t,Alpha, 0.1)
  PSI3_ = PSI3(Phi,Lambda*t,Alpha, 0.1)
  
  # derivada parcial em relação a phi ---------------------------------------
  
  termo1_s = expression((1-delta)*(Pi/Q_*(1+(Lambda+Phi)*digamma(Phi)) + (1-Pi)*(1+(Lambda+Phi)*PSI1_ + Alpha*log(Lambda*t)*G_))/( Pi*(Lambda+Phi)/Q_ + (1-Pi)*(Lambda+Phi+G_) ))
  termo2_s = expression(Alpha*delta*log(t))
  termo3 = expression(d*Alpha*log(Lambda))
  termo4 = expression(n*digamma(Phi))
  termo5 = expression(n/(Lambda+Phi))
  
  f1 = sum(eval(termo1_s), na.rm=TRUE) + sum(eval(termo2_s), na.rm=TRUE) + eval(termo3) - eval(termo4) - eval(termo5)
  
  # calculo derivada dos termos
  dtermo1_s <- D(termo1_s,"Alpha")
  dtermo2_s <- D(termo2_s,"Alpha")
  dtermo3 <- D(termo3,"Alpha")
  dtermo4 <- D(termo4,"Alpha")
  dtermo5 <- D(termo5,"Alpha")
  
  j1_Alpha = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - eval(dtermo4) - eval(dtermo5)
  
  dtermo1_s <- D(termo1_s,"Lambda")
  dtermo2_s <- D(termo2_s,"Lambda")
  dtermo3 <- D(termo3,"Lambda")
  dtermo4 <- D(termo4,"Lambda")
  dtermo5 <- D(termo5,"Lambda")
  
  j1_Lambda = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - eval(dtermo4) - eval(dtermo5)
  
  
  dtermo1_s <- D(termo1_s,"Phi")
  dtermo2_s <- D(termo2_s,"Phi")
  dtermo3 <- D(termo3,"Phi")
  dtermo4 <- D(termo4,"Phi")
  dtermo5 <- D(termo5,"Phi")
  
  j1_Phi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - eval(dtermo4) - eval(dtermo5)
  
  
  dtermo1_s <- D(termo1_s,"Pi")
  dtermo2_s <- D(termo2_s,"Pi")
  dtermo3 <- D(termo3,"Pi")
  dtermo4 <- D(termo4,"Pi")
  dtermo5 <- D(termo5,"Pi")
  
  j1_Pi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - eval(dtermo4) - eval(dtermo5)
  
  
  # derivada parcial em relação a Lambda ---------------------------------------
  
  termo1_s = expression((1-delta)*( (Pi*Phi)/Q_ + (1-Pi)*(1+(Lambda+Phi)*PSI2_ + (Alpha/Lambda)*(Phi-(Lambda*t)^(Alpha*Phi))*G_))/( Pi*(Lambda+Phi)/Q_ + (1-Pi)*(Lambda+Phi+G_)))
  termo2_s = expression( delta*(1+Alpha*t^Alpha*Lambda^(Alpha-1))/(Lambda+(Lambda*t)^Alpha) )
  termo3 = expression( d*Alpha*Phi/Lambda )
  termo4_s = expression( Alpha*Lambda^(Alpha-1)*delta*t^Alpha )
  termo5 = expression(n/(Lambda+Phi))
  
  f2 = sum(eval(termo1_s), na.rm=TRUE) + sum(eval(termo2_s), na.rm=TRUE) + eval(termo3) - sum(eval(termo4_s), na.rm=TRUE) - eval(termo5)
  
  
  # Jacobian Matrix
  dtermo1_s = D(termo1_s,"Alpha")
  dtermo2_s = D(termo2_s,"Alpha")
  dtermo3 = D(termo3,"Alpha")
  dtermo4_s = D(termo4_s,"Alpha")
  dtermo5 = D(termo5,"Alpha")
  
  j2_Alpha = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - sum(eval(dtermo4_s), na.rm=TRUE) - eval(dtermo5)
  
  dtermo1_s <- D(termo1_s,"Lambda")
  dtermo2_s <- D(termo2_s,"Lambda")
  dtermo3 <- D(termo3,"Lambda")
  dtermo4_s <- D(termo4_s,"Lambda")
  dtermo5 <- D(termo5,"Lambda")
  
  j2_Lambda = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - sum(eval(dtermo4_s), na.rm=TRUE) - eval(dtermo5)
  
  
  dtermo1_s <- D(termo1_s,"Phi")
  dtermo2_s <- D(termo2_s,"Phi")
  dtermo3 <- D(termo3,"Phi")
  dtermo4_s <- D(termo4_s,"Phi")
  dtermo5 <- D(termo5,"Phi")
  
  j2_Phi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - sum(eval(dtermo4_s), na.rm=TRUE) - eval(dtermo5)
  
  
  dtermo1_s <- D(termo1_s,"Pi")
  dtermo2_s <- D(termo2_s,"Pi")
  dtermo3 <- D(termo3,"Pi")
  dtermo4_s <- D(termo4_s,"Pi")
  dtermo5 <- D(termo5,"Pi")
  
  j2_Pi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + eval(dtermo3) - sum(eval(dtermo4_s), na.rm=TRUE) - eval(dtermo5)
  
  
  # derivada parcial em relação a Alpha ---------------------------------------
  
  termo1_s = expression( (1-delta)*(1-Pi)*((Lambda+Phi)*PSI3_ + log(Lambda*t)*(Phi-(Lambda*t)^Alpha)*G_)/( Pi*(Lambda+Phi)/Q_ + (1-Pi)*(Lambda+Phi+G_)) )
  termo2_s = expression( Phi*delta*log(t) )
  termo3_s = expression( delta*(Lambda*t)^Alpha*log(Lambda*t)/(Lambda+(Lambda*t)^Alpha) )
  termo4 = expression( d*Phi*log(Lambda) )
  termo5 = expression( d/Alpha )
  termo6_s = expression( delta*(Lambda*t)^Alpha*log(Lambda*t) )
  
  f3 = sum(eval(termo1_s), na.rm=TRUE) + sum(eval(termo2_s), na.rm=TRUE) + sum(eval(termo3_s), na.rm=TRUE) + eval(termo4) + eval(termo5) - sum(eval(termo6_s), na.rm=TRUE)
  
  # Jacobian Matrix
  dtermo1_s = D(termo1_s,"Alpha")
  dtermo2_s = D(termo2_s,"Alpha")
  dtermo3_s = D(termo3_s,"Alpha")
  dtermo4 = D(termo4,"Alpha")
  dtermo5 = D(termo5,"Alpha")
  dtermo6_s = D(termo6_s,"Alpha")
  
  j3_Alpha = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + sum(eval(dtermo3_s), na.rm=TRUE) + eval(dtermo4) + eval(dtermo5) - sum(eval(dtermo6_s), na.rm=TRUE)
  
  dtermo1_s = D(termo1_s,"Lambda")
  dtermo2_s = D(termo2_s,"Lambda")
  dtermo3_s = D(termo3_s,"Lambda")
  dtermo4 = D(termo4,"Lambda")
  dtermo5 = D(termo5,"Lambda")
  dtermo6_s = D(termo6_s,"Lambda")
  
  j3_Lambda = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + sum(eval(dtermo3_s), na.rm=TRUE) + eval(dtermo4) + eval(dtermo5) - sum(eval(dtermo6_s), na.rm=TRUE)
  
  
  dtermo1_s = D(termo1_s,"Phi")
  dtermo2_s = D(termo2_s,"Phi")
  dtermo3_s = D(termo3_s,"Phi")
  dtermo4 = D(termo4,"Phi")
  dtermo5 = D(termo5,"Phi")
  dtermo6_s = D(termo6_s,"Phi")
  
  j3_Phi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + sum(eval(dtermo3_s), na.rm=TRUE) + eval(dtermo4) + eval(dtermo5) - sum(eval(dtermo6_s), na.rm=TRUE)
  
  
  dtermo1_s = D(termo1_s,"Pi")
  dtermo2_s = D(termo2_s,"Pi")
  dtermo3_s = D(termo3_s,"Pi")
  dtermo4 = D(termo4,"Pi")
  dtermo5 = D(termo5,"Pi")
  dtermo6_s = D(termo6_s,"Pi")
  
  j3_Pi = sum(eval(dtermo1_s), na.rm=TRUE) + sum(eval(dtermo2_s), na.rm=TRUE) + sum(eval(dtermo3_s), na.rm=TRUE) + eval(dtermo4) + eval(dtermo5) - sum(eval(dtermo6_s), na.rm=TRUE)
  
  
  
  # derivada parcial em relação a Pi ---------------------------------------
  
  termo1_s = expression( (1-delta)*( (Lambda+Phi)/Q_ - (Lambda+Phi) - G_ )/( Pi*(Lambda+Phi)/Q_ + (1-Pi)*(Lambda+Phi+G_)) )
  termo2 = expression( d/(1-Pi) )
  
  f4 = sum(eval(termo1_s), na.rm=TRUE) - eval(termo2) 
  
  # Jacobian Matrix
  dtermo1_s = D(termo1_s,"Alpha")
  dtermo2 = D(termo2,"Alpha")
  
  j4_Alpha = sum(eval(dtermo1_s), na.rm=TRUE) - eval(dtermo2) 
  
  
  dtermo1_s = D(termo1_s,"Lambda")
  dtermo2 = D(termo2,"Lambda")
  
  j4_Lambda = sum(eval(dtermo1_s), na.rm=TRUE) - eval(dtermo2) 
  
  dtermo1_s = D(termo1_s,"Phi")
  dtermo2 = D(termo2,"Phi")
  
  j4_Phi = sum(eval(dtermo1_s), na.rm=TRUE) - eval(dtermo2) 
  
  dtermo1_s = D(termo1_s,"Pi")
  dtermo2 = D(termo2,"Pi")
  
  j4_Pi = sum(eval(dtermo1_s), na.rm=TRUE) - eval(dtermo2) 
  
  
  
  # solucao da equacao linear ---------------------------------------------
  
  F = t(t(c(f1, f2, f3, f4)))
  
  J1 = c(j1_Alpha, j1_Lambda, j1_Phi, j1_Pi)
  J2 = c(j2_Alpha, j2_Lambda, j2_Phi, j2_Pi)
  J3 = c(j3_Alpha, j3_Lambda, j3_Phi, j3_Pi)
  J4 = c(j4_Alpha, j4_Lambda, j4_Phi, j4_Pi)

  J = rbind(J1, J2, J3, J4)
  y = solve(J, -F)
  
  x = x + y
  x
  
}


# executa o metodo --------------------------------------------------------

# valores iniciais dos parametros
Alpha0 = 1.5
Lambda0 = 0.2
Phi0 = 0.7
Pi0 = 0.8

x = t(t(c(Alpha0, Lambda0, Phi0, Pi0)))
x

e = 1
i = 1
while (e>10e-10) {
  x0 = x
  x = NewtonMethod(x)
  e = sum(abs(x - x0))
  print(c("interação: ", i, "erro: ", e))
  print(x)
  i = i + 1
}


