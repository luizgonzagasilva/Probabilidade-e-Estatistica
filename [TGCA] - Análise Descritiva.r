#setwd("C:\\Users\\deolivev\\Google Drive\\MECAI\\Estatistica")
setwd('C:\\Users\\Vinicius\\Google Drive\\MECAI\\Estatistica')
# !diagnostics off
library(ggplot2)
library(dplyr)
library(tidyverse)
#                                                         #
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

# Remove IDÂ´s relation with other datasets
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
# Motivo: IDÂ´s
  dados[c("_PATIENT", "_cohort", "sample_type_id", "sampleID", "_INTEGRATION")] <- NULL
  
# Remove  _RFS, _RFS_UNIT _RFS_IND and _TIME_TO_EVENT
# Motivo: Mesmo significado das variaveis OSÂ´s
  dados[c("_RFS", "_RFS_UNIT", "_RFS_IND", "_OS_UNIT",
          "_TIME_TO_EVENT", "_TIME_TO_EVENT_UNIT")] <- NULL

# Remove OS IND
# Motivo: Duplicado do EVENT
  # 12400 valores iguais
  sum(dados$`_OS_IND`[!is.na(dados$`_OS_IND`)] == dados$`_EVENT`[!is.na(dados$`_EVENT`)])
  # 410 NAÂ´s
  table(is.na(dados$`_OS_IND`))
  
  dados$`_OS_IND` <- NULL

# 8 VariÃ¡veis Finais
  length(names(dados))
  
  
# ----------------- 2. ANALISE VariÃ¡veis isoladas ------------------------ 

names(dados) <- c('event','os','disease','site','age_ini','gender','sample_type')

summary(dados)
 
# ----------  Event ------------
    # - 410 NAÂ´s 
    table(dados$event)
    sum(is.na(dados$event))
    
    ggplot(dados, aes(x=dados$event)) + geom_bar() + ggtitle('Frequencia Evento') + xlab('Evento')


# ----------  Gender ------------
    # 219 NAÂ´s
    table(dados$gender)
    sum(is.na(dados$gender))
    
    ggplot(dados, aes(x=dados$gender)) + geom_bar() + ggtitle('Frequencia Sexo') + xlab('Sexo')
  
# ----------  Primary Disease ------------
    # 7 NAÂ´s - 34 unique
    unique(dados$disease)
    sum(is.na(dados$disease))
    
    p <- ggplot(dados, aes(x=dados$disease)) + geom_bar() + 
      theme(axis.text.x = element_text(angle = 90)) + ggtitle('Frequencia dos Tipos de Doenças') + xlab('Primary Disease')
    p

# ----------  Primary Site ------------
    # 7 NAÂ´s - 30 unique
    unique(dados$site)
    sum(is.na(dados$site))
    
    p <- ggplot(dados, aes(x=dados$site)) + geom_bar() +
      theme(axis.text.x = element_text(angle = 90)) + ggtitle('Frequencia dos Locais da Doença no Corpo') + xlab('Primary Sites')
    p

# ----------  OS (days) Overall survival time------------
    # 410 NAÂ´s
    summary(dados$os)
    sum(is.na(dados$os))
    
    p <- ggplot(dados, aes(x=dados$os/365)) + geom_histogram(binwidth = 0.5) + 
      scale_x_continuous(breaks = c(0:20), limits = c(0,20)) +
      theme(axis.text.x = element_text(angle = 90)) + 
      ggtitle('Frequencia do tempo do Evento em Anos') + xlab('Anos')
    p

# ----------  Idade Overall survival time
    # 269 NAÂ´s
    summary(dados$age_ini)
    sum(is.na(dados$age_ini))
    
    p <- ggplot(dados, aes(x=dados$age_ini)) + 
      geom_histogram(binwidth = 1) + ggtitle('Idade no começo do diagnóstico') + xlab('Anos')
    p
  
# ----------  Sample ID ----------
    # 79 naÂ´S - 8 UNIQUE
    unique(dados$sample_type)
    sum(is.na(dados$sample_type))  
    
    p <- ggplot(dados, aes(x=dados$sample_type)) + geom_bar() +
      theme(axis.text.x = element_text(angle = 90)) + ggtitle('Frequencia dos Locais da Doença no Corpo') + xlab('Primary Sites')
    p

  
# ----------------- 3. Analise dos NAÂ´s  ------------------------ 

# os - overall surving
# Se OS for NA, nÃ£o faz sentido manter a linha pois Ã© a variÃ¡vel de interesse. EntÃ£o remover todas as linhas onde OS=NA
dados <- dados[!is.na(dados$os),]

# Sobraram 22 linhas com NAÂ´s apenas na age_ini e 4 linhas com NA em sample_type 
#  Talvez seja possÃ�vel aproveitar essas amostras substituindo pela mÃ©dia pra determinada disease.

# age_ini NAÂ´s
dados[is.na(dados$age_ini),]

# sample_type NAÂ´s
dados[is.na(dados$sample_type),]

# ----------------- 4. Separa 3 Diseases mais frequentes  ------------------------ 
freq_disease <- table(dados$disease)

freq_disease <- sort(freq_disease, decreasing = TRUE)

# 3 Diseases mais frequentes/ 4008 linhas
freq_disease[1:3]

# separa base
base_corte <- dados[dados$disease %in% names(freq_disease[1:3]),]

nrow(base_corte)

summary(base_corte)

# ----------------- 5. tratar NAÂ´s  ------------------------ 

dados_na <- base_corte[is.na(base_corte$age_ini),]


base_corte$age_ini[base_corte$disease == 'lung adenocarcinoma']

df_age_lung <- data.frame('age_ini'= base_corte$age_ini[base_corte$disease == 'lung adenocarcinoma'],
                          'gender' = base_corte$gender[base_corte$disease == 'lung adenocarcinoma'],
                          'event'  = base_corte$event[base_corte$disease == 'lung adenocarcinoma'])

# Histograma Idade separado por sexo antes de substituir NA´s
p <- ggplot(df_age_lung, aes(x = df_age_lung$age_ini, fill = df_age_lung$gender)) + 
  geom_histogram(binwidth = 1) + theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(30, 90, 5)) + facet_wrap(df_age_lung$gender) +
  ggtitle('Distribuição da Idade no inicido do tratamento para doença "lung adenocarcinoma" - Antes de substituir NA') + xlab('Anos')
p

# Distribuicao dados NA
p <- ggplot(dados_na, aes(x = dados_na$event, fill = dados_na$gender)) + 
  geom_bar() + theme(legend.position = 'none') +
  facet_wrap(~gender) + xlab('Event') + 
  ggtitle('Distribuição das amostras com NA na variável idade')
p


# MÃ©dia
mean(df_age_lung$age_ini[df_age_lung$gender == 'MALE'], na.rm = TRUE)
mean(df_age_lung$age_ini[df_age_lung$gender == 'FEMALE'], na.rm = TRUE)
# Mediana
median(df_age_lung$age_ini[df_age_lung$gender == 'MALE'], na.rm = TRUE)
median(df_age_lung$age_ini[df_age_lung$gender == 'FEMALE'], na.rm = TRUE)
# Maior Frequencia
sort(table(df_age_lung$age_ini[df_age_lung$gender == 'MALE']), decreasing = TRUE)[1]
sort(table(df_age_lung$age_ini[df_age_lung$gender == 'FEMALE']), decreasing = TRUE)[1]

# Substitui pela Mediana ou qualquer outra ??
base_corte$age_ini[is.na(base_corte$age_ini) & base_corte$gender == 'MALE'] <- median(df_age_lung$age_ini[df_age_lung$gender == 'MALE'], na.rm = TRUE)
base_corte$age_ini[is.na(base_corte$age_ini) & base_corte$gender == 'FEMALE'] <- median(df_age_lung$age_ini[df_age_lung$gender == 'FEMALE'], na.rm = TRUE)

# Sem mais NAÂ´s
sum(is.na(base_corte))

# ----------------- 5. AnÃ¡lise Descritiva parte 2  ------------------------ 
as.factor(base_corte$gender)
as.factor(base_corte$disease)

# Distribuição do tempo do EVENTO para top 3 doenças
p <- ggplot(base_corte, aes(x = base_corte$os/365, fill = base_corte$disease)) + 
  geom_histogram(binwidth = 0.5) + scale_fill_discrete(name = "Doença Principal") +
  scale_x_continuous(breaks = c(0:20), limits = c(0,20)) +
  theme(legend.position = c(0.8, 0.8)) +
  ggtitle('Distribuição do tempo do EVENTO para top 3 doenças') + xlab('Anos')
p

# QUantidade por Sexo
p <- ggplot(base_corte, aes(x = base_corte$disease, fill = base_corte$disease)) + 
  geom_bar() + scale_fill_discrete(name = "Top 3 Doenças") +
  theme(legend.position = c(0.8, 0.8)) +
  facet_wrap(~gender) + theme(axis.text.x = element_text(angle = 45)) +
  ggtitle('QUantidade por Sexo') + xlab('Doenças')
p

# Relação da Doença com a parte do corpo (100% correlação)
p <- ggplot(base_corte, aes(x = base_corte$site, fill = base_corte$site)) + 
  geom_bar() + 
  theme(legend.position = c(0.8, 0.8)) +
  facet_wrap(~disease) + theme(legend.position = 'none') +
  ggtitle('Relação da Doença com a parte do corpo (100% correlação)') + xlab('Local da Doença')
p

# Relação do EVENTO com os tipos de doenças
p <- ggplot(base_corte, aes(x = base_corte$event, fill = base_corte$disease)) + 
  geom_bar() + theme(legend.position = 'none') +
  facet_wrap(~disease) + scale_x_continuous(breaks = c(0:2)) +
  ggtitle('Relação do EVENTO com os tipos de doenças') + xlab('EVENTO')
p

# Classificação de cada doença
p <- ggplot(base_corte, aes(x = base_corte$sample_type, fill = base_corte$site)) + 
  geom_bar() + theme(legend.position = 'none') +
  facet_wrap(~site) + theme(axis.text.x = element_text(angle = 45)) +
  ggtitle('Classificação de cada doença') + xlab('Tipo da doença')
p
# ------------------Idade no Começo do Tratamento por sexo, e doenças --------------------- #

# Distribuição Idade e sexo por lung adenocarcinoma
df_age_lung <- data.frame('age_ini'= base_corte$age_ini[base_corte$disease == 'lung adenocarcinoma'],
                          'gender' = base_corte$gender[base_corte$disease == 'lung adenocarcinoma'])
p <- ggplot(df_age_lung, aes(x = df_age_lung$age_ini, fill = df_age_lung$gender)) + 
  geom_histogram(binwidth = 1) + theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(30, 90, 5)) + facet_wrap(df_age_lung$gender) +
  ggtitle('Distribuição da Idade no inicido do tratamento para doença "lung adenocarcinoma" Com NA tratado') + xlab('Anos')
p

# Distribuição Idade e sexo por breast invasive carcinoma
df_age_breast <- data.frame('age_ini'= base_corte$age_ini[base_corte$disease == 'breast invasive carcinoma'],
                          'gender' = base_corte$gender[base_corte$disease == 'breast invasive carcinoma'])
p <- ggplot(df_age_breast, aes(x = df_age_breast$age_ini, fill = df_age_breast$gender)) + 
  geom_histogram(binwidth = 1) + theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(30, 90, 5)) + facet_wrap(df_age_breast$gender) +
  ggtitle('Distribuição da Idade no inicido do tratamento para doença "breast invasive carcinoma"') + xlab('Anos')
p

# Distribuição Idade e sexo por kidney clear cell carcinoma
df_age_kidney <- data.frame('age_ini'= base_corte$age_ini[base_corte$disease == 'kidney clear cell carcinoma'],
                            'gender' = base_corte$gender[base_corte$disease == 'kidney clear cell carcinoma'])
p <- ggplot(df_age_kidney, aes(x = df_age_kidney$age_ini, fill = df_age_kidney$gender)) + 
  geom_histogram(binwidth = 1) + theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(30, 90, 5)) + facet_wrap(df_age_kidney$gender) +
  ggtitle('Distribuição da Idade no inicido do tratamento para doença "kidney clear cell carcinoma"') + xlab('Anos')
p


#write.csv(base_corte, 'base_corte.csv')

