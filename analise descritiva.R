
# Pacotes -----------------------------------------------------------------

library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(extrafont)
library(ggthemes)
library(gridExtra)
library(hrbrthemes)
require(tm)
require(tidyverse)
require(lubridate)
require(RColorBrewer)
require(SnowballC)
require(wordcloud)

# Bases -------------------------------------------------------------------

setwd('Bases')
amb <- readxl::read_xlsx('ambulatorio.xlsx')
int <- readxl::read_xlsx('internacao.xlsx')
crn <- readxl::read_xlsx('cronicos.xlsx')

# Criacao de variaveis - Internacao ----------------------------------------------------

## dias internado
int$dtIncioInternacao <- as.Date(int$dtIncioInternacao)
int$dtFimInternacaoReal <- as.Date(int$dtFimInternacaoReal)

int$dias_internado <- difftime(int$dtFimInternacaoReal, int$dtIncioInternacao, unit = 'days')
int$dias_internado <- as.numeric(int$dias_internado)
int$dias_internado <- int$dias_internado + 1

## quantidade de internacoes por pessoa
qt_int <- int %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(qtde_int = sum(!is.na(unique(IdInternacao)))) 

int <- int %>%  
  dplyr::left_join(qt_int, by = "IdPessoa")

## flag reinternacao
int_auxiliar <- int %>% 
  dplyr::select(c(IdPessoa, IdInternacao, dtIncioInternacao, dtFimInternacaoReal, qtde_int)) %>% 
  dplyr::distinct() %>% 
  dplyr::distinct() %>% 
  dplyr::filter(qtde_int > 1)

################################ criando coluna adicional com data da ultima alta ################################
int_auxiliar <- int_auxiliar[order(int_auxiliar$IdPessoa, int_auxiliar$dtIncioInternacao),]

int_auxiliar$indice <- 1:nrow(int_auxiliar)

int_auxiliar_antes = data.frame(IdPessoa=int_auxiliar$IdPessoa,indice=int_auxiliar$indice+1, dtFimInternacaoRealAnterior=int_auxiliar$dtFimInternacaoReal)

int_auxiliar2 <- int_auxiliar %>% 
  dplyr::left_join(int_auxiliar_antes, c("dtFimInternacaoRealAnterior"), by = c("IdPessoa"="IdPessoa", "indice"="indice"))
#################################################################################################################


#flag <- int_auxiliar %>% 
#  dplyr::group_by(IdPessoa) %>% 
#  dplyr::summarise(dias = difftime(max(dtIncioInternacao), min(dtFimInternacaoReal)))
int_auxiliar2$dias = difftime(int_auxiliar2$dtIncioInternacao, int_auxiliar2$dtFimInternacaoRealAnterior)


# flag$flag <- ifelse(flag$dias < 31, 1, 0)
int_auxiliar2$flag <- ifelse(int_auxiliar2$dias < 31, 1, 0)


flag <- data.frame(IdPessoa=int_auxiliar2$IdPessoa, dias=int_auxiliar2$dias, flag=int_auxiliar2$flag)

int <- int %>% 
  dplyr::left_join(flag, by = 'IdPessoa')

int$dias <- ifelse(is.na(int$dias) == TRUE, 0, int$dias)
int$flag <- ifelse(is.na(int$flag) == TRUE, 0, int$flag)

## numero de procedimentos realizados durante a internacao
proc <- int %>% 
  dplyr::group_by(IdInternacao) %>% 
  dplyr::summarise(num_proc = sum(!is.na(idprocedimento))) 

int <- int %>%  
  dplyr::left_join(proc, by = "IdInternacao")

## preco total de cada internacao
preco <- int %>% 
  dplyr::group_by(IdInternacao) %>% 
  dplyr::summarise(valor_total_int = sum(vrPagoProcedimentoContaPaga))

int <- int %>% 
  dplyr::left_join(preco, by = "IdInternacao")

## preco medio da internacao (em dias)
int$valor_medio_int <- int$valor_total_int/int$dias_internado


# Criacao de variaveis - Ambulatorio --------------------------------------

## trazer data da primeira internacao
data_final <- int %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(data_int = min(dtIncioInternacao))

amb <- amb %>% 
  dplyr::left_join(data_final, by = 'IdPessoa')

## filtar apenas procedimentos ambulatoriais anterior a data internacao
amb$dtAtendimentoContaPaga <- as.Date(amb$dtAtendimentoContaPaga)

amb$passagem_anterior <- as.factor(ifelse(
  as.numeric(difftime(amb$data_int, amb$dtAtendimentoContaPaga, units = 'days')) > 0,
  'SIM', 'NAO'))

amb <- amb %>% 
  dplyr::filter(passagem_anterior == 'SIM')
  
## numero de vezes que foi ao ambulatorio
qtde_amb <- amb %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(qtde = sum(!is.na(unique(IdPassagem)))) 

amb <- amb %>%  
  dplyr::left_join(qtde_amb, by = "IdPessoa")

## numero medio de vezes em amb por mes
num_mes <- amb %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(num_mes = sum(!is.na(unique(paste(AnoCompetencia, MesCompetencia)))))

amb <- amb %>%  
  dplyr::left_join(num_mes, by = "IdPessoa")

amb$qtde_mes <- (amb$qtde/amb$num_mes)

## preco total gasto em ambulatorio por pessoa
preco <- amb %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(valor_total_amb = sum(vrPagoProcedimentoContaPaga))

amb <- amb %>% 
  dplyr::left_join(preco, by = "IdPessoa")

## preco medio (por passagem) gasto em ambulatorio por pessoa
amb$valor_medio_amb <- amb$valor_total_amb/amb$qtde


# Criacao de variaveis - Cronicos --------------------------------------

## trazer data da primeira internacao
data_final <- int %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(data_int = min(dtIncioInternacao))

crn <- crn %>% 
  dplyr::left_join(data_final, by = 'IdPessoa')

crn$mes <- lubridate::month(ymd(crn$data_int))
crn$ano <- lubridate::year(ymd(crn$data_int))

## filtrar cronicos com referencia a data de internacao
crn <- crn %>% 
  dplyr::filter(paste(AnoCompetencia, MesCompetencia) == paste(ano, mes))

## criar flag cronicos
crn$flag_cronico <- ifelse(crn$isCardiopatias == 0 |
                             crn$isDiabetes == 0 |
                             crn$isDislipidemia == 0 |
                             crn$isHipertensaoArterial == 0 |
                             crn$isInfectocontagiosas == 0 |
                             crn$isOncologico == 0 |
                             crn$isOncologico == 0 |
                             crn$isPulmonar == 0 |
                             crn$isRenal == 0 |
                             crn$isReumatologico == 0 |
                             crn$isSaudeMental == 0,
                           1, 0)


# Base descritiva ----------------------------------------------------------------

## preparar base apenas com variaveis da analise descritiva
int_analise <- int %>% 
  dplyr::select(c(IdPessoa, IdInternacao, idade, cdIndicadorSexoPessoa,
                  dsTipoBeneficiario, dsGrauParentesco,
                  dias_internado, dias, flag, num_proc,
                  valor_total_int, valor_medio_int)) %>% 
  dplyr::distinct()

## fazer media das idades
idades <- int %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(idade2 = mean(idade))

int_analise <- int_analise %>% 
  dplyr::left_join(idades, by = 'IdPessoa') %>% 
  dplyr::select(-idade) %>% 
  dplyr::distinct()

## juntar com base do ambulatorio
int_analise <- int_analise %>% 
  dplyr::left_join(amb, by = 'IdPessoa') %>% 
  dplyr::select(c(IdPessoa, IdInternacao, idade2, cdIndicadorSexoPessoa.x,
           dsTipoBeneficiario.x, dsGrauParentesco.x,
           dias_internado, dias, flag, num_proc,
           valor_total_int, valor_medio_int, qtde, 
           qtde_mes, valor_medio_amb, valor_total_amb)) %>% 
  dplyr::distinct()

## juntar com base de cronicos
int_analise_final <- crn %>% 
  dplyr::select(c(IdPessoa, flag_cronico)) %>%
  dplyr::right_join(int_analise, by = 'IdPessoa')

int_analise_final$flag_cronico <- ifelse(is.na(int_analise_final$flag_cronico) == TRUE,
                                         0, int_analise_final$flag_cronico)

## base sem duplicatas de pessoas
int_analise_sem_dup <- int_analise_final %>% 
  dplyr::select(-c(IdInternacao, num_proc, valor_medio_int, valor_total_int)) %>% 
  dplyr::distinct()

## conferencia de id unico
sum(duplicated(int_analise_sem_dup$IdPessoa)==TRUE)

int_analise_sem_dup$flag_cronico <- as.factor(int_analise_sem_dup$flag_cronico) 
int_analise_sem_dup$flag <- as.factor(int_analise_sem_dup$flag)   


# Graficos e tabelas ----------------------------------------------------------------

## flag reinternacao
ggplot(int_analise_sem_dup) +
  geom_bar(aes(x = flag), fill = 'red3', width = 0.5) +
  ggtitle("Reinternação") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Quantidade") +
  theme(axis.title.x = element_blank())

## sexo
ggplot(int_analise_sem_dup) +
  geom_bar(aes(x = cdIndicadorSexoPessoa.x), fill = 'red3', width = 0.5) +
  ggtitle("Sexo") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Quantidade") +
  theme(axis.title.x = element_blank())

## idade
qplot(y = int_analise_sem_dup$idade2, x= 1, geom = "boxplot") +
  labs(y = "Idade (em anos)", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Idade") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_sem_dup$idade2)

## tipo de beneficiario
ggplot(int_analise_sem_dup) +
  geom_bar(aes(x = dsTipoBeneficiario.x), fill = 'red3', width = 0.5) +
  ggtitle("Tipo de Beneficiario") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Quantidade") +
  theme(axis.title.x = element_blank())

## quantidade de cronicos
ggplot(int_analise_sem_dup) +
  geom_bar(aes(x = flag_cronico), fill = 'red3', width = 0.5) +
  ggtitle("Crônicos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Quantidade") +
  theme(axis.title.x = element_blank())

## dias de permanencia
qplot(y = int_analise_final$dias_internado, x= 1, geom = "boxplot") +
  labs(y = "Dias", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Duracao da Internacao") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_final$dias_internado)

## numero de procedimentos
qplot(y = int_analise_final$num_proc, x= 1, geom = "boxplot") +
  labs(y = "Quantidade", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Numero de Procedimentos Realizados") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_final$num_proc)

# ## preco total da internacao
# ggplot(int_analise_final, aes(x = valor_total_int)) +
#   geom_histogram(fill = 'red3') +
#   ggtitle("Valor Total da Internacao") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab("Valor (R$)") +
#   theme(axis.title.x = element_blank())

## preco total da internacao
qplot(y = int_analise_final$valor_total_int, x= 1, geom = "boxplot") +
  labs(y = "Valor (R$)", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Valor Total da Internacao") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_final$valor_total_int)

# ## preco medio da internacao
# ggplot(int_analise_final, aes(x = valor_medio_int)) +
#   geom_histogram(fill = 'red3') +
#   ggtitle("Valor Medio da Internacao (por procedimento)") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab("Valor (R$)") +
#   theme(axis.title.x = element_blank())

## preco medio da internacao
qplot(y = int_analise_final$valor_medio_int, x= 1, geom = "boxplot") +
  labs(y = "Valor (R$)", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Valor Medio da Internacao (por procedimento)") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_final$valor_medio_int)

## passagens em ambulatorio
qplot(y = int_analise_sem_dup$qtde, x= 1, geom = "boxplot") +
  labs(y = "Quantidade", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Passagens em Ambulatorio") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_sem_dup$qtde)

## numero medio passagens em ambulatorio (por mes)
qplot(y = int_analise_sem_dup$qtde_mes, x= 1, geom = "boxplot") +
  labs(y = "Quantidade", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Passagens em Ambulatorio (por mes)") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_sem_dup$qtde_mes)

## preco medio da passagem em ambulatorio
qplot(y = int_analise_sem_dup$valor_medio_amb, x= 1, geom = "boxplot") +
  labs(y = "Valor (R$)", x = ' ') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Valor Medio da Passagem em Ambulatorio") +
  theme(plot.title = element_text(hjust = 0.5))

summary(int_analise_sem_dup$valor_medio_amb)


# WordCloud ---------------------------------------------------------------

ambulatorio <- readxl::read_xlsx('ambulatorio.xlsx')
internacao <- readxl::read_xlsx('internacao.xlsx')
cronicos <- readxl::read_xlsx('cronicos.xlsx')

### analise para ambulatorio

ambulatorio$data_atendimento <- as.Date(ambulatorio$dtAtendimentoContaPaga)

procedimentos <- as.character(ambulatorio$dsProcedimento)

procedimentos <- paste(procedimentos,collapse = " ")

docs <- Corpus(VectorSource(procedimentos))



docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("portuguese"))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)


inspect(docs)


### matriz com frequencia dos termos

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="red3", main ="Palavras Mais Frequentes (Ambulatório)",
        ylab = "Frequência das palavras")

