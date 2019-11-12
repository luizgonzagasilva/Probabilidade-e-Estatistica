
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)


# Bases -------------------------------------------------------------------

int <- readxl::read_xlsx('internacao.xlsx')


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

int_auxiliar2$dias = difftime(int_auxiliar2$dtIncioInternacao, int_auxiliar2$dtFimInternacaoRealAnterior)
int_auxiliar2$flag <- ifelse(int_auxiliar2$dias < 31, 1, 0)

# historia das internações
int_auxiliar2$IdPessoa = as.character(int_auxiliar2$IdPessoa)
ggplot(int_auxiliar2) +
  geom_segment( aes(x=IdPessoa, xend=IdPessoa, y=dtFimInternacaoReal, yend=dtIncioInternacao), color="black") +
  geom_point( aes(x=IdPessoa, y=dtIncioInternacao), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=IdPessoa, y=dtFimInternacaoReal), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")
