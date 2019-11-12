
# Pacotes ----------------------------------------------
library(dplyr)
library(hrbrthemes)
library(ggplot2)
require(corrplot)


# Bases ----------------------------------------------
setwd('Bases')
amb <- readxl::read_xlsx('ambulatorio.xlsx')
int <- readxl::read_xlsx('internacao.xlsx')
crn <- readxl::read_xlsx('cronicos.xlsx')

# Analise Ambulatorio --------------------------------

## Procedimentos
amb_proc <- amb %>% 
  dplyr::group_by(dsProcedimento) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


amb_proc <- amb_proc[order(-amb_proc$total),]
amb_proc <- amb_proc[1:10,]

p <- ggplot(data=amb_proc, aes(x=reorder(dsProcedimento,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Procedimentos", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


## Categoria Exame
amb_cat_exame <- amb %>% 
  dplyr::group_by(dsSubTipoExame) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


amb_cat_exame <- amb_cat_exame[order(-amb_proc$total),]


p <- ggplot(data=amb_cat_exame, aes(x=reorder(dsSubTipoExame,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Categoria Exame", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p



## Hospitais
amb_hospitais <- amb %>% 
  dplyr::group_by(nmPrestadorOperadora) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


amb_hospitais <- amb_hospitais[order(-amb_hospitais$total),]
amb_hospitais <- amb_hospitais[1:10,]

p <- ggplot(data=amb_hospitais, aes(x=reorder(nmPrestadorOperadora,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Hospitais", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

## Idades
amb_etaria <- amb %>% 
  dplyr::group_by(faixaEtariaIDSS, cdIndicadorSexoPessoa) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))



p <- ggplot(data=amb_etaria, aes(x=faixaEtariaIDSS, y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Hospitais", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p <- p + facet_wrap( "cdIndicadorSexoPessoa" )
p



## Prestador
amb_prestador <- amb %>% 
  dplyr::group_by(cdIndicadorSexoPessoa, dsTipoPrestador) %>%
  dplyr::summarise(n = n())


ggplot(data=amb_prestador, aes(x=dsTipoPrestador, y=n)) + 
  geom_bar(stat="identity") + 
  facet_wrap( "cdIndicadorSexoPessoa" ) + 
  labs(x="prestador", y="valor")

ggplot(data=amb_prestador, aes(x=dsTipoPrestador, y=n, fill=cdIndicadorSexoPessoa)) + 
  geom_bar(stat="identity", position = "stack") + 
  labs(x="prestador", y="valor")




ggplot(data=amb_prestador, aes(x=dsTipoPrestador, y=n, fill=cdIndicadorSexoPessoa)) + 
  geom_bar(stat="identity", position = position_dodge(preserve = 'single')) + 
  labs(x="prestador", y="valor")



# relacao sexo procedimento
amp_sexo_proc <- amb %>%
  dplyr::select(c(cdIndicadorSexoPessoa, dsClassProcedimento))

plot(table(amp_sexo_proc),col=c(" lightblue "), main ="Sexo e Procedimentos", xlab="Sexo", ylab="Procedimento", las = 1)


# relacao tipo op. procedimento
amp_top_proc <- amb %>%
  dplyr::select(c(dsTipoPrestador, dsClassProcedimento))

plot(table(amp_top_proc),col=c(" lightblue "), main ="Tipo Operadora e Procedimentos", xlab="Tipo Operadora", ylab="Procedimento", las = 1)


# relacao hospital procedimento 
amp_host_proc <- amb %>%
  dplyr::filter(vrPagoProcedimentoContaPaga > 100) %>%
  dplyr::select(c(PrestadorPrincipal, dsClassProcedimento))
  
plot(table(amp_host_proc),col=c(" lightblue "), main ="Tipo Operadora e Procedimentos", xlab="Tipo Operadora", ylab="Procedimento", las = 4)




# relacao procedimento valor gasto
amb_proc_vlr <- amb %>% 
  dplyr::select(c(dsClassProcedimento, vrPagoProcedimentoContaPaga))

v_exame <- amb_proc_vlr %>%
  dplyr::filter(dsClassProcedimento == 'Exames') %>%
  dplyr::select(exame=c(vrPagoProcedimentoContaPaga))

v_prontosocorro <- amb_proc_vlr %>%
  dplyr::filter(dsClassProcedimento == 'Pronto-Socorro') %>%
  dplyr::select(prontosocorro=c(vrPagoProcedimentoContaPaga))

v_terapia <- amb_proc_vlr %>%
  dplyr::filter(dsClassProcedimento == 'Terapia') %>%
  dplyr::select(terapia=c(vrPagoProcedimentoContaPaga))

v_consulta <- amb_proc_vlr %>%
  dplyr::filter(dsClassProcedimento == 'Consulta Eletiva') %>%
  dplyr::select(consulta=c(vrPagoProcedimentoContaPaga))

v_outros <- amb_proc_vlr %>%
  dplyr::filter(dsClassProcedimento == 'Outros procedimentos ambulatoriais') %>%
  dplyr::select(outros=c(vrPagoProcedimentoContaPaga))

boxplot(c(v_consulta,v_exame, v_prontosocorro, v_terapia, v_outros), name=c("consulta","exame","prontosocorro","terapia","outros"), col="orange", border="brown")


# total $ por tipo de procedimento 
amb_proc_vlr_grp <- amb_proc_vlr %>%
  dplyr::group_by(dsClassProcedimento) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


barplot(amb_proc_vlr_grp$total,
        main = "Classe do Procedimento",
        xlab = "Classe Procedimento",
        ylab = "Gasto Total",
        names.arg = amb_proc_vlr_grp$dsClassProcedimento,
        col = "darkred")



# relacao exame vs. valor
amb_exm_vlr <- amb %>% 
  dplyr::select(c(dsTipoExame, vrPagoProcedimentoContaPaga))

v_img <- amb_exm_vlr %>%
  dplyr::filter(dsTipoExame == 'Imagem') %>%
  dplyr::select(imagem=c(vrPagoProcedimentoContaPaga))

v_elemeca <- amb_exm_vlr %>%
  dplyr::filter(dsTipoExame == 'Eletrofisiológicos/mecânico') %>%
  dplyr::select(elemec=c(vrPagoProcedimentoContaPaga))

v_lab <- amb_exm_vlr %>%
  dplyr::filter(dsTipoExame == 'Laboratoriais') %>%
  dplyr::select(lab=c(vrPagoProcedimentoContaPaga))

v_patologia <- amb_exm_vlr %>%
  dplyr::filter(dsTipoExame == 'Anatomia patológica e citopatológica') %>%
  dplyr::select(patologia=c(vrPagoProcedimentoContaPaga))

v_examespc <- amb_exm_vlr %>%
  dplyr::filter(dsTipoExame == 'Demais exames especiais') %>%
  dplyr::select(examespc=c(vrPagoProcedimentoContaPaga))


boxplot(c(v_img,v_elemeca, v_lab, v_patologia, v_examespc), col="orange", border="brown")



# total $ por tipo de exame 
amb_exm_vlr_grp <- amb_exm_vlr[!is.na(amb_exm_vlr$dsTipoExame),] %>%
  dplyr::group_by(dsTipoExame) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


barplot(amb_exm_vlr_grp$total,
        main = "Tipo do Exame",
        xlab = "Tipo Exame",
        ylab = "Gasto Total",
        names.arg = amb_exm_vlr_grp$dsTipoExame,
        col = "darkred")


# frenquencia tipo de procedimento por sexo
count <- amb %>% 
  dplyr::select(c(dsClassProcedimento, cdIndicadorSexoPessoa)) 

count <- table(count$cdIndicadorSexoPessoa, count$dsClassProcedimento)

barplot(count, main="Car Distribution", beside=TRUE, col=c("red","darkblue"))
legend("topright", rownames(count), fill=c("red","darkblue"), bty = "n", cex = 0.4)





# frenquencia tipo de procedimento por reinternação
proc_creinternacao <- amb %>% 
  dplyr::filter(IdPessoa==43804587) %>%
  dplyr::select(c(dsClassProcedimento)) 

proc_creinternacao$flag = "S"


proc_sreinternacao <- amb %>% 
  dplyr::filter(IdPessoa!=43804587) %>%
  dplyr::select(c(dsClassProcedimento)) 


proc_sreinternacao$flag = "N"

proc_reinternacao <- merge(proc_sreinternacao, proc_creinternacao, all = T)

count <- table(proc_reinternacao$dsClassProcedimento, proc_reinternacao$flag)

barplot(count, main="Car Distribution", beside=TRUE)
legend("topright", rownames(count), bty = "n", cex = 0.4)

# internacoes ------------------

int$dtIncioInternacao <- as.Date(int$dtIncioInternacao)
int$dtFimInternacaoReal <- as.Date(int$dtFimInternacaoReal)

## quantidade de internacoes por pessoa
qt_int <- int %>% 
  dplyr::group_by(IdPessoa) %>% 
  dplyr::summarise(qtde_int = sum(!is.na(unique(IdInternacao)))) 

int <- int %>%  
  dplyr::left_join(qt_int, by = "IdPessoa")

# flag reinternacao
int_auxiliar <- int %>% 
  dplyr::select(c(IdPessoa, IdInternacao, dtIncioInternacao, dtFimInternacaoReal, qtde_int)) %>% 
  dplyr::distinct() %>% 
  dplyr::distinct() %>% 
  dplyr::filter(qtde_int > 0)


# historia das internacoes
int_auxiliar$IdPessoaStr = as.character(int_auxiliar$IdPessoa)
ggplot(int_auxiliar) +
  geom_segment( aes(x=IdPessoaStr, xend=IdPessoaStr, y=dtFimInternacaoReal, yend=dtIncioInternacao), color="black") +
  geom_point( aes(x=IdPessoaStr, y=dtIncioInternacao), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=IdPessoaStr, y=dtFimInternacaoReal), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")


## Classe Procedimentos
int_cproc <- int %>% 
  dplyr::group_by(dsClassProcedimento) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


int_cproc <- int_cproc[order(-int_cproc$total),]
int_cproc <- int_cproc[1:10,]

p <- ggplot(data=int_cproc, aes(x=reorder(dsClassProcedimento,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Procedimentos", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p



## Espec. Procedimentos
int_eproc <- int %>% 
  dplyr::group_by(especialidadeProc) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


int_eproc <- int_eproc[order(-int_eproc$total),]
int_eproc <- int_eproc[1:10,]

p <- ggplot(data=int_eproc, aes(x=reorder(especialidadeProc,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Procedimentos", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p



## Prestador Principal
int_prestador <- int %>% 
  dplyr::group_by(nmPrestadorPrincipal) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


int_prestador <- int_prestador[order(-int_prestador$total),]
int_prestador <- int_prestador[1:10,]

p <- ggplot(data=int_prestador, aes(x=reorder(nmPrestadorPrincipal,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Procedimentos", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


## Grupo CID
int_grupocid <- int %>% 
  dplyr::group_by(dsGrupoCID) %>%
  dplyr::summarise(total = sum(vrPagoProcedimentoContaPaga))


int_grupocid <- int_grupocid[order(-int_grupocid$total),]
int_grupocid <- int_grupocid[1:10,]

p <- ggplot(data=int_grupocid, aes(x=reorder(dsGrupoCID,-total) , y=total)) 
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + labs(x="Procedimentos", y="valor")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p



# cronicos -------------------------------------------------
subcrn <- crn[c("isDiabetes","isDislipidemia","isHipertensaoArterial","isInfectocontagiosas","isRenal","isReumatologico","isSaudeMental","cdClassMultimorbidade")]

dist.crn = as.matrix(dist(subcrn))

corrplot(cor( subcrn, use = "complete.obs"), method = "circle", order = "hclust", addrect = 3, shade.col=NA, tl.col="black", tl.srt =45, addCoef.col="black")
corrplot(cor( subcrn, use = "complete.obs"), method = "circle",  shade.col=NA, tl.col="black", tl.srt =45, addCoef.col="black")










