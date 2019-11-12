require(tm)
require(tidyverse)
require(lubridate)
require(RColorBrewer)
require(SnowballC)
require(wordcloud)

cronicos <- read.csv2("cronicos.csv")
internacao <- read.csv2("internacao.csv")
ambulatorio <- read.csv2("ambulatorio.csv")

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
        col ="lightblue", main ="Palavras mais frequentes",
        ylab = "Frequência das palavras")


amb <- ambulatorio %>%
  select(IdPessoa, data_atendimento) 

data_atendimento <- unique(amb$data_atendimento  )
idpessoa <- unique(amb$IdPessoa)

pessoas <- vector("list", length = length(idpessoa))
j <- 1
qiq <- vector("list", length = length(idpessoa))


for(i in idpessoa){
pessoas[[j]] <- amb %>%
  filter(IdPessoa == i)

qiq[[j]] <- pessoas[[j]] %>%
  count(data_atendimento)

qiq[[j]]$idpessoa <- unique(pessoas[[j]]$IdPessoa)

qiq[[j]]$reinternacao <- 0

qiq[[j]] <- as.data.frame(qiq[[j]])
  

j <- j + 1  

}


for(j in 1:length(qiq)){
for(i in 1:(nrow(qiq[[j]]) - 1)){
  
  if(isTRUE(unique(qiq[[j]][i + 1,1] == seq.Date(from = qiq[[j]][i,1] + 1,to = qiq[[j]][i,1]+ 30,by = 1))[2]) == TRUE)
  qiq[[j]][i + 1,"reinternacao"] <- 1 + qiq[[j]][i, "reinternacao"]
  else{
    qiq[[j]][i + 1,"reinternacao"] <- 0
  }    

  }
}

df <- matrix(ncol = 4)
df <- as.data.frame(df)
df_reinternacao <- matrix(ncol = 4)
df_reinternacao <- as.data.frame(df_reinternacao)
names(df_reinternacao) <- c("data_atendimento","n","idpessoa","reinternacao")
df_reinternacao <- df_reinternacao[-1,]

for(i in 1:length(qiq)){

df <- qiq[[j]]
df_reinternacao <- rbind(df_reinternacao,df)

}

