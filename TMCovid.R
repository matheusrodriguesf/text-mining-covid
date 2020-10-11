# Executar esse bloco caso não tenha o pacote instalado
# install.packages("magrittr")
# install.packages("tm")
# install.packages("wordcloud")

# Executar esse bloco para instalar pacotes auxiliares
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("gridExtra")

# Carregando os pacotes

library(wordcloud)
library(magrittr)
library(tm)
library(tidyverse)
library(tidyr)
library(cluster)
library(tidytext)
library(gridExtra)

#Definição do endereço do ambiente de trabalho
setwd("/home/matheus/pos-graduacao/pln/projeto/text-mining-covid")

## Carregamento dos dados

# Leitura do arquivo por linha
covid <- readLines("/home/matheus/pos-graduacao/pln/projeto/text-mining-covid/PM-COVID-4064-TM.txt")
# Leitura do arquivo por DF
covid_df <- read.delim("/home/matheus/pos-graduacao/pln/projeto/text-mining-covid/PM-COVID-4064-TM.txt", header = FALSE, sep = "\n")
covid_df$doc_id <- seq.int(nrow(covid_df))
covid_df <- covid_df[,c(2,1)]
colnames(covid_df) <- c("doc_id", "text")

# Leitura do arquivo de Tweets covid
tweets_covid <-read_csv('/home/matheus/pos-graduacao/pln/projeto/text-mining-covid/covid19_tweets.csv')

head(covid_df, 5)
tail(covid_df, 5)

print(paste("Número de linhas:", nrow(covid_df)),sep=" ")
print(paste("Número de colunas:",ncol(covid_df),sep=" "))

glimpse(tweets_covid)

head(tweets_covid, 5)
tail(tweets_covid, 5)

print(paste("Número de linhas:", nrow(tweets_covid)),sep=" ")
print(paste("Número de colunas:",ncol(tweets_covid),sep=" "))

tweets_covid<-tweets_covid %>%select(c(user_location, text, source))

# Reduzindo a quantidade de registros a serem analisados
tweets_covid<-head(tweets_covid, 5000)

## Definindo as fontes dos textos
covid_source <- VectorSource(covid)
coviddf_source <- DataframeSource(covid_df)
tweets_covid_source <- VectorSource(tweets_covid$text)

## Transformando em corpus

covid_corpus <- VCorpus(covid_source)
coviddf_corpus <- VCorpus(coviddf_source)
tweets_covid_corpus <- VCorpus(tweets_covid_source)

print(covid_corpus)
print(coviddf_corpus)
covid_corpus[[1]]
covid_corpus[[1]]$content

print(tweets_covid_corpus)
tweets_covid_corpus[[1]]
tweets_covid_corpus[[1]]$content

# Lista de stopwords en, pt e es
stopwords("en"); stopwords("pt"); stopwords("es")
new_stops <- c("COVID","coronavirus", "the", "The", stopwords("en"))

## Aplicando a limpeza e normalização dos Textos
covid_clean <- tm_map(covid_corpus, removeWords, words = c(new_stops))
covid_clean <- tm_map(covid_clean, removePunctuation)
covid_clean <- tm_map(covid_clean, stripWhitespace)
covid_clean[[1]]$content

remover_https_url <-function(texto)gsub("https.*","",  texto)
remover_http_url <-function(texto)gsub("http.*","",  texto)
remover_barra <-function(texto)gsub("/", "", texto)
remover_barras <-function(texto)gsub("\\|", "", texto)
remover_arroba <-function(texto)gsub("@", "", texto)
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(remover_https_url))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(remover_http_url))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(remover_barra))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(remover_barras))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(remover_arroba))                                                        
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(removePunctuation))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(removeNumbers))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, removeWords, words = c(new_stops))
tweets_covid_corpus <-tm_map(tweets_covid_corpus, content_transformer(stripWhitespace))

## Frequência dos Termos 
covid_dtm <- DocumentTermMatrix(covid_clean)
covid_dtm
covid_m1 <- as.matrix(covid_dtm)

covid_tdm <- TermDocumentMatrix(covid_clean)
covid_tdm
covid_m2 <- as.matrix(covid_tdm)
tweets_covid_corpus[[1]]$content

covid_m1[1:10, 1000:1010]
covid_m2[1000:1010, 1:10]

tweet_covid_dtm <- DocumentTermMatrix(tweets_covid_corpus)
tweet_covid_dtm
tweet_covid_tdm <- TermDocumentMatrix(tweets_covid_corpus)
tweet_covid_tdm

tweet_covid_m1 <- as.matrix(tweet_covid_dtm)
tweet_covid_m2 <- as.matrix(tweet_covid_tdm)

tweet_covid_m1[1:10, 1000:1010]
tweet_covid_m2[1000:1010, 1:10]

term_frequency_covid <- rowSums(covid_m2)
term_frequency_covid <- sort(term_frequency_covid, decreasing = TRUE)
df<-head(term_frequency_covid, 100) 
df<-data.frame(df)
head(df, 10)

term_frequency_tweet_covid <- rowSums(tweet_covid_m2)
term_frequency_tweet_covid <- sort(term_frequency_tweet_covid, decreasing = TRUE)
df<-head(term_frequency_tweet_covid, 100)
df<-data.frame(df)
head(df, 10)

freq.terms <- findFreqTerms(covid_tdm, lowfreq = 20)
freq.terms[1:100]

freq.terms <- findFreqTerms(tweet_covid_tdm, lowfreq = 20)
freq.terms[1:100]

## Visualização de Dados
f.word.count <- function(my.list) { sum(stringr::str_count(my.list, "\\S+")) }
df <- data.frame(text.source = c("pub_med", "twitter"), line.count = NA, word.count = NA)
my.list <- list(pub_med = covid_df, twitter = tweets_covid)
df$line.count <- sapply(my.list, length)
df$word.count <- sapply(my.list, f.word.count)

head(df)

ggplot(df, aes(x=text.source, y=line.count)) +
  geom_bar(stat="identity",color="blue",alpha=.6, width=.4) +
  coord_flip() +
  theme_bw()

ggplot(df, aes(x = text.source,y = word.count, fill=text.source)) + 
  geom_bar(stat = "identity",width=0.8) + 
  scale_fill_manual(values = c(
    "#00bfff",
    "#ffff00"
  ))

dispositivos_utilizados<-tweets_covid %>%
  count(source)%>%
  arrange(-n)%>%
  head(10)

ggplot(dispositivos_utilizados, aes(x=source, y=n)) +
  geom_bar(stat="identity",color="blue",fill=rgb(0.1,0.4,0.5,0.7),alpha=.6, width=.4) +
  ggtitle("Gráfico de barra dispositivos utilizados") + 
  xlab("Dispositivos") +
  ylab("Quantidade") + 
  coord_flip() +
  theme_bw()

top_10_location<-tweets_covid %>%
  count(user_location)%>%
  arrange(-n)%>%
  head(10)

ggplot(top_10_location, aes(x = user_location,y = n, fill=user_location)) + 
  geom_bar(stat = "identity",width=0.8) + 
  ggtitle("Gráfico de barras regiões que mais comentaram") + 
  xlab("Localização") +
  ylab("Count") + 
  scale_fill_manual(values = c(
    "#00bfff",
    "#ffff00",
    "#ff9900",
    "#ff0000",
    "#0000ff",
    "#000000",
    "#009900",
    "#cc3300",
    "#f2960d",
    "#ff0000"))

term <- names(term_frequency_covid)
num <- term_frequency_covid
word_freqs <- data.frame(term, num)
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = "blue")


freq <- sort(colSums(as.matrix(covid_dtm)), decreasing=TRUE)   

dtmss <- removeSparseTerms(covid_dtm, 0.80)   
dtmss   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")
fit 
plot(fit, hang=1) 
groups <- cutree(fit, k=6)   
rect.hclust(fit, k=6, border="red")

d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

set.seed(1234)
wordcloud(words = word_freqs$term, freq = word_freqs$num, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


term <- names(term_frequency_tweet_covid)
num <- term_frequency_tweet_covid
word_freqs <- data.frame(term, num)
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = "black")

set.seed(1234)
wordcloud(words = word_freqs$term, freq = word_freqs$num, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

freq <- sort(colSums(tweet_covid_m1), decreasing=TRUE)   
barplot(freq[1:10],col="blue",las=2)

tweets_covid %>% 
  select('text') %>% 
  unnest_tokens(word,'text') %>% 
  count(word,sort=TRUE) %>%
  head(5)

tweets_covid %>%
  unnest_tokens(word, 'text') %>%
  anti_join(stop_words) %>%
  count(word) %>%
  ggplot(aes(n)) +
  geom_histogram() +
  scale_x_log10()

tweets_covid %>%select('text')%>%
  unnest_tokens(bigram, 'text', token = "ngrams", n = 2) %>%
  head()
barplot(freq[1:10],col="lightgreen",las=2)

