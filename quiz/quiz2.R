library(tm)
library(ggplot2)
library(RWeka)

#setwd("C:/Users/Yue Yang/Desktop/R/capstone")
blogs <- readLines("data/sub_en_US/sub_en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("data/sub_en_US/sub_en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("data/sub_en_US/sub_en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")


blogs<-sapply(blogs, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up
news<-sapply(news, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up
twitter<-sapply(twitter, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up

corpus_list<-list(blogs,news,twitter)
corpus<-c(blogs,news,twitter)

summary1 <- data.frame('File' = c("Blogs","News","Twitter"),
                      "Object_Size" = sapply(corpus_list, function(x){format(object.size(x),"MB")}),
                      'N_sample' = sapply(corpus_list, function(x){length(x)}),
                      'Total_Char' = sapply(corpus_list, function(x)
                      {sum(nchar(x))}),
                      'Max_Char' = sapply(corpus_list, function(x)
                        {max(nchar(x))}),
                      'Min_Char' = sapply(corpus_list, function(x)
                      {min(nchar(x))}),
                      'Median_Char' = sapply(corpus_list, function(x)
                      {median(nchar(x))})
                      )
head(summary1,10)
#****************************************************************************************
profane_words<-readLines("profanity.txt", warn = FALSE, encoding = "UTF-8")

clean_corpus <- function(corpus, rm_stopwords=TRUE, rm_profanity=TRUE){
  corpus <- VCorpus(VectorSource(corpus))                      
  corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
  corpus <- tm_map(corpus, stripWhitespace) # Remove unneccesary white spaces
  corpus <- tm_map(corpus, removeNumbers) # Remove numbers
  corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
  if(rm_stopwords==TRUE){corpus <- tm_map(corpus1, removeWords, stopwords("english"))}
  if(rm_profanity==TRUE){corpus <- tm_map(corpustr, removeWords, profane_words)}
  corpus <- tm_map(corpus, PlainTextDocument) # Plain text
  return(corpus)
}

corpus<-clean_corpus(corpus)

#****************************************************************************************
uni_tokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi_tokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri_tokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quad_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Controls for the ngrams
uni_control  <- list(tokenize = uni_tokenizer , bounds = list(global = c(5,Inf)))
bi_control   <- list(tokenize = bi_tokenizer  , bounds = list(global = c(3,Inf)))
tri_control  <- list(tokenize = tri_tokenizer , bounds = list(global = c(2,Inf)))
quad_control <- list(tokenize = quad_tokenizer, bounds = list(global = c(2,Inf)))

# Sets the default number of threads to use
options(mc.cores=3)

# Generate the Term document matrix
unigram_tdm  <- TermDocumentMatrix(corpus, control = uni_control)
bigram_tdm   <- TermDocumentMatrix(corpus, control = bi_control)
trigram_tdm  <- TermDocumentMatrix(corpus, control = tri_control)
quadgram_tdm <- TermDocumentMatrix(corpus, control = quad_control)

gc()
#****************************************************************************************

unigram_tdm_dense <- removeSparseTerms(unigram_tdm, 0.9995)
bigram_tdm_dense <- removeSparseTerms(bigram_tdm, 0.9995)
trigram_tdm_dense <- removeSparseTerms(trigram_tdm, 0.9995)
quadgram_tdm_dense <- removeSparseTerms(quadgram_tdm, 0.9995)



df_freq <- function(tdm){
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  df <- data.frame(word=names(freq), freq=freq)
  return(df)
}

uni_freq<-df_freq(unigram_tdm_dense)
bi_freq<-df_freq(bigram_tdm_dense)
tri_freq<-df_freq(trigram_tdm_dense)
quad_freq<-df_freq(quadgram_tdm_dense)

sapply(list(uni_freq,bi_freq,tri_freq,quad_freq), dim)
sapply(list(uni_freq,bi_freq,tri_freq,quad_freq), function(x) summary(x$freq) )

#****************************************************************************************



source("files1/quiz/quiz2_functions.R")

week3("just do that")

#****************************************************************************************
save(unifreq,file="1gram.Rda")
save(bifreq,file="2gram.Rda")
save(trifreq,file="3gram.Rda")
save(quadfreq,file="4gram.Rda")


save(corpus1,file="corpus1.Rda")
save(corpus2,file="corpus2.Rda")
load("files1/corpus1.Rda")
load("files1/corpus2.Rda")


load("files1/1gram.Rda")
load("files1/2gram.Rda")
load("files1/3gram.Rda")
load("files1/4gram.Rda")

load("files1/oneGram.Rdata")

rm(list = ls())
