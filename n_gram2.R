#This script uses quanteda to manage corpus
library(tm)
library(ggplot2)

#setwd("C:/Users/Yue Yang/Desktop/R/capstone")
blogs <- readLines("data/sub_en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("data/sub_en_US/en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("data/sub_en_US/en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")


blogs<-sapply(blogs, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up
news<-sapply(news, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up
twitter<-sapply(twitter, function(x) iconv(x, "latin1", "ASCII", sub="")) #cleaning up

corpus_list<-list(blogs,news,twitter)
corpus<-c(blogs,news,twitter)

#****************************************************************************************
#Old "tm" stuff, can be circumvented.
# corpus <- VCorpus(VectorSource(corpus))                      
# corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
# corpus <- tm_map(corpus, stripWhitespace) # Remove unneccesary white spaces
# corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
# corpus <- tm_map(corpus, removeNumbers) # Remove numbers
# corpus <- tm_map(corpus, PlainTextDocument) # Plain text
# corpus <- tm_map(corpus, removeWords, stopwords("english")) # Not sure if I want to do this
# 
# freq_frame <- function(tdm){
#   freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#   freq_frame <- data.frame(word=names(freq), freq=freq)
#   return(freq_frame)
# }

#****************************************************************************************

library(quanteda)
q_corpus<-corpus(corpus)

#If you want to remove stopwords, then...
tokensAll <- tokens(char_tolower(corpus), remove_numbers=TRUE,  remove_punct=TRUE,  remove_separators=TRUE,
                    remove_twitter=TRUE)
tokensNoStopwords <- tokens_remove(tokensAll, stopwords("english"))
tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 2)
dfm_2g<-dfm(tokensNgramsNoStopwords)

#This is, you don't care about stopwords...
# dfm_2g <- dfm(q_corpus,
#            remove_numbers=TRUE,  remove_punct=TRUE,  remove_separators=TRUE,
#            remove_twitter=TRUE, remove = NULL, ngrams = 2, concatenator = " ")


dfm_2g_dense<-dfm_trim(dfm_2g, sparsity = 0.999)

#If you don't mind loading 2 additional packages...
# library(plyr)
# library(data.table)
# dt<-data.table(ngram=colnames(dfm_2g_dense), freq=colSums(dfm_2g_dense)) %>% arrange(desc(freq))
# head(dt,10)

freq_frame <- function(tdm){
  freq <- sort(colSums(as.matrix(tdm)), decreasing=TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}

df_freq<-freq_frame(dfm_2g_dense)
head(df_freq,20)


#****************************************************************************************
#ngram works with vectors, still don't know how to do text-mining without Vcorpus
# library(ngram)
# 
# corpus_2grams<-sapply(corpus, function(x) ngram_asweka(x, min=2, max=2))
# 
# Bigrams <- TermDocumentMatrix(corpus_2grams)
# 
# 
# BigramsDense <- removeSparseTerms(Bigrams, 0.995)
# UnigramsDense <- removeSparseTerms(Unigrams, 0.999)
# 
# unifreq<-freq_frame(UnigramsDense)
# head(unifreq,15)
# bifreq<-freq_frame(BigramsDense)
# head(bifreq,15)