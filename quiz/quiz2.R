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
#This part is the processing of the corpus by functions from "tm" package
corpus <- VCorpus(VectorSource(corpus))                      
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # Remove unneccesary white spaces
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus1 <- tm_map(corpus, PlainTextDocument) # Plain text
corpus2 <- tm_map(corpus1, removeWords, stopwords("english")) # Not sure if I want to do this


freq_frame <- function(tdm){
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}

#****************************************************************************************
library(RWeka)
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

#Unigrams <- TermDocumentMatrix(corpus)
Unigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = UnigramTokenizer))
Bigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = BigramTokenizer))
Trigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer))
Quadgrams <- TermDocumentMatrix(corpus1, control = list(tokenize = QuadgramTokenizer))
Pentagrams <- TermDocumentMatrix(corpus1, control = list(tokenize = PentagramTokenizer))

#****************************************************************************************

UnigramsDense <- removeSparseTerms(Unigrams, 0.999)
BigramsDense <- removeSparseTerms(Bigrams, 0.999)
TrigramsDense <- removeSparseTerms(Trigrams, 0.999)
QuadgramsDense <- removeSparseTerms(Quadgrams, 0.999)
PentagramsDense <- removeSparseTerms(Pentagrams, 0.9999)

unifreq<-freq_frame(UnigramsDense)
head(unifreq,15)
bifreq<-freq_frame(BigramsDense)
head(bifreq,15)
trifreq<-freq_frame(TrigramsDense)
head(trifreq,15)
quadfreq<-freq_frame(QuadgramsDense)
head(quadfreq,15)
pentafreq<-freq_frame(PentagramsDense)
head(pentafreq,25)
#****************************************************************************************
source("files1/quiz/quiz2_functions.R")

week3 <- function() {
  inputData <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
  for(i in 1:length(inputData)) {
    answer <- paste("Q", i, ": ", paste(getNextWordsSuggestion(inputData[i]), collapse = ","), sep = "")
    print(answer)
  }
}
week3()

