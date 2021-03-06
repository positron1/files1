library(tm)
library(ggplot2)

#setwd("C:/Users/Yue Yang/Desktop/R/capstone")
blogs <- readLines("data/sub_en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("data/sub_en_US/en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("data/sub_en_US/en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")

# blogs_byte <- iconv(blogs, "UTF-8", "ASCII", "byte")
# news_byte <- iconv(news, "UTF-8", "ASCII", "byte")
# twitter_byte <- iconv(twitter, "UTF-8", "ASCII", "byte") 
# byte<-list(blogs_byte, news_byte, twitter_byte)

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
 
#Unigrams <- TermDocumentMatrix(corpus)
Unigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = UnigramTokenizer))
Bigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = BigramTokenizer))
Trigrams <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer))
Quadgrams <- TermDocumentMatrix(corpus1, control = list(tokenize = QuadgramTokenizer))

#****************************************************************************************

UnigramsDense <- removeSparseTerms(Unigrams, 0.999)
BigramsDense <- removeSparseTerms(Bigrams, 0.999)
TrigramsDense <- removeSparseTerms(Trigrams, 0.999)
QuadgramsDense <- removeSparseTerms(Quadgrams, 0.999)

unifreq<-freq_frame(UnigramsDense)
head(unifreq,15)
bifreq<-freq_frame(BigramsDense)
head(bifreq,15)
trifreq<-freq_frame(TrigramsDense)
head(trifreq,15)
quadfreq<-freq_frame(QuadgramsDense)
head(quadfreq,15)

#****************************************************************************************
#This part generates the database to be used in predictions
library(RSQLite)
source("files1/functions.R")
db <- dbConnect(SQLite(), dbname="train.db")
dbSendQuery(conn=db,"CREATE TABLE NGram (pre TEXT,word TEXT,freq INTEGER,n INTEGER)")  
# ROWID automatically created with SQLite dialect


# Process with pre and current word
#processGram(freq_5)
processGram(quadfreq)
processGram(trifreq)
processGram(bifreq)

# Insert into SQLite database
sql_5 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 5)"
bulk_insert(sql_5, freq_5)
sql_4 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 4)"
bulk_insert(sql_4, freq_4)
sql_3 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 3)"
bulk_insert(sql_3, freq_3)
sql_2 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 2)"
bulk_insert(sql_2, freq_2)

dbDisconnect(db)
