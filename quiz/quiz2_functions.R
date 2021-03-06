## Get last word out of a string
getLastWord <- function (txt, seperator = " ") {
  txtElem <- strsplit(txt, seperator)[[1]]
  txtElem[length(txtElem)]
}

## Get last word out of a vector of strings
getLastWords <- function(txts) {
  lastWords <- c()
  if (length(txts) != 0) {
    for(i in c(1:length(txts)))
      lastWords[i] <- getLastWord(txts[i])
  }
  lastWords
}

getLengthOfWords <- function(txt, seperator = " ") {
  length(strsplit(txt, seperator)[[1]])
}

sanitizeNlastWords <- function(txt, sanitize = TRUE) {
  if (sanitize){
    txt <- tolower(txt)
  }
  txt
}

getLastNwords <- function(txt, n, seperator = " ") {
  txtElems <- strsplit(txt, seperator)[[1]]
  if (length(txtElems) < n) {
    stop("Text length invalid.")
  } else {
    lowerBound <- (length(txtElems) - n + 1)
    txtElems <- txtElems[lowerBound:length(txtElems)]
  }
  lastWords <- paste(txtElems, collapse = " ")
  sanitizeNlastWords(lastWords)
}

##
## Match search text with entries in N Gram data.frame
##
filterNgrams <- function(nGramDf, nminus1GramDf, searchTxt) {
  # Will perl = TRUE incure performance issue ??? Or is it relevant ???
  words<-nGramDf[grep(paste("^", searchTxt, " ", sep = ""), as.character(nGramDf$word), perl = TRUE), ][, c("word")]
  freq1<-nGramDf[grep(paste("^", searchTxt, " ", sep = ""), as.character(nGramDf$word), perl = TRUE), ][, c("freq")]
  freq2<-nminus1GramDf[grep(paste("^", searchTxt, " ", sep = ""), as.character(nGramDf$word), perl = TRUE), ][, c("freq")]
  
  data.frame(words=as.character(df),freq=freq)
  }

##
## Given a text string as input, predict the 3 following possible words
##
getNextWordsSuggestion <- function(inputTxt) {
  suggestedWords <- c()
  nGramDfNames <- c("pentafreq", "quadfreq", "trifreq", "bifreq", "unifreq") # 4 3 2 1 0
  for (i in 1:length(nGramDfNames)) {
    lowerBound <- 5 - i
    if (getLengthOfWords(inputTxt) < lowerBound) {
      next
    } else {
      if (nGramDfNames[i] == nGramDfNames[5]) {
        suggestedWords <- c(suggestedWords, as.character(get(nGramDfNames[i])[1:3, "word"]))
      } else {
        lastNwords <- getLastNwords(inputTxt, lowerBound)
        filterNgrams(get(nGramDfNames[i]),get(nGramDfNames[i-1]), lastNwords)
        
        suggestedWords<- c(suggestedWords,  getLastWords())
      }
    }
  }
  
  
  suggestedWords_final<-c()
  suggestedWords <- unique(suggestedWords)
  suggestedWords_final[1:3]<-suggestedWords[1:3]
  suggestedWords <- subset(suggestedWords, !(suggestedWords %in% stopwords()))
  suggestedWords_final[4:6]<-suggestedWords[1:3]
  suggestedWords_final
  
}

week3 <- function(inputData) {
  for(i in 1:length(inputData)) {
    answer1 <- paste("Prob.", i, " with stopwords", ": ", paste((getNextWordsSuggestion(inputData[i]))[1:3], collapse = ","), sep = "")
    answer2 <- paste("Prob.", i, " without stopwords",": ", paste((getNextWordsSuggestion(inputData[i]))[4:6], collapse = ","), sep = "")
    print(answer1)
    print(answer2)
  }
}

