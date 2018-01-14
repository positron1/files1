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
filterNgrams <- function(nGramDf, searchTxt) {
  # Will perl = TRUE incure performance issue ??? Or is it relevant ???
  nGramDf[grep(paste("^", searchTxt, " ", sep = ""), nGramDf$Term, perl = TRUE), ][, c("Term")]
}

##
## Given a text string as input, predict the 3 following possible words
##
getNextWordsSuggestion <- function(inputTxt) {
  suggestedWords <- c()
  nGramDfNames <- c("fiveGramDf", "fourGramDf", "triGramDf", "biGramDf", "oneGramDf") # 4 3 2 1 0
  for (i in 1:length(nGramDfNames)) {
    lowerBound <- 5 - i
    if (getLengthOfWords(inputTxt) < lowerBound) {
      next
    } else {
      if (nGramDfNames[i] == nGramDfNames[5]) {
        suggestedWords <- c(suggestedWords, get(nGramDfNames[i])[1:3, "Term"])
      } else {
        lastNwords <- getLastNwords(inputTxt, lowerBound)
        suggestedWords<- c(suggestedWords, 
                           getLastWords(filterNgrams(get(nGramDfNames[i]), lastNwords)))
      }
    }
  }
  suggestedWords <- subset(suggestedWords, !(suggestedWords %in% stopwords()))
  suggestedWords <- unique(suggestedWords)
  suggestedWords[1:3]
}
