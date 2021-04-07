#' Title: NBA Fan engagement Case
#' Purpose: LEad, Clean and Save Data as CSV files
#' Author: Christian Romero
#' email: romerop.christian@gmail.com
#' Date: Jan 20 2021

# Libs
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

# Set working directory 
setwd("~/Desktop/HULT academic/MSBA/NLP/hult_NLP_student/cases/NBA Fan Engagement/data")

# Options
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Functions

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}


cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Create stop words
stops <- c(stopwords('SMART'), "nba", "basketball", "ball", "rt", "amp")

# Data
tweets <- read.csv('J_July2020.csv', header=TRUE)


#Sampling
idx <- 1:length(tweets$text)
set.seed(1234)
idx <- sample(idx, 100000)
tweetsmall <- tweets[idx,]

tweetsmall$text <- gsub("[^\x01-\x7F]", "", tweetsmall$text)

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(tweetsmall))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)


# Convert into a DF
df <- data.frame(text = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)

#Save each cleaned DF into a CSV file to avoid CleanCorpus preprocessing times
write.csv(df,"~/Desktop/HULT academic/MSBA/NLP/hult_NLP_student/cases/NBA Fan Engagement/data/May20NBA.csv", row.names = FALSE)


