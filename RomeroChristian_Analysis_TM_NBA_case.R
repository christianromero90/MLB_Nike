#' Title: NBA Fan engagement Case
#' Purpose: Analyze twitter fan engagement data
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
library(pbapply)


# Set working directory 
setwd("~/Desktop/HULT academic/MSBA/NLP/hult_NLP_student/cases/NBA Fan Engagement/data")

# Options
customStopwords <- c("season", "team", "celtics", "game", "lakers", "years", "ago", "today",  "orleans", "pelicans","york knicks", "new york", "los angeles", "atlanta hawks", "charlotte hornets", "dallas mavericks", "golden state warriors", "la clippers", "miami heat", "new orleans pelicans", "orlando magic", "portland trail blazers", "toronto raptors", "boston celtics", "chicago bulls", "denver nuggets", "houston rockets", "la lakers", "milwaukee bucks", "new york knicks", "philadelphia sixers", "sacramento kings", "utah jazz", "brooklyn nets", "cleveland cavaliers", "detroit pistons", "indiana pacers", "memphis grizzlies", "minnesota timberwolves", "oklahoma city thunder", "phoenix suns", "san antonio spurs", "washington wizards")

bigramTokens <- function(x) unlist(lapply(NLP::ngrams(words(x),2),paste,collapse=" "),use.names=FALSE)

options(scipen = 999)

options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'October20NBA|September20NBA|August20NBA|July20NBA|June20NBA|May20NBA')

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Vector Corpus; omit the meta data
Oct <- VCorpus(VectorSource(October20NBA.csv$text))
Sept <- VCorpus(VectorSource(September20NBA.csv$text))
Aug <- VCorpus(VectorSource(August20NBA.csv$text))
July <- VCorpus(VectorSource(July20NBA.csv$text))
June <- VCorpus(VectorSource(June20NBA.csv$text))
May <- VCorpus(VectorSource(May20NBA.csv$text))

#Remove Extra Stop Words
Oct <- tm_map(Oct, removeWords, customStopwords)
Sept <- tm_map(Sept, removeWords, customStopwords)
Aug <- tm_map(Aug, removeWords, customStopwords)
July <- tm_map(July, removeWords, customStopwords)
June <- tm_map(June, removeWords, customStopwords)
May <- tm_map(May, removeWords, customStopwords)

# Another way to extract the cleaned text 
Oct<- unlist(pblapply(Oct, content))
Sept<- unlist(pblapply(Sept, content))
Aug<- unlist(pblapply(Aug, content))
July<- unlist(pblapply(July, content))
June<- unlist(pblapply(June, content))
May<- unlist(pblapply(May, content))

# Collapse each into a single "subject" ie a single document
Oct <- paste(Oct, collapse = ' ')
Sept <- paste(Sept, collapse = ' ')
Aug <- paste(Aug, collapse = ' ')
July <- paste(July, collapse = ' ')
June <- paste(June, collapse = ' ')
May <- paste(May, collapse = ' ')


# Combine the subject documents into a corpus of *n* documents
SixMonths <- c(May, June, July, Aug, Sept, Oct)
SixMonths <- VCorpus((VectorSource(SixMonths)))

#Corpus sept
vectorsept <- c(Sept)
corpsept <- VCorpus((VectorSource(vectorsept)))

#Corpus oct
vectoroct <- c(Oct)
corpoct <- VCorpus((VectorSource(vectoroct)))

#Combine May-June
septoct <- c(Sept, Oct)
septoct <- VCorpus((VectorSource(septoct)))


# Make TDM
control = list(tokenize=bigramTokens)
NBATDM  <- TermDocumentMatrix(SixMonths)
NBATDMm <- as.matrix(NBATDM)

# name Columns
colnames(NBATDMm)  <- c('May', 'June', 'July', 'Aug', 'Sept', 'Oct')


commonality.cloud(NBATDMm, 
                  max.words=100, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(2.5,0.25))


# Comparison Cloud

control = list(tokenize=bigramTokens)
NBAsixmonthsTDM  <- TermDocumentMatrix(SixMonths, control = control)
NBAsixmonthsTDMm <- as.matrix(NBAsixmonthsTDM)


# Make sure order is the same as the c(objA, objB) on line ~80
colnames(NBAsixmonthsTDMm) <- c('Oct', 'Sept', 'Aug', 'July', 'June', 'May')

# Make comparison cloud
comparison.cloud(NBATDMm, 
                 max.words=150, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(NBATDMm),"Dark2"),
                 scale=c(3,0.1))



### get the most frquent terms
topTermsA <- rowSums(NBATDMm)

# Add the terms
topTermsA <- data.frame(terms = rownames(NBATDMm), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

# Simple barplot; values greater than 'X'
topWords      <- subset(topTermsA, topTermsA$freq >= 12000)
topWords      <- topWords[order(topWords$freq, decreasing=F),]

# Chg to factor for ggplot
topWords$terms <- factor(topWords$terms, 
                         levels=unique(as.character(topWords$terms))) 

ggplot(topWords, aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)


######


# Inspect word associations
associations <- findAssocs(NBATDM, 'nike', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF[1:10,]

# Make a dot plot
ggplot(assocDF[1:10,], aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF[1:10,], col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 



##  MORE WORD ASSOCIATION

word_associate(October20NBA.csv$text, 
               match.string = c('nike', 'nxt'), 
               stopwords = customStopwords,
               wordcloud = T,
               cloud.colors = c('blue','orange'))



# WORD COUNTS
nikeMay    <- grepl("nike", May20NBA.csv$text, ignore.case=TRUE)
adidasMay <- grepl("adidas", May20NBA.csv$text, ignore.case=TRUE)

nike_pct_may = sum(nikeMay) / nrow(May20NBA.csv)
adi_pct_may = sum(adidasMay) / nrow(May20NBA.csv)

nikeJun    <- grepl("nike", June20NBA.csv$text, ignore.case=TRUE)
adidasJun <- grepl("adidas", June20NBA.csv$text, ignore.case=TRUE)

nike_pct_jun = sum(nikeJun)/nrow(June20NBA.csv)
adi_pct_jun = sum(adidasJun) / nrow(June20NBA.csv)

nikeJuly    <- grepl("nike", July20NBA.csv$text, ignore.case=TRUE)
adidasJuly <- grepl("adidas", July20NBA.csv$text, ignore.case=TRUE)

nike_pct_jul = sum(nikeJuly)/nrow(July20NBA.csv)
adi_pct_jul = sum(adidasJuly) / nrow(July20NBA.csv)

nikeAug    <- grepl("nike", August20NBA.csv$text, ignore.case=TRUE)
adidasAug <- grepl("adidas", August20NBA.csv$text, ignore.case=TRUE)

nike_pct_aug = sum(nikeAug)/nrow(August20NBA.csv)
adi_pct_aug = sum(adidasAug) / nrow(August20NBA.csv)

nikeSept    <- grepl("nike", September20NBA.csv$text, ignore.case=TRUE)
adidasSept <- grepl("adidas", September20NBA.csv$text, ignore.case=TRUE)

nike_pct_sep = sum(nikeSept)/nrow(September20NBA.csv)
adi_pct_sep = sum(adidasSept) / nrow(September20NBA.csv)

nikeOct    <- grepl("nike", October20NBA.csv$text, ignore.case=TRUE)
adidasOct <- grepl("adidas", October20NBA.csv$text, ignore.case=TRUE)

nike_pct_oct = sum(nikeOct)/nrow(October20NBA.csv)
adi_pct_oct = sum(adidasOct) / nrow(October20NBA.csv)

month_vector <- as.Date(c("2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01" , "2020-10-01"))
nike_vector <- c(nike_pct_may, nike_pct_jun, nike_pct_jul, nike_pct_aug, nike_pct_sep, nike_pct_oct)
adidas_vector <- c(adi_pct_may, adi_pct_jun, adi_pct_jul, adi_pct_aug, adi_pct_sep, adi_pct_oct)

voice_data <- data.frame(month_vector, nike_vector, adidas_vector, stringsAsFactors=FALSE)


ggplot(data=voice_data, aes(x=month_vector)) +
geom_line(aes(y=nike_vector, colour = 'Nike'), colour='orange') +
geom_line(aes(y=adidas_vector, colour = 'Adidas'), colour='blue') + 
ggtitle("Share of voice Nike vs Adidas") +
xlab("Month") + ylab("Mentions / total tweets")

mean(nike_vector)*
mean(adidas_vector)
mean(c(nrow(May20NBA.csv), nrow(June20NBA.csv), nrow(July20NBA.csv), nrow(August20NBA.csv), nrow(September20NBA.csv), nrow(October20NBA.csv)))

