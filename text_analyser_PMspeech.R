install.packages('tm')
library(tm)

setwd("/Users/rjirwanshah/Desktop/Personal/Data Science Primer Course/R random scripts/Text Analysis")
speech <- read.csv ("Budget 2016 revision- Full text of PM's speech - Sheet1.csv", stringsAsFactors=FALSE)
str(speech)

# we treat all the speech as one text
# 'collapse' = paste together elements of a vector, rather than pasting vectors together
speech_combined <- paste(speech$Text, collapse=" ")

# set up the source and create a corpus
speech_source <- VectorSource(speech_combined)
corpus <- Corpus(speech_source)

summary(corpus)  

# cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# OPTIONAL : exclude all StopWords. for Malay stop words > http://datago.my/blog/malay-stop-words/
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# note : "tm" package supported languages are danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish, and swedish

# OPTIONAL : remove unnecessary words - custom words eg : Malay (if your data set in Malay)
# corpus <- tm_map(corpus, removeWords, c("dan", "yang", "ini"))

# create the document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

# take the column sums of this matrix, which will give us a named vector.
frequency <- colSums(dtm2)

# total number of terms
length(frequency)
#create sort order (asc)
ordr <- order(frequency,decreasing=TRUE)
#inspect most frequently occurring terms
frequency[head(ordr)]
#inspect least frequently occurring terms
frequency[tail(ordr)]
# list of terms that occur at least 50 times
findFreqTerms(dtm,lowfreq=50)

install.packages('ggplot2')
library(ggplot2)

words_frequency=data.frame(term=names(frequency),occurrences=frequency)
p <- ggplot(subset(words_frequency, frequency>10), aes(term, occurrences))
p <- p + geom_bar(stat='identity')
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

###########
# WORLCLOUD
###########

install.packages('wordcloud')
library('wordcloud')

words <- names(frequency)
wordcloud(words[1:400], frequency[1:400],colors=brewer.pal(6,'Dark2'))


