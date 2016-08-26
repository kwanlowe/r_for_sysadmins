library(tm)
library(SnowballC)
library(wordcloud)

mywords <- c("kwan", "lowe", "management" )
remedy <- read.csv("remedy.csv", stringsAsFactors = FALSE)

remedyCorpus <- Corpus(VectorSource(remedy$Text))
remedyCorpus <- tm_map(remedyCorpus, PlainTextDocument)
# remedyCorpus <- tm_map(remedyCorpus, tolower)

remedyCorpus <- tm_map(remedyCorpus, removePunctuation)
# remedyCorpus <- tm_map(remedyCorpus, removeWords, c(stopwords('english'), mywords) )
remedyCorpus <- tm_map(remedyCorpus, removeWords, stopwords('english') )
remedyCorpus <- tm_map(remedyCorpus, removeWords, mywords )

# remedyCorpus <- tm_map(remedyCorpus, stemDocument)
wordcloud(remedyCorpus, max.words = 60, random.order = FALSE)
