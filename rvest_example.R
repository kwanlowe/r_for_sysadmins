library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)

mywords <- c("Red", "Hat", "Enterprise", "Linux")
bugzilla <- html_session("https://bugzilla.redhat.com/buglist.cgi?quicksearch=sssd")

bugzillaCorpus <- html_nodes(bugzilla, ".bz_short_desc_column") %>% html_text()
bugzillaCorpus <- Corpus(VectorSource(bugzillaCorpus))
bugzillaCorpus <- tm_map(bugzillaCorpus, PlainTextDocument)

bugzillaCorpus <- tm_map(bugzillaCorpus, removePunctuation)
bugzillaCorpus <- tm_map(bugzillaCorpus, removeWords, stopwords('english') )
bugzillaCorpus <- tm_map(bugzillaCorpus, removeWords, mywords )

wordcloud(bugzillaCorpus, max.words = 80, random.order = FALSE, scale=c(4,.2))