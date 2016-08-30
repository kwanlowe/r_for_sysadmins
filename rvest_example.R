library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)

mywords <- c("Red", "Hat", "Enterprise", "Linux")
bugzilla <- html_session("https://bugzilla.redhat.com/buglist.cgi?bug_id=103401%2C1341531%2C1243784%2C547546%2C1353245%2C1277197%2C1215968%2C1054997%2C1033334%2C1020622%2C838070%2C702271%2C557939%2C1364485%2C1333983%2C1330711%2C1325394%2C1311749%2C1310860%2C1305965%2C1294506%2C1286492%2C1281442%2C1277922%2C1276753%2C1276310%2C1265179%2C1257511%2C1219541%2C1208608%2C1196539%2C1186757%2C1183103%2C1179045%2C1176553%2C1176214%2C1166319%2C1125174%2C1097930%2C1093803%2C1063914%2C1060287%2C1025359%2C995415%2C954261%2C950611%2C921135%2C887931%2C822148%2C665820%2C616415%2C190862")
#bugzilla_data <- rvest::html_table(bugzilla)
bugzillaCorpus <- html_nodes(bugzilla, ".bz_short_desc_column") %>% html_text()
bugzillaCorpus <- Corpus(VectorSource(bugzillaCorpus))
bugzillaCorpus <- tm_map(bugzillaCorpus, PlainTextDocument)

bugzillaCorpus <- tm_map(bugzillaCorpus, removePunctuation)
bugzillaCorpus <- tm_map(bugzillaCorpus, removeWords, stopwords('english') )
bugzillaCorpus <- tm_map(bugzillaCorpus, removeWords, mywords )

# bugzillaCorpus <- tm_map(bugzillaCorpus, stemDocument)
wordcloud(bugzillaCorpus, max.words = 100, random.order = FALSE)