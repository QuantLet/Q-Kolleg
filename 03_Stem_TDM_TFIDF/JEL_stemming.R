library(tm)  

load("03_Stem_TDM_TFIDF/JEL_descr_raw.Rdata") # opens as JEL


#### Stem the texts, remove stopwords, etc
corpus <- Corpus(VectorSource(JEL$descr))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, 
                 c(stopwords("en"), "general", "introductory", "unclassified"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stemDocument)

JEL$descr <- sapply(corpus, paste0)


#### Save the stemmed JEL descriptions:
save(JEL, file = "04_Analysis/JEL_descr.Rdata")
