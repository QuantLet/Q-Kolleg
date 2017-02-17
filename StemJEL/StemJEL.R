library(tm)  
load("StemJEL/JEL_raw.Rdata") # opens as JEL


########################################################################

### 0. Functions for this script
### 1. Stemming & remove interpunction, caps, numbers, whitespace
### 2. TFIDF of the JEL descriptions
### 3. Save data in .Rdata for further analysis

########################################################################

### 0. Functions for this script
  # create Document-Term-Matrix:
  sparse_dtm = function(corpus, ...){
    DocumentTermMatrix(corpus, control = list(...))
  }

  # remove sparse terms:
  small_dtm = function(corpus, threshold = 0.99, ...){
    removeSparseTerms(sparse_dtm(corpus, ...), sparse = threshold)
  }


### 1. Stemming & remove interpunction, caps, numbers, whitespace
  corpus = Corpus(VectorSource(JEL$descr))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, 
             c(stopwords("en"), "general", "introductory", "unclassified"))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stemDocument)

  
### 2. TFIDF for the JEL descriptions:
  JEL.TF = small_dtm(corpus, weighting = weightTf)


### 3. Save data in .Rdata for further analysis
  JEL$descr = sapply(corpus, paste0)
save(JEL, file = "Textmining_Abstracts/JEL_descr.Rdata")
save(JEL.TFIDF, file = "TopicModelling_Articles/JEL_TF.Rdata")
