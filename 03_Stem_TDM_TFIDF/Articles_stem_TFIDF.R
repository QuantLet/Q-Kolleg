library(tm)

########################################################################

### 0. Functions for this script
### 1. Read all .txt files into a large list 
### 2. Stemming, remove interpunction, stopwords, etc.
### 3. Create a TF/IDF matrix & save result in .Rdata format

########################################################################


#### 0. Functions for this script                                  #####

  # Create Document-Term-Matrix:
  sparse_dtm = function(corpus, ...){
    DocumentTermMatrix(corpus, control = list(...))
  }

  # Remove sparse terms:
  small_dtm = function(corpus, threshold = 0.99, ...){
    removeSparseTerms(sparse_dtm(corpus, ...), sparse = threshold)
  }


### 1. Read all .txt files into a large list                       #####
  # Read strings from .txt into R-session:
  mytxts = list.files("03_Stem_TDM_TFIDF/txt", pattern = "*.txt",
                      full.names = T)
  abstractlist = lapply(mytxts, function(textdoc){
    paste0(scan(textdoc, what = character(), fileEncoding = "latin1"),  
           collapse = " ")})

  
### 2. Stemming, remove interpunction, stopwords, etc.             #####
  corpus = Corpus(VectorSource(abstractlist))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers) 
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeWords, c(stopwords("english"), 
                                         "/f", "use", "can", "one", "will"))
  corpus = tm_map(corpus, PlainTextDocument)  
  corpus = tm_map(corpus, stemDocument, "english")

 
### 3. Create a TF/IDF matrix & save result in .Rdata format       #####
  TF = small_dtm(corpus, threshold = 0.995, weighting = weightTf)
  save(TF, file = "04_Analysis/Articles_TFIDF.Rdata")
