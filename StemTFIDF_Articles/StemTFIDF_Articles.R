library(tm)

## NOTE: The .txt-files for the AEA articles are not available public.
##  The .Rdata-file of the TFIDF-matrix of the AEA articles can be found
##  in the "TopicModelling_Article" Quantlet

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


### 1. Read CRC .txt files into a large list                       #####
# Read strings from .txt into R-session:
  mytxts      = list.files("StemTFIDF_Articles", 
                      pattern = "*.txt", full.names = T)
  CRC.inds    = grepl("([1-9])", substr(mytxts, 20, 23))
  mytxts      = mytxts[CRC.inds]
  articlelist = lapply(mytxts, function(textdoc){
      paste0(scan(textdoc, what = character(), fileEncoding = "latin1"),
      collapse = " ")})


### 2. Stemming, remove interpunction, stopwords, etc.             #####
  corpus = Corpus(VectorSource(articlelist))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeWords, c(stopwords("english"),
                          "/f", "use", "can", "one", "will"))
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, stemDocument, "english")


### 3. Create a TF/IDF matrix & save result in .Rdata format       #####
  Testdata = small_dtm(corpus, threshold = 0.995, weighting = weightTf)
  save(Testdata, file = "TopicModelling_Articles/CRC_TF.Rdata")
  
  ## The Traindata extraction has been omitted due to copyright. Output
  ##  may be found in TopicModelling_Articles/AEA_TF.Rdata
