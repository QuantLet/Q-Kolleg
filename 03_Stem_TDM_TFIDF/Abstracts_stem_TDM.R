library(tm)

load("03_Stem_TDM_TFIDF/Abstracts_raw.Rdata")   # Opens as abstr_info

########################################################################

### 0. Functions for this script
### 1. Stemming & remove interpunction, caps, numbers, whitespace
### 2. Get different versions of the TDM (capped, log, TF/IDF)
### 3. Save data in .Rdata for further analysis

########################################################################


#### 0. Functions for this script                                  #####

  # functions that help to detect letters character string
  keep.letter = function(x){gsub("[^[:alpha:]]", "", x)}
  
  # create Document-Term-Matrix:
  sparse_dtm = function(corpus, ...){
    DocumentTermMatrix(corpus, control = list(...))
  }
  
  # remove sparse terms:
  small_dtm = function(corpus, threshold = 0.99, ...){
    removeSparseTerms(sparse_dtm(corpus, ...), sparse = threshold)
  }
  
  # perform log(1 + x):
  log_x = function(termdoc){log(1 + as.matrix(termdoc))}
  
 
#### 1. Stemming & remove interpunction, caps, numbers, whitespace #####
  
  abstr = Corpus(VectorSource(abstr_info$abstracts))
  abstr = tm_map(abstr, content_transformer(tolower))
  abstr = tm_map(abstr, removeWords, 
                c(stopwords("en"), "general", "introductory", "unclassified"))
  abstr = tm_map(abstr, removePunctuation)
  abstr = tm_map(abstr, removeNumbers)
  abstr = tm_map(abstr, stemDocument)
  
  abstr_info$abstracts = sapply(abstr, paste0)
   
  
#### 2. Get different versions of the TDM (capped, log, TF/IDF)   #####
  
  TDM_log               = log_x(small_dtm(abstr))
  TDM_binary            = small_dtm(abstr, weighting = weightBin)
  TDM_binary_capped     = small_dtm(abstr, weighting = weightBin, 
                                    bounds = list(global = c(1, 175)))
  TDM_log_capped        = log_x(small_dtm(abstr,
                                          bounds = list(global = c(1,75))))
  TFIDF                 = small_dtm(abstr, weighting = weightTfIdf)
  
  TDM = list(TDM_log, TDM_binary, TDM_binary_capped, TDM_log_capped, TFIDF)

  
#### 3. Save data in .Rdata for further analysis                  #####
  
  save(TDM, file = "04_Analysis/Abstract_TDMs.Rdata")
  save(abstr_info, file = "04_Analysis/Abstract_infotable.Rdata")
  
