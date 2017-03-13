
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **StemTFIDF_Abstracts** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : StemTFIDF_Abstracts

Published in : <‘> Not published <‘>

Description : 'Stems and cleans the abstracts from CRC 649. Also produces TermDocumentMatrices of
different kinds, including capped TDMs, TF, TFIDF, log-transformed TFIDF etc.'

Keywords : Textmining, data collection, Journal of Economic Literature, JEL, remove stopwords.

See also : 'ScrapeAbstracts, ScrapeArticles, ScrapeJEL, PDF_2_TXT, StemTFIDF_Articles, StemJEL,
Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Output : 'Abstract_TDMs.Rdata is saved to to Textmining_Abstracts. Abstract_infotable.Rdata is
saved to Textmining_Abstracts and TopicModelling_Articles'

Input : Abstracts_raw.Rdata

```


### R Code:
```r
library(tm)

load("StemTFIDF_Abstracts/Abstracts_raw.Rdata")   # Opens as abstr_info

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
  
  save(TDM, file = "Textmining_Abstracts/Abstract_TDMs.Rdata")
  save(abstr_info, file = "Textmining_Abstracts/Abstract_infotable.Rdata")
  save(abstr_info, file = "TopicModelling_Articles/Abstract_infotable.Rdata")

```
