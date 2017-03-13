
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **StemJEL** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : StemJEL

Published in : <‘> Not published <‘>

Description : 'Stems and cleans the JEL-codes and descriptions. Afterwards the stemmed descriptions
are saved in Textmining_Abstracts and TopicModelling_Articles for further analysis'

Keywords : 'Textmining, stemming, remove stopwords, data collection, Journal of Economic
Literature, JEL'

See also : 'ScrapeAbstracts, ScrapeArticles, ScrapeJEL, PDF_2_TXT, StemTFIDF_Abstracts,
StemTFIDF_Articles, StemJEL, Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Input : JEL_raw.Rdata

Output : 'JEL_descr.Rdata is saved to Textmining_Abstracts. JEL_TF.Rdata is saved to
TopicModelling_Articles'

```


### R Code:
```r
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

```
