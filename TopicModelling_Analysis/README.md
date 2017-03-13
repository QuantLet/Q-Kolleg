
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TopicModelling_Analysis** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : TopicModelling_Analysis

Published in : <‘> Not published <‘>

Description : 'Inspects the LDA output and posterior output and attempts to investigate structures
and insights'

Keywords : 'Textmining, TFIDF matrix, Latent Dirichlet Allocation, (Unsupervised) Machine Learning,
Topic Modelling'

See also : 'ScrapeAbstracts, ScrapeArticles, ScrapeJEL, PDF_2_TXT, StemTFIDF_Abstracts,
StemTFIDF_Articles, StemJEL, Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Input : 'lda_output.Rdata, CRC_posterior.Rdata, JEL_posterior.Rdata and lda_terms.txt to
TopicModelling_Analysis'

```


### R Code:
```r
library(topicmodels)

######################################################################

### 0 a). Load data
### 0 b). Define functions needed for this script
### 1.    Calculate topic distribution for Traindata
### 2.    Calculate topic distribution for Testdata based on LDA output
### 3.    Calculate topic distribution for JEL descriptions

######################################################################

### 0 a). Load data

  load(file = "TopicModelling_Analysis/lda_output.Rdata")     # lda.output
  load(file = "TopicModelling_Analysis/CRC_posterior.Rdata")  # CRC.posterior
  load(file = "TopicModelling_Analysis/JEL_posterior.Rdata")  # JEL.posterior
  
  lda.terms = read.csv2(file = "TopicModelling_Analysis/lda_terms.txt", 
                        header = T, stringsAsFactors = F, sep = ",")
  lda.terms = lda.terms[,-1]
  

### 0 b). Define functions needed for this script
  
  # Find the assigned topic for a posterior lda object
    abs.topic.assign = function(someposter){
      apply(someposter$topics, 1, which.max)
    }
  
  # Find the summed relative density of topic assignment on posterior lda object
    rel.topic.assign = function(someposter){
      colSums(someposter$topics)/sum(someposter$topics)
    }
    
  # The column names for the topic distribution tables
    table_colnames = apply(lda.terms, 2, 
                           function(x){paste0(x, collapse = ", ")}) 
    
  # Get the index of the second highest value in a row:
    secondbest = function(poster.topic){
      apply(poster.topic, 1, 
            function(x){which(order(x, decreasing = F) == 2)})
    }
    
  # Get the top 5 topics:
    get.top5 = function(topictable, absolute = F, N = 5){
      correction = 1
      if(absolute){
        correction = sum(topictable)
      }
      sort(topictable, decreasing =T)[1:N]/correction
    }
    
    
### 1.    Calculate topic distribution for Traindata
    
  # Number of documents assigned to every topic, decreasing order: 
    lda.topics               = topics(lda.output)
    lda.topics.table         = table(lda.topics)
    names(lda.topics.table)  = table_colnames
    AEA_abs_topics = get.top5(lda.topics.table, absolute = T)

  # The total loadings per topic for Traindata:
    topic_prop        = colSums(lda.output@gamma)/sum(lda.output@gamma)
    names(topic_prop) = table_colnames
    AEA_rel_topics    = get.top5(topic_prop)
    
    
### 2. Calculate topic distribution for Testdata (CRC papers) 
###     based on LDA output

  # Absolute topic assignment (incl. appropriate column names)
    CRC.topics             = abs.topic.assign(CRC.posterior)
    CRC.topic.table        = table(CRC.topics)
    # Some topics are not assigned to any doc in the CRC testdata
      pres.top             = grep("[0-9]", colnames(lda.terms)) %in%
                                names(CRC.topic.table)
    names(CRC.topic.table) = table_colnames[pres.top]
    CRC_abs_topics         = get.top5(CRC.topic.table, absolute = T)
    
  
  # Relative assignment (incl. appropriate column names)
    CRC.topics.dens        = rel.topic.assign(CRC.posterior)
    names(CRC.topics.dens) = table_colnames      
    CRC_rel_topics         = get.top5(CRC.topics.dens)
    
### 3. Calculate topic distribution for JEL descriptions based on LDA output:
  
  JEL.topics = abs.topic.assign(JEL.posterior)
  # Oh oh, not very promising.. All descriptions are assigned to topic 20.
  
    
    
    
```
