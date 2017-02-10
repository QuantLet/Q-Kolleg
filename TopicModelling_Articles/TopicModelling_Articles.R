library(topicmodels)
library(tm)

load(file = "TopicModelling_Articles/Abstract_infotable.Rdata")  # loads as abstr_info
load(file = "TopicModelling_Articles/JEL_descr.Rdata")           # loads as JEL
load(file = "TopicModelling_Articles/Articles_TFIDF.Rdata")      # loads as TF

########################################################################

### 0. Functions for this script
### 1. Determine the number of topics to extracts for LDA
### 2. Run & analyse the output
### 3. Save the LDA output

########################################################################


#### 0. Functions for this script                                  #####

keep.letter = function(x){gsub("[^[:alpha:]]", "", x)}


#### 1. Determine the number of topics to extract for LDA
  ## Note that, as more articles become availabe, this restriction 
  ## be removed. Currently, the LDA cannot identify some topics,
  ## since they are simply not present in the training set.
  abstr_info = abstr_info[-c(length(abstr_info))]

  JELcats = sapply(abstr_info$jel, keep.letter)
  JELcats = sapply(tolower(JELcats), strsplit, split = "", fixed = T)
  tab = table(unlist(JELcats))
  k = length(tab[tab/sum(tab) > 0.01])
  
  relevant.JELs = JEL$descr[which(tab/sum(tab) > 0.01)]
  
  
#### 2. Run & analyse the output                                   #####
  
  # Running LDA for k topics 
  lda.output = LDA(TF, k = k, method = "Gibbs")
  
  # Overview of allocation of topics and terms per topic
  lda.topics = as.matrix(topics(lda.output))
  lda.terms = as.matrix(terms(lda.output, 10))
  
  # Number of documents assigned to every topic, decreasing order:
  table(lda.topics)[order(table(lda.topics), decreasing = T)]
  
  # Find the topics that the descriptions will be assigned to by LDA:
  JEL.posterior = posterior(lda.output, 
                          DocumentTermMatrix(Corpus(VectorSource(JEL$descr))))
  
  JEL.posterior.relevant = posterior(lda.output, 
                      DocumentTermMatrix(Corpus(VectorSource(relevant.JELs))))
  
  JEL.topics = apply(JEL.posterior$topics, 1, which.max)
  JEL.topics.relevant = apply(JEL.posterior.relevant$topics, 1, which.max)
  
  
#### 3. Save the LDA output                                        #####
  
  save(lda.output, file = "TopicModelling_Articles/lda_output.Rdata")
