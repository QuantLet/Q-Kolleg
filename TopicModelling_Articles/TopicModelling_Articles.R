#####################################################################

### 0. Load data and packages
### 1. Run the LDA
### 2. Provide some insight in LDA output
### 3. Calculate topic distribution for Testdata based on LDA output

#####################################################################


### 0. Load data and packages:

  library(topicmodels)

  # Traindata (Derivation is omitted, due to copyright issues)
  load(file = "TopicModelling_Articles/AEA_TF.Rdata")  # Traindata
  load(file = "TopicModelling_Articles/CRC_TF.Rdata")  # Testdata

  
### 1. Run the LDA
  
  k = 20
  # Running LDA for k topics  
  lda.output = LDA(Traindata, k = k, method = "Gibbs") 
  save(lda.output, file = "TopicModelling_Articles/lda_output.Rdata") 
  
### 2. Provide some insight in LDA output:

  # Overview of allocation of topics and terms per topic 
  lda.topics = as.matrix(topics(lda.output)) 
  lda.terms  = as.matrix(terms(lda.output, 10)) 
  
  write.csv(lda.terms, file = "TopicModelling_Articles/lda_terms.txt")
  lda.terms = read.csv2("TopicModelling_Articles/lda_terms.txt", 
                         header = T, stringsAsFactors = F, sep = ",")
  lda.terms = lda.terms[,-1]
  
  # Number of documents assigned to every topic, decreasing order: 
  table(lda.topics)[order(table(lda.topics), decreasing = T)] 

  # The total loadings per topic for Traindata:
  topic_proportions = colSums(lda.output@gamma)/sum(lda.output@gamma)
  
### 3. Calculate topic distribution for Testdata based on LDA output
  
  # Find the topics that the descriptions will be assigned to by LDA: 
  CRC.posterior = posterior(lda.output, Testdata) 
  save(CRC.posterior, file = "TopicModelling_Articles/CRC_posterior.Rdata")
  
  CRC.topics = apply(CRC.posterior$topics, 1, which.max) 


### 4. Calculate topic distribution for JEL descriptions based on LDA output:
  load("TopicModelling_Articles/JEL_TF.Rdata")
  
  JEL.posterior = posterior(lda.output, corpus)
  JEL.topics = apply(JEL.posterior$topic, 1, which.max)
  
  Corpus(VectorSource(JEL[,2]))
  
  JEL
  