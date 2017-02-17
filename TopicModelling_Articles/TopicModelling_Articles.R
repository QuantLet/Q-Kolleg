#####################################################################

### 0. Load data and packages
### 1. Run the LDA
### 2. Get the major terms in every topic to .txt
### 3. Calculate posterior for CRC papers
### 4. Calculate posterior for JEL descriptions

#####################################################################


### 0. Load data and packages:

  library(topicmodels)

  # Traindata (Derivation is omitted, due to copyright issues)
  load(file = "TopicModelling_Articles/AEA_TF.Rdata")  # Traindata
  load(file = "TopicModelling_Articles/CRC_TF.Rdata")  # Testdata
  load(file = "TopicModelling_Articles/JEL_TF.Rdata")  # JEL.TF
  
  
### 1. Run the LDA
  
  k = 20
  # Running LDA for k topics  
  lda.output = LDA(Traindata, k = k, method = "Gibbs") 
  save(lda.output, file = "TopicModelling_Analysis/lda_output.Rdata") 

    
### 2. Get the major terms in every topic to .txt
  
  lda.terms  = as.matrix(terms(lda.output, 10)) 
  write.csv(lda.terms, file = "TopicModelling_Analysis/lda_terms.txt")
  
  
### 3. Calculate posterior for CRC 649 papers (AKA Traindata)
  
  CRC.posterior = posterior(lda.output, Testdata) 
  save(CRC.posterior, file = "TopicModelling_Analysis/CRC_posterior.Rdata")
  
  
### 4. Calculate posterior for JEL descriptions:
  
  JEL.posterior = posterior(lda.output, JEL.TF)
  save(JEL.posterior, file = "TopicModelling_Analysis/JEL_posterior.Rdata")
  