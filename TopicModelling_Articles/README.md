
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TopicModelling_Articles** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : TopicModelling_Articles

Published in : <‘> Not published <‘>

Description : 'Trains a Latent Dirichlet Allocation (LDA) model on AEA papers and calculate the
posterior of the latent distributions for the (unseen) CRC 649 papers'

Keywords : 'Textmining, TFIDF matrix, Latent Dirichlet Allocation, (Unsupervised) Machine Learning,
Topic Modelling'

See also : 'ScrapeAbstracts, ScrapeArticles, ScrapeJEL, PDF_2_TXT, StemTFIDF_Abstracts,
StemTFIDF_Articles, StemJEL, Textmining_Abstracts, TopicModelling_Analysis'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Input : AEA_TF.Rdata, CRC_TF.Rdata, JEL_TF.Rdata,

Output : 'lda_output.Rdata, CRC_posterior.Rdata, JEL_posterior.Rdata and lda_terms.txt to
TopicModelling_Analysis'

```


### R Code:
```r
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
  
```
