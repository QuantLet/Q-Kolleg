library(RMySQL)
library(tm)
library(cluster)

## Get the data in place:
  con = dbConnect(MySQL(), dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "******",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)

  dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
  
  ## Also get the project code in the right place:
    is.letter = function(x){grepl("[[:alpha:]]", x)}
    is.number = function(x){grepl("[[:digit:]]", x)}
  
    get_pcode <- function(projectcode){
      p.code = substr(projectcode, 1, 3)   
      letters = which(is.letter(substr(p.code,3,3)) == TRUE)
      p.code[letters] = substr(p.code[letters],1,2)
      return(p.code)
    }
    pcode <- get_pcode(dbtt$projectcode)
    rough_pcode <- as.factor(substr(pcode, 1, 1))
    
## Function for later manipulations:
  # Create Document-Term-Matrix:
    sparse_dtm = function(corpus, ...){
        DocumentTermMatrix(corpus, control = list(...))
    }
 
  # Remove sparse terms:
    small_dtm = function(corpus, threshold = 0.99, ...){
      removeSparseTerms(sparse_dtm(corpus, ...), sparse = threshold)
    }

  # Perform log(1 + x):
    log_x = function(termdoc){log(1 + as.matrix(termdoc))}
    
  # Inspect the terms present in the Document-Term-Document 
    getfreq = function(dtm){
      freqs = colSums(as.matrix(dtm))
      return(freqs[order(-freqs)])
    }
    
  # Dimensionality reducer:
    reduced_dim_pca = function(pca, thresh = 0.66){
      keep = sum((cumsum(pca$sdev)/length(pca$sdev)) < thresh)
      return(pca$x[,1:keep])
    }
  
## Start text mining and plotting:
  # Get different types of Doc-Term-Matrix of the lemmatized corpus:
    # Note that for the non-binary weighted matrices, a log transformation is applied:
    TDM_lemma             = log_x(small_dtm(Corpus(VectorSource(dbtt$lemma))))
    TDM_lemma_bin         = small_dtm(Corpus(VectorSource(dbtt$lemma)), 
                                       weighting = weightBin)
    TDM_lemma_bin_capped  = small_dtm(Corpus(VectorSource(dbtt$lemma)), threshold = 0.99,
                                       weighting = weightBin, 
                                       bounds = list(global = c(1, 175)))
    TDM_lemma_capped      = log_x(small_dtm(Corpus(VectorSource(dbtt$lemma)), threshold = 0.99,
                                       bounds = list(global = c(1,75))))
    
    TDM = list(TDM_lemma, TDM_lemma_bin, TDM_lemma_bin_capped, TDM_lemma_capped)
  
    # Inspect the terms left in the Doc-Term-Matrix and their frequencies: 
      freqs = lapply(TDM, getfreq)

## CLUSTERING  ##
    # Compute distance between document vectors:
    d = lapply(TDM, function(x){dist(as.matrix(x))})
      
  ## Hierarchical clustering using Ward's method.
     group = lapply(d, hclust, method = "ward.D")
     # table predictions vs. project codes (Hierarchical clustering, Ward's):
       ward_fit = lapply(group, function(x){ table(cutree(x, 4), rough_pcode)})

  ## K-means
     kfit = lapply(d, function(x){kmeans(x, 4, nstart = 100)$cluster})
     # table predictions vs. project codes (kmeans)
     kmeans_fit = lapply(kfit, table, rough_pcode)
        

## No need for PCA while doing kmeans
## Idea: Project on first 3 principal component in 3D
## Idea: Multidimensional scaling for projection, instead of PCA
## TFIDF
## Background: word2vec