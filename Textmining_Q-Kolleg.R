### Textmining procedure ###

## Get the data in place:
  con = dbConnect(MySQL(), dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "O9rVnS%J",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)

  dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
  dbDisconnect(con)
## Also get the project code in the right place:
  pcode = get_pcode(dbtt$projectcode)
  rough_pcode <- as.factor(substr(pcode, 1, 1))
    

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
        

