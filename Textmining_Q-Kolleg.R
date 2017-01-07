### Textmining procedure ###

source("helperfunctions_Q-Kolleg.R")
library(tm)
library(RMySQL)

## Get the data in place:
  con = dbConnect(MySQL(), dbname = "Q-Kolleg", 
                  user = "stoiberj2.hub", password = "...",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)

  dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
  dbDisconnect(con)
## Also get the project code in the right place:
  pcode = get_pcode(dbtt$projectcode)
  rough_pcode = as.factor(substr(pcode, 1, 1))
    

## Start text mining and plotting:
  # Get different types of Doc-Term-Matrix of the lemmatized corpus:
    # Note that for the non-binary weighted matrices, a log transformation is applied:
  lemmatized <- Corpus(VectorSource(dbtt$lemma))
    TDM_lemma             = log_x(small_dtm(lemmatized))
    TDM_lemma_bin         = small_dtm(lemmatized, weighting = weightBin)
    TDM_lemma_bin_capped  = small_dtm(lemmatized, weighting = weightBin, 
                                       bounds = list(global = c(1, 175)))
    TDM_lemma_capped      = log_x(small_dtm(lemmatized,
                                       bounds = list(global = c(1,75))))
    TFIDF_lemma           = small_dtm(lemmatized, weighting = weightTfIdf)
    
    TDM = list(TDM_lemma, TDM_lemma_bin, TDM_lemma_bin_capped, 
               TDM_lemma_capped, TFIDF_lemma)
  
    # Inspect the terms left in the Doc-Term-Matrix and their frequencies: 
      freqs = lapply(TDM, getfreq)

## PCA ##
      # apply PCA to all matrices, add additional field to each entry
      # that indicate how much PC we need for a certain explanatory level
      pca = lapply(X = c(1:length(TDM)), FUN=function(X){pca.man(as.matrix(TDM[[X]]), tau = 0.3)})

      
## CLUSTERING  ##
     # Compute distance between document vectors:
     d = lapply(TDM, function(x){dist(as.matrix(x), method="manhattan")})
      
  ## Hierarchical clustering using Ward's method.
     group = lapply(d, hclust, method = "ward.D2")
     # table predictions vs. project codes (Hierarchical clustering, Ward's):
     ward_fit = lapply(group, function(x){ table(cutree(x, 4), rough_pcode)})
     
  ## K-means
     kfit = lapply(d, function(x){kmeans(x, 4, nstart = 100)$cluster})
     # table predictions vs. project codes (kmeans)
     kmeans_fit = lapply(kfit, table, rough_pcode)
     
## Presentation / For illustrational reasons ##
     # how many pc do we need to explain 30% of variation?
     unlist(sapply(pca, "[",6))
     
     # PCA plot for TF-IDF matrix and Screeplot
     plot(pca[[5]],type="l",main=paste0("Screeplot, 10% explained by first ",pca[[5]]$idx," PC"))
     plot(pca[[5]]$x[,c(1,2)], col =  rough_pcode, main="PC1 vs PC2 on TF-IDF")
     
     # Hierarchical Ward clustering
     plot(group[[5]], hang = -1, cex = .6, xlab="")
     rect.hclust(group[[5]], k= 4, border = 1:4)
     ward_fit[[5]]
     plot(pca[[5]]$x[,c(1,2)], col =  cutree(group[[5]],4), main="PC1 vs PC2 on TF-IDF")
     
     # K-means
     plot(pca[[5]]$x[,c(1,2)], col =  kfit[[5]], main="PC1 vs PC2 on TF-IDF")
     kmeans_fit[[5]]
     
     # Silhouette information 
     # hierarchical
     mean(silhouette(cutree(group[[5]],4),d[[5]])[,3])
     plot(silhouette(cutree(group[[5]],4),d[[5]])[,3], main="Silhoutte hierarchical clustering", col=cutree(group[[5]],4))
     
     # k-means
     mean(silhouette(kfit[[5]],d[[5]] )[,3])
     plot(silhouette(kfit[[5]],d[[5]] )[,3], main="Silhoutte k-means", col = kfit[[5]])
     