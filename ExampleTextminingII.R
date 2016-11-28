rm(list = ls())
graphics.off()

setwd("C:/Users/Johannes/Dropbox/Digital_Economics")

## Load required packages:
  libraries = c("topicmodels", "cluster", "devtools", "readr", "cldr", "psych",
                "irlba", "tm", "RMySQL", "SnowballC", "slam", "reshape2", 
                "ggplot2", "httr", "magrittr", "rvest", "stringr", "XML", "stringi")
  lapply(libraries, function(x){
    if(!(x %in% installed.packages())){install.packages(x)}})
  lapply(libraries, require, quietly = TRUE, character.only = TRUE)
  
  # cldr-package is not available on CRAN:
  if(!("cldr" %in% installed.packages())){
    devtools::install_version("cldr", version = "1.1.0")
  }
  
#####################################################################
####    Text-mining from local source 
#####################################################################
## Get data:
  drv = dbDriver("MySQL") 
  con = dbConnect(drv, dbname = "Q-Kolleg", 
                  user = "stoiberj2.hub", password = "Im9A#5r?",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)

  # extract the table abstracts
    allabstracts = dbGetQuery(con, "SELECT * FROM abstracts")
  
  dbDisconnect(con)

## Detect and split languages: (cldr-package)
  detlang = detectLanguage(allabstracts$abstracts)
  
  # want only to proceed with english abstracts, 
    en.abstracts = split(allabstracts, detlang$detectedLanguage)$ENGLISH

## Data preparation: (tm-package) 
  # Turn the abstract text into a "Corpus"
    corpus = Corpus(VectorSource(en.abstracts$abstracts))

  # Remove punctuation, numbers, stopwords, special characters, white space:
    cleanit <- function(corpus, lang = "en"){
      corpus = tm_map(corpus, removePunctuation)
      corpus = tm_map(corpus, removeNumbers)
      corpus = tm_map(corpus, removeWords, stopwords(kind = paste(lang)))
      corpus = tm_map(corpus, content_transformer(function(d){
        gsub("[^a-z]", " ", d)}))
      corpus = tm_map(corpus, content_transformer(function(d){
        stri_trans_general(d, "latin-ascii")}))
      corpus = tm_map(corpus, stripWhitespace)
      return(corpus)
      }
    corpus = cleanit(corpus)
  
  # Stemming:
    stemit <- function(x){
      stemDocument(PlainTextDocument(x))}
    corpus = tm_map(corpus, stemit)
    
  # Assign paper number as name:
    names(corpus) = en.abstracts$number


## Term document matrix:
  tdm = TermDocumentMatrix(corpus,control = list(minWordLength = 3))
  dtm = DocumentTermMatrix(corpus, control= list(minWordLength = 3))

  # Remove very sparse terms
    tdm.cmn = removeSparseTerms(tdm,0.9)
    dim(tdm); dim(tdm.cmn)
    tdm.cmn
    removeSparseTerms(tdm,0.6)$dimnames$Terms

#################################################
# cluster analysis
#################################################
# how many clusters may we have? 
en.abstracts$jel[1:20]
# extract first 1/3 characters from projectcode
p.code = substring(en.abstracts$projectcode, 1,3)

is.letter = function(x) grepl("[[:alpha:]]", x)
is.number = function(x) grepl("[[:digit:]]", x)

# check the 3rd character. if it is a letter, than delete. else not. 
p.code[which(is.letter(substr(p.code,3,3))==TRUE)] = substr(p.code[which(is.letter(substr(p.code,3,3))==TRUE)],1,2)
p.code  = as.factor(p.code)

# a further cluster approach. take only the letters from the project groups.
p2.code = as.factor(substr(as.character(p.code),1,1))

# number of clusters:
cl1 = length(sort(unique(p.code)))
cl2 = length(sort(unique(p2.code)))


# extract dtm from and save as matrix object. Use sparse one
# apply log transformation and compare without transformation
lm = log(1 + as.matrix(removeSparseTerms(dtm,0.9)))
m  = as.matrix(removeSparseTerms(dtm,0.9))
#compute distance between document vectors
d  = dist(m)
ld = dist(lm)


# PCA
# PCA
pca  = prcomp(m, center=TRUE, scale=TRUE)
lpca = prcomp(lm, center=TRUE, scale=TRUE)

# screplot
plot(pca$sdev^2, ylim=c(0.5,3), ylab="Eigenvalues of components", xlab="component number")
points(lpca$sdev^2, col= "red")
abline(h=1)


# density plot for those with higher eigenvalues than 1
ev = length(which(pca$sdev > 1))
plot(density(prcomp(m, center =  TRUE, scale = TRUE)$x[,1:ev]), main="")
lines(density((prcomp(lm, center=TRUE, scale=TRUE)$x[,1:ev])), col="red")




# 1st two pc and 5 project groups i ncolors
plot(pca$x[,c(1,2)],  col=p2.code, main = "PC1 vs PC2 on TDM")
plot(pca$x[,c(3,4)],  col=p2.code, main = "PC3 vs PC4 on TDM")
plot(lpca$x[,c(1,2)], col=p2.code, main = "PC1 vs PC2 on transformed TDM")
plot(lpca$x[,c(3,4)], col=p2.code, main = "PC3 vs PC4 on transformed TDM")



# go furhter with transformed matrix
#run hierarchical clustering using Ward's method.
groups <- hclust(ld,method="ward.D")

#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups, cl2)
table(cutree(groups, 4), p2.code)

# plot PC1 vs PC2, color as given by the 5 found clusters
plot(lpca$x[,1:2], col=cutree(groups, 4), main="4 Cluster")


### k-means
#k means algorithm, 100 starting configurations
kfit <- kmeans(ld, 4, nstart=100)
#plot - need library cluster
clusplot(lm, kfit$cluster, color=T, shade=T,  lines=0)
table(kfit$cluster, p2.code)

plot(lpca$x[,1:2], col= kmeans(ld, 5, nstart=100)$cluster, main="5 cluster")


# cake diagram from table
paper.id = names(which(kfit$cluster==1))
en.abstracts$title[en.abstracts$number %in% paper.id]

