rm(list = ls())
graphics.off()


# cldr-package is not available on CRAN:
if(!("cldr" %in% install.packages())){
  devtools::install_version("cldr", version = "1.1.0")
}

setwd("C:/Users/Johannes/Dropbox/Digital_Economics")
# Load required packages:
libraries = c("topicmodels","devtools","readr", "cldr","psych","irlba","tm", "RMySQL", "SnowballC","wordcloud","slam","reshape2" ,"ggplot2","httr","magrittr","rvest","stringr","XML")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)

#####################################################################
####    Text-mining from local source 
#####################################################################
## Get data:
drv = dbDriver("MySQL") 
con = dbConnect(drv, dbname = "Q-Kolleg", 
                user = "stoiberj2.hub", password = "",
                host = "neyman.wiwi.hu-berlin.de", port = 3306)
# extract the table abstracts
allabstracts = dbGetQuery(con, "SELECT * FROM abstracts")
# close db connection
dbDisconnect(con)

## Detect and split languages: (cldr-package)
detlang = detectLanguage(allabstracts$abstracts)
table(detlang$detectedLanguage)
## want only to proceed with english abstracts, 
en.abstracts = split(allabstracts, detlang$detectedLanguage)$ENGLISH

## Data preparation: (tm-package) 
# Turn the abstract text into a "Corpus"
corpus = Corpus(VectorSource(en.abstracts$abstracts))

# Remove punctuation, numbers, stopwords, special characters, white space:
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords(kind="en"))
# select those which should not be regarded as special characters: a-z
replaceSpecialChars = function(d) {gsub("[^a-z]", " ", d)} 
corpus = tm_map(corpus, replaceSpecialChars)
corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, PlainTextDocument)

# after preprocessing, the filenames had been deleted, assign them discussion papaer number
names(corpus) = en.abstracts$number

writeLines(as.character(corpus[[1]]))

# get term document matrix
meta(corpus[[1]])
tdm = TermDocumentMatrix(corpus,control = list(minWordLength = 3))
dtm = DocumentTermMatrix(corpus, control= list(minWordLength = 3))

inspect(dtm)
inspect(tdm)
# remove very sparse terms
tdm.cmn = removeSparseTerms(tdm,0.9)
dim(tdm); dim(tdm.cmn)
tdm.cmn
removeSparseTerms(tdm,0.6)$dimnames$Terms

#################################################
# conduct here#
#################################################
# how many clusters may we have? 
en.abstracts$jel[1:20]
# extract first 3 characters from projectcode
p.code = substring(en.abstracts$projectcode, 1,3)



is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)
# check the 3rd character. if it is a letter, than delete. else not. 
p.code[which(is.letter(substr(p.code,3,3))==TRUE)] = substr(p.code[which(is.letter(substr(p.code,3,3))==TRUE)],1,2)
p.code = as.factor(p.code)
p2.code = as.factor(substr(as.character(p.code),1,1))
# number of clusters:
cl = length(sort(unique(p.code)))


# extract dtm from and save as matrix object. Use sparse one
m = as.matrix(removeSparseTerms(dtm,0.9))
#compute distance between document vectors
d <- dist(m)

# PCA
pca = prcomp(m, center=TRUE, scale=TRUE)
summary(pca) 
plot(pca, type="l")
scree(m, factors=FALSE)



#run hierarchical clustering using Ward's method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups,5)
hier.clust = cutree(groups, 5)
clustm = table(hier.clust, p.code)
str(p.code)
image(t(clustm[nrow(clustm):1,] ), axes=FALSE, zlim=c(-4,4), col=c(0,0,0,4))
plot(pca$x[,1:2], col=cutree(groups, 38), main="38 Cluster")
plot(pca$x[,1:2], col=cutree(groups, 5), main="5 Cluster")



### k-means
#k means algorithm, 2 clusters, 100 starting configurations
kfit <- kmeans(d, 38, nstart=100)
#plot - need library cluster
clusplot(m, kfit$cluster, color=T, shade=T, labels=2, lines=0)
table(kfit$cluster, p.code)

plot(pca$x[,1:2], col= kmeans(d, 38, nstart=100)$cluster, main="38 cluster")
plot(pca$x[,1:2], col= kmeans(d, 5, nstart=100)$cluster, main="5 cluster")







