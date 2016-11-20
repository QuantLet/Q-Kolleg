rm(list = ls())
graphics.off()

# cldr-package is not available on CRAN:
if(!("cldr" %in% install.packages())){
  devtools::install_version("cldr", version = "1.1.0")
  }

# Load required packages:
libraries = c("tm", "cldr", "RMySQL", "SnowballC","wordcloud","slam","reshape2" ,"ggplot2")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)

#####################################################################
####    Text-mining from local source 
#####################################################################
setwd("C:\\Users\\Johannes\\Dropbox\\Digital_Economics\\Abstract_data")
filename = list.files()
# load data, 800 txt files
corpus = Corpus(DirSource())
# some information about corpus
meta(corpus)
class(corpus)
class(corpus[[1]])
corpus[1]
corpus[[1]]
inspect(corpus)

head(corpus)
# data preprocessing
# use lower case letters, remove punctuation, remove numbers
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
# replaceSpecialChars ist eine selbst geschriebene Funktion:
# special characters
replaceSpecialChars = function(d) {gsub("[^a-z]", " ", d)} 
corpus = tm_map(corpus, replaceSpecialChars)
# stemming
# corpus = tm_map(corpus, stemDocument)
# look at first abstract
writeLines(as.character(corpus[[1]]))
# some whitespaces are produced, so delete them
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)


# get term document matrix
tdm = TermDocumentMatrix(corpus,control = list(minWordLength = 1))
# remove very sparse terms
tdm.common = removeSparseTerms(tdm,0.9)
dim(tdm); dim(tdm.common)

# inspect 
inspect(tdm[1:10,1:8])
inspect(tdm.common[1:10,1:8])

# find words which occured more than 100 times
findFreqTerms(tdm, 100)

# find association with trading
findAssocs(tdm, "trading",0.3)


### wordcloud with words, that appear at least for 100 times
wordfreq = findFreqTerms(tdm.common, lowfreq=100)
termFrequency <- rowSums(as.matrix(tdm.common[wordfreq,])) 
wordcloud(words=names(termFrequency),freq=termFrequency,min.freq=3,max.words=50,random.order=T,colors=c("red","green","blue","purple","orange"))

# convert tdm in a matrix. needs less space
tdm.dense = as.matrix(tdm.common)

tdm.dense = melt(tdm.dense, value.name = "count")
head(tdm.dense)

# make a plot
ggplot(tdm.dense, aes(x = Docs, y = Terms, fill = log10(count))) 
geom_tile(colour = "white") 
scale_fill_gradient(high="#FF0000" , low="#FFFFFF")
ylab("") 
theme(panel.background = element_blank())
theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


### problem: document name disappeared. need to take care for document names

#####################################################################
####    Text-mining from Q-Kolleg database
#####################################################################
## Get data:
   drv = dbDriver("MySQL") 
   con = dbConnect(drv, dbname = "Q-Kolleg", 
                    user = "username", password = "password",
                    host = "neyman.wiwi.hu-berlin.de", port = 3306)
   allabstracts = dbGetQuery(con, "SELECT * FROM abstracts")
  
## Detect and split languages: (cldr-package)
   detlang = detectLanguage(allabstracts$abstext)
   table(detlang$detectedLanguage)
   langsplit = split(allabstracts$abstext, detlang$detectedLanguage)
   
   # We want to disregard the "Unknown"-languages, as they are empty abstracts:
     if("Unknown" %in% names(langsplit)){
        langsplit[[which(names(langsplit) == "Unknown")]] <- NULL
     }
    
## Data preparation: (tm-package)
  # Turn the abstract text into a "Corpus" which the tm-package requires:
    alltm_lang = lapply(langsplit, function(x){Corpus(VectorSource(x))})
   
  # Remove punctuation, stopwords, numbers, special characters, white space:
    alltm_lang = lapply(alltm_lang, tm_map, removePunctuation)
    alltm_lang[[1]] = tm_map(alltm_lang[[1]], removeWords, stopwords(kind = "en"))
    alltm_lang[[2]] = tm_map(alltm_lang[[2]], removeWords, stopwords(kind = "de"))
    alltm_lang = lapply(alltm_lang, tm_map, removeNumbers)
    alltm_lang = lapply(alltm_lang, tm_map, stripWhitespace)
    alltm_lang = lapply(alltm_lang, tm_map, PlainTextDocument)
    replaceSpecialChars = function(d) {gsub("[^a-z]äöüÄÖÜß", " ", d)} # allows äüöß
    alltm_lang = lapply(alltm_lang, tm_map, content_transformer(replaceSpecialChars))

  
## TermDocumentMatrix:
   tdm = lapply(alltm_lang, TermDocumentMatrix)
   tdm.cmn = lapply(tdm, removeSparseTerms, 0.9)

## Some pre-mature text analytics:
  # correlation between documents with "trading"
  lapply(tdm, findAssocs, "trading", 0.3)   # Obviously nothing for German
  
  # wordcloud with N most common words:
  N = 15
  tdmmat = lapply(tdm.cmn, as.matrix)
  sortitout = lapply(tdmmat, function(x){sort(rowSums(x), decreasing = T)})
  sortitout = lapply(sortitout, function(x){x[1:N]})
 
  getcloud = function(termfreq){
    wordcloud(words = names(termfreq), 
              freq = termfreq, min.freq = 1, 
              max.words = N, 
              colors = c("red","green","blue","purple","orange"))
  }
  getcloud(sortitout[[1]])
