rm(list = ls())
graphics.off()


libraries = c("tm", "RMySQL", "SnowballC","wordcloud","slam","reshape2" ,"ggplot2")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

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
replaceSpecialChars <- function(d) {gsub("[^a-z]", " ", d)} 
corpus <- tm_map(corpus, replaceSpecialChars)
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


