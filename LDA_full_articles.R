library(magrittr)
library(rvest)
library(httr)
library(stringr)
library(pdftools)
library(tm)
library(topicmodels)
library(RMySQL)
source("helperfunctions_Q-Kolleg.R")

## Get the names of the articles from the database:
drv = MySQL()
con = dbConnect(drv, dbname = "Q-Kolleg", 
                user = "schroedk.hub", password = "..",
                host = "neyman.wiwi.hu-berlin.de", port = 3306)
numbers = c(dbGetQuery(con, "SELECT number FROM abstracts"))

# Get the URLs and create names for the pdfs:
fromhere = paste0("https://sfb649.wiwi.hu-berlin.de/papers/pdf/SFB649DP", 
                  numbers$number, ".pdf", sep = "")

# Get the right directory for the articles:
if(basename(getwd()) != "RDC_papers"){
  if(!("RDC_papers" %in% list.files())){
    dir.create("RDC_papers"); setwd("RDC_papers")
  } else {setwd("RDC_papers")}
}

# Download papers from all valid links, ignore invalid ones:
for(i in 1:length(fromhere)){
  tryCatch({download.file(fromhere[i], paste0(numbers$number[i], ".pdf"))}, 
         error = function(e){cat("ERROR: ", conditionMessage(e), "\n")})
}

# Convert the pdf-files to txt-files using pdftotext.exe:
mypdfs = list.files(pattern = "*.pdf")
lapply(mypdfs, function(i){
  system(paste('"/Users/Ken/pdftotext"', paste0('"', i, '"')), wait = FALSE)})

## Some pdf files are marked as password protected.. 
## Only keep the pdfs where .txt translation is possible:
mytxts = list.files(pattern = "*.txt")
mypdfs = list.files(pattern = "*.pdf")
trans.pos = (substr(mypdfs, 0, nchar(mypdfs)-4) %in% substr(mytxts, 0, nchar(mytxts)-4))
file.remove(mypdfs[!trans.pos])
mypdfs = list.files(pattern = "*.pdf")

# Read strings from .txt into R-session:
mytxts2 = list.files(pattern = "*.txt", full.names = T)
abstractlist = lapply(mytxts2, function(textdoc){
                  paste0(scan(textdoc, what = character(), fileEncoding = "latin1"),  # or j =
                                collapse = " ")})

# Now that all text between Introduction and References has been extracted,
# We start with Latent Dirichlet Allocation using Gibbs sampling:
  # 1) Stem the documents in the corpus:
  # 2) Remove additional generic words, that were influencing the topic terms
  # 3) Create Document Term Matrix
  # 4) Run LDA with k topics
  #     # Run number of topics for every JEL-code present in dataset with > 1%:
  # 5) 
corpus = Corpus(VectorSource(abstractlist))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, c(stopwords("english"), 
                                        "/f", "use", "can", "one", "will"))
corpus = tm_map(corpus, PlainTextDocument)  
corpus = tm_map(corpus, stemDocument, "english")

TF = small_dtm(corpus, threshold = 0.995, weighting = weightTf)


# Determine number of topics to extract:
  # Right now, JEL codes used in less than 1% of the cases are ignored
  # Note that, as more articles become available, this restriction should be
  # removed!
  jel = dbGetQuery(con, "SELECT jel FROM abstracts")
  JELcats = sapply(jel$jel, keep.letter)
  JELcats = sapply(tolower(JELcats), strsplit, split = "", fixed = T)
  tab = table(unlist(JELcats))
  k = length(tab[tab/sum(tab) > 0.01])
  
## Running LDA for 15 and 5 topics:
lda.output <- LDA(TF, k = k, method = "Gibbs")

lda.topics <- as.matrix(topics(lda.output))
lda.terms <- as.matrix(terms(lda.output, 25))



## Checking whether the JEL descriptions will be assigned to same topics:
 ## Load JEL descriptions from database:
 dict <- dbGetQuery(con, "SELECT * FROM dictionary")

# The JELcodes that appear more than 1% of the time
relevant.JEL <- dict$JELcode[tolower(dict$JELcode) %in% names(tab[(tab/sum(tab) > 0.01)])]
smalldict <- dict[dict$JELcode %in% relevant.JEL,]

# Find the topics that the descriptions will be assigned to by LDA:
JEL.posterior <- posterior(lda.output, 
                           DocumentTermMatrix(Corpus(VectorSource(dict$dict))))
JEL.posterior.relevant <- posterior(lda.output, 
                           DocumentTermMatrix(Corpus(VectorSource(smalldict$dict))))

JEL.topics <- apply(JEL.posterior$topics, 1, which.max)
JEL.topics.relevant <- apply(JEL.posterior.relevant$topics, 1, which.max)

# Create nice overview:
names(JEL.topics) <- dict$description
names(JEL.topics.relevant) <- smalldict$description

JEL.topics.relevant
colSums(JEL.posterior.relevant$topics)


#### Save the complete stemmed corpus in the database:  ####
## Need to define INT
abstracts <- abstracts[-which(abstracts$number == "2009-027")[2],]
art.nr = substr(mytxts, 0, nchar(mytxts)-4)
articlesdf = data.frame(id = 1:length(art.nr), 
                        number = art.nr, 
                        jelreal = abstracts$jel[abstracts$number %in% art.nr],
                        stemmed = unlist(abstractlist), stringsAsFactors = F)

if (!("articles" %in% dbListTables(con))){
  dbSendQuery(con, "
              CREATE TABLE articles 
              (id INT,
              number VARCHAR(20),
              jelreal VARCHAR(20),
              stemmed VARCHAR(2000),
              PRIMARY KEY (id));")
}

# add data into table
dbWriteTable(con, name = "articles",
             value = articlesdf,
             row.names=F, append=TRUE, overwrite=FALSE)

# save the output of lda.output:
saveRDS(lda.output, "lda_output.rds")
