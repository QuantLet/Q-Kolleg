library(koRpus)
library(tm)
library(SnowballC)
library(RMySQL)
library(cldr)

## Define functions for stemming & lemmatization
  # The actual lemma & stemming function. Requires installation of
  # Treetagger (see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)
  treetagged = function(docname, objorfile = "file"){
    treetag(docname, 
      treetagger = "manual", format = objorfile,
      lang = "en",
      apply.sentc.end = TRUE, 
      encoding = "Latin1",
      TT.options = list(path = "/Users/Ken/Treetagger", 
                        preset = "en"),
      stopwords = tm::stopwords("en"),
      stemmer = SnowballC::wordStem)
  }

  # Cleaning the tagged object from symbols, stopwords, etc.
  cleantags = function(dirty){
    clean = kRp.filter.wclass(dirty, corp.rm.class = "nonpunct")
    clean = kRp.filter.wclass(clean, corp.rm.class = "stopword")
    clean = within(clean@TT.res, lemma[lemma == "<unknown>"] <- token[lemma == "<unknown>"])
    deletion = c("number", "symbol", "possesive", "punct", "sentc")
    clean = clean[!(clean$wclass %in% deletion),]
    return(clean)
  }

  lemmastem = function(docname){
    return(cleantags(treetagged(docname)))
  }

  # Clean working directory from empty and non-english .txt files:
  align_abstracts = function(){
    docs = list.files(pattern = "*.txt")
    abstracts = dbGetQuery(con, "SELECT * FROM abstracts")
    langs = detectLanguage(abstracts$abs)
    noneng = which(langs$detectedLanguage != "ENGLISH")
    file.remove(docs[noneng])
    author_etc = abstracts[-noneng,-ncol(abstracts)]
    return(author_etc)
  }
  
## Connect to database. Use info from abstracts-table to detect languages
 ## and remove empty and non-english abstracts
  drv = MySQL()
  con = dbConnect(drv, dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)
  source("/Users/Ken/Q-Kolleg/Webscraping.R")
  
## Use the above functions to lemmatize and stem the text:
  # Remove empty and non-english text files from the working directory:
  author_etc = align_abstracts()
  
  # Lemmatize and stem the text-documents:
  lemstems = list()
  docs = list.files()
  for(i in 1:length(docs)){
    lemstems[[i]] = lemmastem(docs[i])
  }

  # Extract the lemmatized and stemmed text:
  lemmas = lapply(lemstems, `[`, "lemma")
  stems = lapply(lemstems, `[`, "stem") 

## Since we want to also compare this lemmatization to stemming
  ## in a tdm context, we'll put it all together again....
  ## Also, this result will be used to store in the database.
     lemmastring = lapply(lapply(lemmas, `[`, "lemma"), 
                           function(x){paste(x$lemma, collapse = " ")}
                           )
     stemstring = lapply(lapply(stems, `[`, "stem"),
                          function(x){paste(x$stem, collapse = " ")}
                          )
     # lowercase, strip white space & remove "abstracts" from text:
     dbprep = function(strings){
       strings = lapply(strings, tolower)
       strings = lapply(strings, function(x){trimws(x, "both")})
       strings = lapply(strings, function(x){gsub("abstract ", "", x)})
       return(strings)
     }
     lemmastring = dbprep(lemmastring)
     stemstring = dbprep(stemstring)
  
  ## Get the tree-tagged results in a suitable shape for the database:
     lemmadf = as.data.frame(lemmastring, stringsAsFactors = F, row.names = "lemma")
     stemdf  = as.data.frame(stemstring,  stringsAsFActors = F, row.names = "stem")
     treetagg_res = data.frame(author_etc, t(lemmadf), t(stemdf), 
                               stringsAsFactors = F, row.names = F)
     treetagg_res$row_names = NULL
  
## Save the treetag-results in our database:
  # Create a new table with  in the database, called treetag
    dbWriteTable(con, name = "treetagger", value = treetagg_res, 
                 overwrite = T, row.names = F)
    
  # Check out the data from the "treetagger" table in the database:
    dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
  