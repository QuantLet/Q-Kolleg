###############################################################################
##                                                                           ##
##     This File provides Helper Functions for The whole Procedure           ##
##                                                                           ##
##     1. Functions used in Webscraping                                      ##
##     2. Functions for Treetagger - Preporcessing                           ##
##     3. Functions used for JEL code scraping                               ##
##  
##
##
###############################################################################


###############################################################################
##     1. Functions used in Webscraping.R                                    ##
###############################################################################

## get body of html files
totext = function(x){
  # input x is a list containing text from the html files
  xpathApply(x, "//body//text()", xmlValue)[[1]]
  }

## Read & clean the abstracts:
readit = function(x){
  # input x are document files
  raw = paste0(readLines(x), collapse = "\r")
  str_replace_all(raw, "\r", " ")
  }

###############################################################################
##     2. Functions for Preprocessing - Treetagger.R                         ##
###############################################################################

## Define functions for stemming & lemmatization with Treetagger:
## Requires installation of
## Treetagger (see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)

## The actual lemma & stemming function. Requires installation of
## Treetagger (see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)

## apply treetagger software
treetagged = function(docname, objorfile = "file"){
  treetag(docname, 
          treetagger = "manual",
          format = objorfile,
          lang = "en",
          apply.sentc.end = TRUE, 
          encoding = "Latin1",
          TT.options = list(path = "/Users/Ken/Treetagger", 
                            preset = "en"),
          stopwords = tm::stopwords("en"),
          stemmer = SnowballC::wordStem)
  }

## Cleaning the tagged object from symbols, stopwords, etc.
cleantags = function(dirty){
  clean = kRp.filter.wclass(dirty, corp.rm.class = "nonpunct")
  clean = kRp.filter.wclass(clean, corp.rm.class = "stopword")
  clean = within(clean@TT.res, lemma[lemma == "<unknown>"] <- token[lemma == "<unknown>"])
  deletion = c("number", "symbol", "possesive", "punct", "sentc")
  clean = clean[!(clean$wclass %in% deletion),]
  return(clean)
  }


## Combine the above two functions:
lemmastem = function(docname, ...){
  return(cleantags(treetagged(docname, ...)))
  }


## Clean working directory from empty and non-english .txt files.
## Does not need any input
align_abstracts = function(){
  docs = list.files(pattern = "*.txt")
  small = which(file.size(docs) < 5)
  abstracts = dbGetQuery(con, "SELECT * FROM abstracts")
  langs = detectLanguage(abstracts$abs)
  noneng = which(langs$detectedLanguage != "ENGLISH")
  dropit = unique(c(small, noneng))
  file.remove(docs[dropit])
  author_etc = abstracts[-dropit,-ncol(abstracts)]
  return(author_etc)
  }


## Convert to lower case; strip white space; remove "abstract":
dbprep = function(strings){
  strings = lapply(strings, tolower)
  strings = lapply(strings, function(x){trimws(x, "both")})
  strings = lapply(strings, function(x){gsub("abstract ", "", x)})
  return(strings)
  }


###############################################################################
##     3.  Functions used for JEL code scraping                              ##
###############################################################################

## Clean the html text for second and third hierarchy JEL-codes:
## Third hierarchy is e.g. A13
## Second hierarchy is e.g A1
clean_html = function(htmltext){
  htmltext = htmltext[substr(htmltext, 0, 3) == "JEL"]
  htmltext = gsub("JEL: *", "", htmltext)
  htmltext = gsub("* - *", " ", htmltext)
  htmltext = htmltext[order(htmltext)]
  }

# Clean the html text for first hierarchy JEL-codes:
# First hierarchy is e.g. A
clean_headers = function(htmltext){
  htmltext = gsub("* Subcategories\\[edit\\]", "", htmltext)
  htmltext = gsub(" JEL:", "", htmltext)
  nrchars  = nchar(htmltext)
  headers  = substr(htmltext, nrchars, nrchars)
  htmltext = paste0(headers, " ", htmltext)
  htmltext = substr(htmltext, 0, nrchars)
  htmltext = htmltext[-c(1, length(htmltext)-1, length(htmltext))]
  return(htmltext)
  }

# Removes some of the superfluous words in the JEL-descriptions:
remove_terms <- function(strings, badterms){
  badterms <- paste0(badterms, " ")
  for(i in 1:length(badterms)){
    strings <- gsub(badterms[i], "", strings)
  }
  return(strings)
  }