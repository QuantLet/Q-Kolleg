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

######################################################
###  Functions for dictionary classification:
######################################################
# Check how often a word occurs in a given dictionary:
  word_nr_match <- function(doc, dictionary){
    sum(sapply(doc, function(x){grepl(x, dictionary, fixed = T)}))
  }

# Calculate the sum of occurances for all words in a document
# in a given dictionary:
  doc_sum_match <- function(onedoc, onedict){
    sum(sapply(onedoc, word_nr_match, onedict))
  }

# Extend above to all dictionaries (all words in one doc, all dictionaries)
  doc_all_dicts <- function(onedoc, alldicts){
    sapply(alldicts, doc_sum_match, onedoc = onedoc)
  }

# Extract the JEL-code that corresponds to the best matching dictionary
# for a given document: Choose randomly if multiple dictionary have max.
# matching criteria. 
  max_jel <- function(onedoc, alldicts, jelcodes){
    match_dict <- doc_all_dicts(onedoc, alldicts)
    bestdict <- which(match_dict == max(match_dict))
    return(jelcodes[bestdict])
  }

# Do max_jel() for all documents:
  alldocs_alldicts <- function(alldocs, ...){
    lapply(alldocs, max_jel, ...)
  }

# Get some summary statistics of the classified list:
  summarize <- function(classifiedlist){
    sapply(levels(lemdict$JELcode), function(x){sum(classifiedlist == x)})
  }

# Extract the letters from the RDC-based JEL-codes:
  getletters <- function(x){
    na.omit(unlist(strsplit(unlist(x), "[^a-zA-Z]+")))
  }
  
# Calculate the percentage of time in which the real and predicted
  # labels coincide: hit rate.
  performance <- function(realcodes, predcodes){
    # realcodes and predcodes may contain more than one JEL-code
    intersection <- mapply(function(x, y) intersect(x, y), predcodes, realcodes)
    hits <- sum(sapply(intersection, function(x){length(x) != 0})) 
    hitrate <- hits / length(predcodes)
    return(hitrate)
  }
  