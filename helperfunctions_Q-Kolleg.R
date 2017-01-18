###############################################################################
##                                                                           ##
##     This file provides helper functions for the whole procedure           ##
##                                                                           ##
##     1. Functions used in webscraping                                      ##
##     2. Functions for treetagger - preporcessing                           ##
##     3. Functions used for JEL code scraping                               ##
##     4. Functions for dictionary classification                            ##
##     5. Functions for textmining procedure                                 ##
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
  setwd(paste0(path, "/Abstracts"))
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
##     3.  Functions used for JEL code scraping - JEL_scraping.R             ##
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
remove_terms = function(strings, badterms){
  badterms = paste0(badterms, " ")
  for(i in 1:length(badterms)){
    strings = gsub(badterms[i], "", strings)
    }
  return(strings)
  }


###############################################################################
##     4.  Functions for   - Dictionary_Classification.R                     ##
###############################################################################

# Check how often a word occurs in a given dictionary:
  word_nr_match = function(doc, dictionary, binary = FALSE){
    hits = sum(sapply(doc, function(x){grepl(x, dictionary, fixed = T)}))
    if(binary){
      hits = sum(hits > 0)
    }
    return(hits)
  }

# Calculate the sum of occurances for all words in a document
# in a given dictionary:
  doc_sum_match = function(onedoc, onedict, correct = FALSE, ...){
    docsummatch = sum(sapply(onedoc, word_nr_match, onedict, ...))
    if(correct){
      docsummatch = docsummatch / log(words_in_dict(onedict))
    }
    return(docsummatch)
  }

  words_in_dict = function(dict){
    sapply(gregexpr("\\W+", dict), length) + 1
  }
  
# Extend above to all dictionaries (all words in one doc, all dictionaries)
  doc_all_dicts = function(onedoc, alldicts, ...){
    sapply(alldicts, doc_sum_match, onedoc = onedoc, ...)
  }

# Extract the JEL-code that corresponds to the best matching dictionary
# for a given document: Choose randomly if multiple dictionary have max.
# matching criteria. 
  max_jel = function(onedoc, alldicts, jelcodes, ...){
    match_dict = doc_all_dicts(onedoc, alldicts, ...)
    bestdict = order(-match_dict)
    return(jelcodes[bestdict])
  }

# Do max_jel() for all documents:
  alldocs_alldicts = function(alldocs, ...){
    lapply(alldocs, max_jel, ...)
  }

# Get some summary statistics of the classified list:
  summarize = function(classifiedlist){
    sapply(levels(lemdict$JELcode), function(x){sum(x %in% classifiedlist)})
  }

# Extract the letters from the RDC-based JEL-codes:
  getletters = function(x){
    na.omit(unlist(strsplit(unlist(x), "[^a-zA-Z]+")))
  }
  
# Get the n-th best prediction:
  nth_hit = function(pred_labels, from_to = c(1, 1)){
    lapply(pred_labels, function(x){x[from_to[1]:from_to[2]]})
  }
  
# Calculate the percentage of time in which the real and predicted
  # labels coincide: hit rate.
  performance = function(realcodes, predcodes, from_to = c(1, 1)){
    # realcodes and predcodes may contain more than one JEL-code
    predcodes = nth_hit(predcodes, from_to = from_to)
    intersection = mapply(function(x, y) intersect(x, y), predcodes, realcodes)
    hits = sum(sapply(intersection, function(x){length(x) != 0})) 
    hitrate = hits / length(predcodes)
    return(hitrate)
  }

  # Get the performance development of the best labels to worst labels:
  performance_dev_single = function(realcodes, predcodes){
    sapply(1:20, function(x){
      performance(realcodes, predcodes, from_to = c(x, x))})
  }
  
  # Get the accumulated performance development of adding an x-th label
  performance_cumul = function(realcodes, predcodes){
    sapply(1:20, function(x){
      performance(realcodes, predcodes, from_to = c(1, x))
    })
  }
  
  # Plot the development of the hits for the n-th best dictionary match:
  performance_plot = function(realcodes, predcodes1, predcodes2, predcodes3, cumul = TRUE){
    if(cumul){
      perform1 = performance_cumul(realcodes, predcodes1)
      perform2 = performance_cumul(realcodes, predcodes2)
      perform3 = performance_cumul(realcodes, predcodes3)
    } else{
      perform1 = performance_dev_single(realcodes, predcodes1)
      perform2 = performance_dev_single(realcodes, predcodes2)
      perform3 = performance_dev_single(realcodes, predcodes3)
    }
    par(mar = c(5.1, 4.1, 4.1, 13.1), xpd = TRUE)
    plot(perform1, type = "b", col = "red", ylim = c(0, 1), pch = 16,
         xlab = "n-th best match", ylab = "% containing correct label",
         lwd = 1.2, main = "performance development, dictionary approach")
    legend("bottomright", inset = c(-0.2, 0), legend = c("No correction, dupes",
                      "Correction, dupes",
                      "Correction, no dupes"), lwd = 0.9,
           col = c("blue", "red", "green"), bty = "n")
    lines(perform2, col = "green", type = "b", pch = 16)
    lines(perform3, col = "blue", type = "b", pch = 16)
  }
  

###############################################################################
##     5. Functions for textmining procedure                                 ##
###############################################################################

# functions that help to detect letters and numbers in character string
is.letter = function(x){grepl("[[:alpha:]]", x)}
keep.letter = function(x){gsub("[^[:alpha:]]", "", x)}
is.number = function(x){grepl("[[:digit:]]", x)}

# function to extract the project code
get_pcode = function(projectcode){
  p.code = substr(projectcode, 1, 3)   
  letters = which(is.letter(substr(p.code,3,3)) == TRUE)
  p.code[letters] = substr(p.code[letters],1,2)
  return(p.code)
  }


## Bunch of functions for later manipulations:
  # Create Document-Term-Matrix:
  sparse_dtm = function(corpus, ...){
    DocumentTermMatrix(corpus, control = list(...))
  }
  
  # Remove sparse terms:
  small_dtm = function(corpus, threshold = 0.99, ...){
    removeSparseTerms(sparse_dtm(corpus, ...), sparse = threshold)
  }
  
  # Perform log(1 + x):
  log_x = function(termdoc){log(1 + as.matrix(termdoc))}
  
  # Inspect the terms present in the Document-Term-Document 
  getfreq = function(dtm){
    freqs = colSums(as.matrix(dtm))
    return(freqs[order(-freqs)])
  }
  
  # Dimensionality reducer:
  reduced_dim_pca = function(pca, thresh = 0.66){
    keep = sum((cumsum(pca$sdev)/length(pca$sdev)) < thresh)
    return(pca$x[,1:keep])
  }


