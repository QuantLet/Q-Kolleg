## Define functions for stemming & lemmatization with Treetagger:
## Requires installation of
# Treetagger (see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)

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

# Combine the above two functions:
lemmastem = function(docname, ...){
  return(cleantags(treetagged(docname, ...)))
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


# Convert to lower case; strip white space; remove "abstract":
dbprep = function(strings){
  strings = lapply(strings, tolower)
  strings = lapply(strings, function(x){trimws(x, "both")})
  strings = lapply(strings, function(x){gsub("abstract ", "", x)})
  return(strings)
}