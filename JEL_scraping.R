<<<<<<< HEAD
library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)
library(koRpus)
library(RMySQL)
=======

setwd("/Users/Ken/Q-Kolleg")
source("helperfunctions_Q-Kolleg.R")


### Scrape the three levels of the JEL-code hierarchy:
  wiki = POST("https://en.wikipedia.org/wiki/JEL_classification_codes")

  # The most detailed descriptions at A13-level
    wikiJEL1 = read_html(wiki) %>%
      html_nodes("body")   %>%
      html_nodes("div")    %>%
      `[`(3)               %>%
      html_nodes("div")    %>%
      `[`(3)               %>%
      html_nodes("div")    %>%
      `[`(4)               %>%
      html_nodes("dl")     %>%
      html_nodes("dd")     %>%
      html_nodes("dl")     %>%
      html_nodes("dd")     %>%
      html_text()
    wikiJEL1 = clean_html(wikiJEL1)

  # Less detailed descriptions at A1-level
    wikiJEL2 = read_html(wiki) %>%
      html_nodes("body") %>%
      html_nodes("div")  %>%
      `[`(3)             %>%
      html_nodes("div")  %>%
      `[`(3)             %>%
      html_nodes("div")  %>%
      `[`(4)             %>%
      html_nodes("p")    %>%
      html_text()
    wikiJEL2 = clean_html(wikiJEL2)

  # Most general description at A-level
    wikiJEL3 = read_html(wiki) %>%
      html_nodes("body") %>%
      html_nodes("div")  %>%
      `[`(3)             %>%
      html_nodes("div")  %>%
      `[`(3)             %>%
      html_nodes("div")  %>%
      `[`(4)             %>%
      html_nodes("h2")   %>%
      html_text()
    wikiJEL3 = clean_headers(wikiJEL3)

    # Putting them all together:
      JEL = c(wikiJEL1, wikiJEL2, wikiJEL3)
      JEL = JEL[order(JEL)]

    # Get the data in the right shape and format (2 columns, 20 rows):
      JEL = str_split_fixed(JEL, " ", 2)
      JEL[,1] = substr(JEL[,1], 0, 1)
      JEL = as.data.frame(JEL)
      colnames(JEL) = c("code", "descr")
      JEL$descr = as.character(JEL$descr)

    # Combine (aggregate) all JEL-descriptions from the same JEL-maincode:
      JEL = aggregate(JEL$descr, by = list(JEL$code), 
                        FUN = function(x){paste0(x, collapse = " ")})
      colnames(JEL) = c("code", "descr")

## Lemmatize the dictionary:
  # Use treetagger and extract the lemmatized texts
    lemdict = lapply(JEL$descr, 
                     function(x){lemmastem(as.vector(x), objorfile = "obj")})
    lemdict = sapply(lemdict, `[`, c("lemma"))
    lemdict = lapply(lemdict, paste0, collapse = " ")
  
  # Remove whitespace, to lower case, unwanted words
    lemdict = dbprep(lemdict)
    lemdict = remove_terms(lemdict, 
                           badterms = c("general", "introductory", "unclassified"))
    lemdict = cbind(levels(JEL$code), as.data.frame(lemdict, stringsAsFactors = F))
    lemdict = cbind(wikiJEL3, lemdict)
    names(lemdict) = c("description", "JELcode", "dict")

## Save the lemmatized dictionary in the database:
  # Connect to the database:
  drv = MySQL()
  con = dbConnect(drv, dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "..",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)
  
  # Create table if not existent yet:
  if(!("dictionary" %in% dbListTables(con))){
  dbSendQuery(con, "
              CREATE TABLE dictionary
              (description VARCHAR(100),
              JELcode VARCHAR(20),
              dict  VARCHAR(2000),
              PRIMARY KEY (JELcode));")
  } 

  # Add information to db-table dictionary
    dbWriteTable(con, name = "dictionary", value = lemdict,
                 append=T, overwrite = F, row.names = F)
