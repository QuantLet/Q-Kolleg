library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)
library(koRpus)
library(tm)

########################################################################

### 0. Functions for this script
### 1. Scrape JEL codes and description from Wikipedia
### 2. Clean, reformat and save the scraped information

########################################################################


#### 0. Functions for this script

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

  
#### 1. Scrape JEL codes and description from Wikipedia
  
  # Scrape the three levels of the JEL-code hierarchy:
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
  

#### 2. Clean, reformat and save the scraped information
  
  # Get the data in the right shape and format (2 columns, 20 rows):
  JEL = str_split_fixed(JEL, " – ", 2)
  JEL[,1] = substr(JEL[,1], 0, 1)
  JEL = as.data.frame(JEL, stringsAsFactors = F)
  colnames(JEL) = c("code", "descr")

  # Combine (aggregate) all JEL-descriptions from the same JEL-maincode:
  JEL = aggregate(JEL$descr, by = list(JEL$code), 
                  FUN = function(x){paste0(x, collapse = " ")})
  colnames(JEL) = c("code", "descr")
  

  # Save the JEL description in a .Rdata-file
  save(JEL, file = "StemJEL/JEL_raw.Rdata")
