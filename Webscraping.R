library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)

### Scrape the text data from RDC-page: ###

## Get the table-info from the RDC-page:
  drc = POST("https://sfb649.wiwi.hu-berlin.de/fedc/discussionPapers_formular_content.php",
              body = list(filterTypeName = "filterTypeName:AUTHORS",
                          filteryear = "all",
                          B1 = "Search"), encode = "form")
  content = read_html(drc) %>%
    html_nodes("body") %>%
    html_nodes("tbody") %>%
    html_nodes("tr") %>%
    html_nodes("tr") %>%
    html_nodes("table") %>%
    `[`(4) %>%
    html_table(header = NA, fill = T)
  content = content[[1]]

  # Some page specific manipulations:
    content = content[-nrow(content), ]
    colnames(content) = c("number", "title", "authors", "projectcode", 
                           "date", "jel", "abstract", "download", 
                           "quantlets", "empty")
    cols = ncol(content)
    content = content[, -((cols-3):cols)]
    content = lapply(content, str_replace_all, "\r\n", "")
  
## Get the abstracts to local directory:
  # Create a folder called "Abstracts" in the working directory:
    dir.create("Abstracts")
    setwd(paste0(getwd(), "/Abstracts"))
    
  # Get the URLs and parse them:
  fromhere = paste0("http://sfb649.wiwi.hu-berlin.de/fedc/DP_abstract.php?id=SFB649DP", 
                     content$number, ".pdf")
  doc = lapply(fromhere, function(x){htmlParse(x, encoding = "Latin-1")})
  
  # Get the body of the HTML-files
  totext = function(x){
    xpathApply(x, "//body//text()", xmlValue)[[1]]}
  plain.text = lapply(doc, totext)
  
  # Take them all together, concatenate and save them:
  whichones = lapply(plain.text, function(x){paste(x, collapse = "\n")})
  savehere = paste0(1:length(whichones), ".txt")
  for(i in 1:length(whichones)){
    cat(whichones[[i]], file = savehere[i])
  }

# Pad the filenames with zeros (e.g. 1 ==> 001, 23 ==> 023):
  alldocs = list.files()
  file.rename(alldocs, str_pad(alldocs, 7, side = "left", pad = "0"))
  
## Create a data.frame containing table-data AND abstracts in one:
  # Read & clean the abstracts:
    readit = function(x){
      raw = paste0(readLines(x), collapse = "\r")
      str_replace_all(raw, "\r", " ")
    }

    r_abs = sapply(list.files(), readit)
  
    # dfcontent will contain all scraped data:
    dfcontent = as.data.frame(content, stringsAsFactors = F)
    dfcontent$abstracts = r_abs
  
## dfcontent is a dataframe containing all data ##
  