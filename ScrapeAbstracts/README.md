
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **ScrapeAbstracts** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : ScrapeAbstracts

Published in : <‘> Not published <‘>

Description : 'Scrapes the abstracts of all publications from CRC 649 website and prepares the
format for further textmining'

Keywords : Web scraping, data collection, CRC 649, SFB

See also : 'ScrapeArticles, ScrapeJEL, PDF_2_TXT, StemTFIDF_Abstracts, StemTFIDF_Articles,
Stem_JEL, Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Output : Abstracts_raw.Rdata is saved to StemTFIDF_Abstracts

```


### R Code:
```r
library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)
library(koRpus)
library(tm)

#####################################################################

## 0. Functions for the script
## 1. Scrape table-info from CRC 649 page
## 2. Scrape & save abstracts of CRC 649 papers

#####################################################################

### 0. Functions for the script:                  ###
  # get body of html files
  totext = function(x){
    # input x is a list containing text from the html files
    xpathApply(x, "//body//text()", xmlValue)[[1]]
  }


### 1. Get the table-info from the CRC 649 page:  ###

  drc = POST("https://sfb649.wiwi.hu-berlin.de/fedc/discussionPapers_formular_content.php",
              body = list(filterTypeName = "filterTypeName:AUTHORS",
                          filteryear = "all",
                          B1 = "Search"), encode = "form")
  abstr_info = read_html(drc) %>%
    html_nodes("body") %>%
    html_nodes("tbody") %>%
    html_nodes("tr") %>%
    html_nodes("tr") %>%
    html_nodes("table") %>%
    `[`(4) %>%
    html_table(header = NA, fill = T)
  abstr_info = abstr_info[[1]]

  # Some page specific manipulations:
  abstr_info = abstr_info[-nrow(abstr_info), ]
  colnames(abstr_info) = c("number", "title", "authors", "projectcode", 
                        "date", "jel", "abstract", "download", 
                        "quantlets", "empty")
  cols       = ncol(abstr_info)
  abstr_info = abstr_info[, -((cols-3):cols)]
  abstr_info = lapply(abstr_info, str_replace_all, "\r\n", "")
  
  # date in date format
  abstr_info$date = as.Date(abstr_info$date, format="%d.%m.%Y")


### 2. Scrape & save the abstracts of RDC-papers  ###
  
  # Get the URLs and parse them
  fromhere = paste0("http://sfb649.wiwi.hu-berlin.de/fedc/DP_abstract.php?id=SFB649DP", 
                     abstr_info$number, ".pdf")
  doc      = lapply(fromhere, function(x){htmlParse(x, encoding = "Latin-1")})
  
  # Get the body of the HTML-files
  plain.text = lapply(doc, totext)
  plain.text = lapply(plain.text, function(x){gsub("\r\n", " ", x)})
  plain.text = lapply(plain.text, function(x){gsub("\n|Abstract:", "", x)})

  # Combine the abstract texts with the paper information
  abstr_info$abstracts = unlist(plain.text)
  
  # Save the object in the 4_Analysis subfolder
  save(abstr_info, file = "StemTFIDF_Abstracts/Abstracts_raw.Rdata")
  

```
