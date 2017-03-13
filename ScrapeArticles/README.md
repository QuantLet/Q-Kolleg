
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **ScrapeArticles** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : ScrapeArticles

Published in : <‘> Not published <‘>

Description : 'Identifies valid URLs for the articles and downloads and saves the full PDFs of all
publications from the CRC 649 website.'

Keywords : Web scraping, data collection, CRC 649, SFB

See also : 'ScrapeAbstracts, ScrapeJEL, PDF_2_TXT, StemTFIDF_Abstracts, StemTFIDF_Articles,
Stem_JEL, Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Output : 800+ PDF documents are saved to PDF_2_TXT

```


### R Code:
```r
# CAUTION: This script downloads 800+ PDFs (800+ MB)

library(magrittr)
library(rvest)
library(httr)

########################################################################

### 0. Functions for this script
### 1. Scrape table-info from CRC 649 page
### 2. Scrape & save CRC 649 papers (PDF)

########################################################################


#### 0. Functions for this script                                  #####
  
  # get body of html files
  totext = function(x){
    # input x is a list containing text from the html files
    xpathApply(x, "//body//text()", xmlValue)[[1]]
  }


#### 1. Get the table-info from the CRC 649 page:                  #####
  
  drc = POST("target_url.php",
           body = list(firstsearch = "allauthors",
                       sec.second = "allyears",
                       B1 = "Search"), encode = "form")
  table_info = read_html(drc) %>%
    html_nodes("body") %>%
    html_nodes("tbody") %>%
    html_nodes("tr") %>%
    html_nodes("tr") %>%
    html_nodes("table") %>%
    `[`(4) %>%
    html_table(header = NA, fill = T)
  table_info = table_info[[1]]

  # Some page specific manipulations:
  table_info           =   table_info[-nrow(table_info), ]
  colnames(table_info) =   c("number", "title", "authors", "projectcode", 
                              "date", "jel", "abstract", "download", 
                              "quantlets", "empty")
  cols                 = ncol(table_info)
  table_info           = table_info[, -((cols-3):cols)]
  table_info           = lapply(table_info, str_replace_all, "\r\n", "")

  # date in date format
  table_info$date      = as.Date(table_info$date, format="%d.%m.%Y")
  
  
#### 2. Scrape & save CRC 649 papers (PDF)                        #####
  
  # Get the URLs and create names for the pdfs:
  fromhere = paste0("https://sfb649.wiwi.hu-berlin.de/papers/pdf/SFB649DP", 
                    table_info$number, ".pdf", sep = "")
  
  # Set the directory to the location where the PDFs should be saved:
  setwd("PDF_2_TXT")
  
  
  # Download papers from all valid links, ignore invalid ones:
  for(i in 1:length(fromhere)){
    tryCatch({download.file(fromhere[i], paste0(table_info$number[i], ".pdf"))}, 
             error = function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  }
  

```
