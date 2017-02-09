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
  
  drc = POST("https://sfb649.wiwi.hu-berlin.de/fedc/discussionPapers_formular_content.php",
           body = list(filterTypeName = "filterTypeName:AUTHORS",
                       filteryear = "all",
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
  table_info = table_info[-nrow(table_info), ]
  colnames(table_info) = c("number", "title", "authors", "projectcode", 
                           "date", "jel", "abstract", "download", 
                           "quantlets", "empty")
  cols = ncol(table_info)
  table_info = table_info[, -((cols-3):cols)]
  table_info = lapply(table_info, str_replace_all, "\r\n", "")

  # date in date format
  table_info$date = as.Date(table_info$date, format="%d.%m.%Y")
  
  
#### 2. Scrape & save CRC 649 papers (PDF)                        #####
  
  # Get the URLs and create names for the pdfs:
  fromhere = paste0("https://sfb649.wiwi.hu-berlin.de/papers/pdf/SFB649DP", 
                    table_info$number, ".pdf", sep = "")
  
  # Create a directory where the PDF articles can be saved:
  setwd("02_PDF_to_txt")
  if(!("PDFs" %in% list.files())){dir.create("PDFs")}
  setwd("PDFs")
  
  # Download papers from all valid links, ignore invalid ones:
  for(i in 1:length(fromhere)){
    tryCatch({download.file(fromhere[i], paste0(table_info$number[i], ".pdf"))}, 
             error = function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  }
  
