library(magrittr)
library(rvest)
library(RMySQL)

source("helperfunctions_Q-Kolleg.R")

#### SCRAPE ONLY THE KEYWORDS OF THE JEL CODES ####

## Get the indicator for the JEL codes
  JEL = c(LETTERS[1:18], "Y", "Z")
  JEL_keys = list()

## Extract text from the different JEL-pages
  for(i in 1:20){
    url = paste0("https://www.aeaweb.org/content/jel_guide/second_level.php?class=", 
               JEL[i])
    JELpart = read_html(url) %>%
      html_nodes("body") %>%
      html_nodes("div") %>%
      `[`(3) %>%
      html_text()
    JEL_keys[[i]] = JELpart
  }

## Clean the keywords from hidden tabs etc.
  clean_keywords = function(input, rm_econ = T){
    input = gsub("\n\tKeywords:\n\n\t\t", "", input)
    input = gsub(",\t\t", " ", input)
    input = gsub("\t", "", input)
    input = tolower(input)
    if(rm_econ){
      input = gsub(" economcs", "", input)
      input = gsub("economic ", "", input)
      input = gsub(" economic", "", input)
    }
  }
  clean_keys = lapply(JEL_keys, clean_keywords)
  
  ## Remove duplicate keywords:
  clean_keys_unique =  sapply(sapply(sapply(clean_keys, strsplit, " "), unique), 
                              paste, collapse = " ")
  ## Strip leading and trailing whitespaces:
  clean_keys_unique = unlist(lapply(clean_keys_unique, trimws))
  clean_keys_unique = as.data.frame(cbind(JEL, clean_keys_unique), 
                                    stringsAsFactors = F)
  
## Save the keyword dictionary in the database:
  # Connect to the database:
  drv = MySQL()
  con = dbConnect(drv, dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "..",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)

  # Create table if not existent yet:
  if(!("keyworddict" %in% dbListTables(con))){
    dbSendQuery(con, "
                CREATE TABLE keyworddict
                (JEL VARCHAR(20),
                keyword_dict  VARCHAR(2000),
                PRIMARY KEY (JEL));")
  } 

  # Add information to db-table dictionary
  dbWriteTable(con, name = "keyworddict", value = clean_keys_unique, overwrite = T)
  dbDisconnect(con)
