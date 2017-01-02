library(RMySQL)
source("helperfunctions_Q-Kolleg.R")


####  START USING THE DICTIONARIES:
  ## Get the data from the database:
    # Connect to the database:
    drv = MySQL()
    con = dbConnect(drv, dbname = "Q-Kolleg", 
                user = "schroedk.hub", password = "O9rVnS%J",
                host = "neyman.wiwi.hu-berlin.de", port = 3306)
    # Fetch the data:
    dictionaries = dbGetQuery(con, "SELECT * FROM dictionary")
    dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
    texts = transform(dbtt, splitwords = I(strsplit(as.character(lemma), " ")))

  ## Get the letter of the most common JEL code:
  JELraw = sapply(dbtt$jel, getletters)
  
  ## Classify all documents with duplicate words in dictionary:
  dict_classified = alldocs_alldicts(texts$splitwords, 
                                      dictionaries$dict, 
                                      dictionaries$JELcode,
                                      correct = TRUE,
                                      binary = FALSE)

  ## Classify all documents without duplicate words in dictionary:
  dict_unique = sapply(lapply(strsplit(dictionaries$dict, " "), unique), 
                       paste, character(1L), collapse = "")
  classified_unique_dict = alldocs_alldicts(texts$splitwords, 
                                            dict_unique,
                                            dictionaries$JELcode,
                                            correct = TRUE,
                                            binary = TRUE)
  
  ## Classify all documents without correcting for the length of the dictionary, 
    ## and allowing multiple matches for a single word:
  classified_unique_raw = alldocs_alldicts(texts$splitwords,
                                           dictionaries$dict,
                                           dictionaries$JELcode,
                                           correct = FALSE,
                                           binary = FALSE)
  
  ## Plot the performance of the classifiers:
  performance_plot(JELraw, dict_classified, classified_unique_dict, classified_unique_raw)


