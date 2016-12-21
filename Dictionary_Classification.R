library(RMySQL)
source("helperfunctions_Q-Kolleg.R")


####  START USING THE DICTIONARIES:
  ## Get the data from the database:
    # Connect to the database:
    drv = MySQL()
    con = dbConnect(drv, dbname = "Q-Kolleg", 
                user = "schroedk.hub", password = "..",
                host = "neyman.wiwi.hu-berlin.de", port = 3306)
    # Fetch the data:
    dictionaries = dbGetQuery(con, "SELECT * FROM dictionary")
    dbtt = dbGetQuery(con, "SELECT * FROM treetagger")
    texts = transform(dbtt, splitwords = I(strsplit(as.character(lemma), " ")))

  ## Classify all documents 
  dict_classified = alldocs_alldicts(texts$splitwords, 
                                      dictionaries$dict, 
                                      dictionaries$JELcode)

  ## Summarize the results and look for sensible results:
  class_summary = summarize(dict_classified)

  ## Get the letter of the most common JEL code:
  JELraw = sapply(dbtt$jel, getletters)

  ## Match between real and classified JEL:
  performance(JELraw, dict_classified)
