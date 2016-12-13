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
  setwd(path)
  dir.create("Abstracts")
  setwd(paste0(path, "/Abstracts"))
    
  # Get the URLs and parse them:
  fromhere = paste0("http://sfb649.wiwi.hu-berlin.de/fedc/DP_abstract.php?id=SFB649DP", 
                     content$number, ".pdf")
  doc = lapply(fromhere, function(x){htmlParse(x, encoding = "Latin-1")})
  
  # Get the body of the HTML-files
  plain.text = lapply(doc, totext)
  
  # Take them all together, concatenate and save them:
  whichones = lapply(plain.text, function(x){paste(x, collapse = "\n")})
  savehere = paste0(1:length(whichones), ".txt")
  for(i in 1:length(whichones)){
    cat(whichones[[i]], file = savehere[i])
  }

## Pad the filenames with zeros (e.g. 1 ==> 001, 23 ==> 023):
  alldocs = list.files()
  file.rename(alldocs, str_pad(alldocs, 7, side = "left", pad = "0"))
  
## Create a data.frame containing table-data AND abstracts in one:
  r_abs = sapply(list.files(), readit)
  
  # dfcontent will contain all scraped data:
  dfcontent = as.data.frame(content, stringsAsFactors = F)
  dfcontent$abstracts = r_abs
    
  # turn around the order, that id 1 is always Paper 2005-001
  dfcontent = cbind("id"= 1:dim(dfcontent)[1],
                    dfcontent[(dim(dfcontent)[1]:1),])
   
  # date in date format
  dfcontent$date = as.Date(dfcontent$date, format="%d.%m.%Y")
    
## dfcontent is a dataframe containing all data, including unique identfier 
## Add to remote database
## Save information in Q-Kolleg db

  # Establish connection:
  drv = dbDriver("MySQL") 
  con = dbConnect(drv, dbname = "Q-Kolleg", 
                  user = "schroedk.hub", password = "..",
                  host = "neyman.wiwi.hu-berlin.de", port = 3306)
    
  # Create table called abstracts, if it doesn't yet exist in the database,
  # else an error messeage appears
  if (!("abstracts" %in% dbListTables(con))){
  dbSendQuery(con, "
              CREATE TABLE abstracts 
                (id INT,
                number VARCHAR(20),
                title  VARCHAR(300),
                authors VARCHAR(300),
                projectcode VARCHAR(30),
                date DATE,
                jel  VARCHAR(30),
                abstracts VARCHAR(2000),
                PRIMARY KEY (id));")
  }
  
  # add data into table
  dbWriteTable(con, name = "abstracts",
              value = dfcontent,
              row.names=F, append=TRUE, overwrite=FALSE)

  # Check info on columns in abstracts
  dbGetQuery(con, "DESCRIBE abstracts")
  
  # close DB connection
  dbDisconnect(con)
