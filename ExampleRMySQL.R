# R code to import data from a database into R using RMySQL

#################################################################
### Setup
#################################################################

rm(list = ls())
graphics.off()

# Installing RMySQL, Loading RMySQL
  libraries = c("RMySQL")
  lapply(libraries, function(x) if(
    !(x %in% installed.packages())){install.packages(x)})
  lapply(libraries, require, quietly = TRUE, character.only = TRUE)

#################################################################
### RMySQL (local database)
#################################################################

# Connect to world database:
# 1. Download from MySQL: https://dev.mysql.com/doc/index-other.html
# 2. Load via DBMS as database called world. 
  con = dbConnect(drv = MySQL(),
                  user = "root", password = "qwertz",
                  dbname = "world", host = "localhost")

# List with tables in connection
  dbListTables(con)

# List variables in specific table
  dbListFields(con, "city")

# Run a query:
  # Specify query
  qry = "SELECT * FROM city"
  # Send query
  rs = dbSendQuery(con, qry)
  # Fetch query and import in R as data.frame
  data = fetch(rs)
  class(data)

# Free the result, so that other queries can be submitted
  dbClearResult(rs)


# Close connection to local database
  dbDisconnect(con)

# Proceed to work with data
  head(data)
  
  
#################################################################
### RMySQL (remote database)
#################################################################
  # Establish connection with a public database
  drv = dbDriver("MySQL") 
  con = dbConnect(drv, dbname = "ensembl_mart_84", user = "anonymous", 
                        host = "martdb.ensembl.org", port = 5316) 

# Checking the tables
  tables = dbListTables(con)
  length(tables)
  # tables[substr(tables, 1, 4) == "oana"]
  # oanatinus_gene_ensembl__gene__main seems interesting

# Long, explicit way of running a query
  qry1  = "SELECT * FROM  oanatinus_gene_ensembl__gene__main"
  qry2  = "SELECT * FROM oanatinus_gene_ensembl__go_GO__dm"

  rs1  = dbSendQuery(con, qry1)
  data = fetch(rs1)
  dbClearResult(rs1)

  rs2   = dbSendQuery(con, qry2)
  data2 = fetch(rs2)
  dbClearResult(rs2)

# Submit, fetch and close with one command
  data1 = dbGetQuery(con, qry1)
  data2 = dbGetQuery(con, qry2)

# Close connections
  dbDisconnect(con)
