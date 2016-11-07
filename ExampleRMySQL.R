# R code to import data from a database into R using RMySQL


#################################################################
### Setup
#################################################################

rm(list = ls())
graphics.off()

# Installing RMySQL, Loading RMySQL
libraries = c("RMySQL")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

#################################################################
### RMySQL
#################################################################

# connect to world database:
# 1. download from MySQL: https://dev.mysql.com/doc/index-other.html
# 2. Load via DBMS as database called world. 
con = dbConnect(drv=MySQL(),
                user="root", password = "qwertz",
                dbname = "world", host = "localhost")

# List with tables in connection
dbListTables(con)

# List variables in specific table
dbListFields(con, "city")

# run a query

qry = "SELECT * FROM city"
rs1 = dbSendQuery(con, qry)
data = fetch(rs1)
class(data)

# Free resources associated with rs1
dbClearResult(rs1)


# close connection to database
dbDisconnect(con)

# proceed to work with data
head(data)
