###############################################################################
##                                                                           ##
##     Mainfile for Textmining Application with Database Connection          ##
##                                                                           ##
##  
##
##
###############################################################################

rm(list = ls())
graphics.off()

libraries = c("httr","magritter","rvest","stringer","XML","RMySQL", "koRpus", "tm", "SnowballC",
              "cldr")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

####### set working directory
JS = "C:/Users/Johannes/Dropbox/Digital_Economics"
KS = "/Users/Ken/Dropbox/Digital_Economics"

if(dir.exists(JS)== TRUE){path = JS}
if(dir.exists(KS)== TRUE){path = KS}

source(paste(path,"07_Code/helperfunctions.R",sep=""))
source(paste(path,"07_Code/data.R", sep=""))


## Note: 
# Requires installation of
# Treetagger (see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)


## load helperfunctions
source("helperfunctions_Q-Kolleg.R")

# load helperfunctions
source("helperfunctions_Q-Kolleg.R")