###############################################################################
##                                                                           ##
##     Mainfile for Textmining Application with Database Connection          ##
##                                                                           ##
##    1. path/wd                                                             ##
##    2. Load helperfunction file                                            ##
##    3. Run data prepertion R-scripts                                       ##
##    4. Run textmining R-script                                             ##
##                                                                           ##
##                                                                           ##
###############################################################################

rm(list = ls())
graphics.off()

libraries = c("httr","magritter","rvest","stringer","XML","RMySQL", "koRpus", "tm",
              "SnowballC", "cldr", "cluster")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

####### set working directory
wdJS  = "C:/Users/Johannes/Dropbox/Digital_Economics/"
wdKS  = "/Users/Ken/Dropbox/Digital_Economics/"
GitJS = "C:/Users/Johannes/Documents/GitHub/Q-Kolleg/"
GitKS = "/Users/Ken/Q-Kolleg/"

if(dir.exists(wdJS)  == TRUE){path = wdJS}
if(dir.exists(wdKS)  == TRUE){path = wdKS}
if(dir.exists(GitKS) == TRUE){pathGit = GitKS}
if(dir.exists(GitJS) == TRUE){pathGit = GitJS}

# load the helperfunctions
source(paste(pathGit,"helperfunctions_Q-Kolleg.R",sep=""))

# preperation only have to be done once in beginning
source(paste(pathGit,"Webscraping.R", sep=""))
source(paste(pathGit,"Treetagger.R", sep=""))
source(paste(pathGit,"JEL_scraping.R", sep=""))


## TODO HERE: extend rm by everything we do not need after preperations
rm("dfcontent", "alldocs", "cols", "con", "content", "doc", "drc", "drv", 
"fromhere", "i", "libraries", "GitJS", "GitKS", "plain.text", "r_abs", 
"savehere", "whichones")

## Working with the data: If neiterh CRC649 page changed nor JEL Code description
## skip the three steps above ( Webscraping.R, Treetagger.R, JEL_scarping and continue here)
source(paste(pathGit,"Textmining_Q-Kolleg.R", sep=""))

#### NEXT steps:
## Test Data preperation files. (all 3!)
## Find solution For password
## Extend rm() command by things we want to skip
## No need for PCA while doing kmeans
## Idea: Project on first 3 principal component in 3D
## Idea: Multidimensional scaling for projection, instead of PCA
## TFIDF
## Background: word2vec
## As some basic illustration create wordcloud
## Conduct with ppt presentation
