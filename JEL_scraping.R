library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)
source("JEL_scraping_HF.R")
source("Treetagger_HF.R")

wiki <- POST("https://en.wikipedia.org/wiki/JEL_classification_codes")

# The most detailed descriptions at A13-level
wikiJEL1 <- read_html(wiki) %>%
  html_nodes("body")   %>%
  html_nodes("div")    %>%
  `[`(3)               %>%
  html_nodes("div")    %>%
  `[`(3)               %>%
  html_nodes("div")    %>%
  `[`(4)               %>%
  html_nodes("dl")     %>%
  html_nodes("dd")     %>%
  html_nodes("dl")     %>%
  html_nodes("dd")     %>%
  html_text()
wikiJEL1 <- clean_html(wikiJEL1)

# Less detailed descriptions at A1-level
wikiJEL2 <- read_html(wiki) %>%
  html_nodes("body") %>%
  html_nodes("div")  %>%
  `[`(3)             %>%
  html_nodes("div")  %>%
  `[`(3)             %>%
  html_nodes("div")  %>%
  `[`(4)             %>%
  html_nodes("p")    %>%
  html_text()
wikiJEL2 <- clean_html(wikiJEL2)

# Most general description at A-level
wikiJEL3 <- read_html(wiki) %>%
  html_nodes("body") %>%
  html_nodes("div")  %>%
  `[`(3)             %>%
  html_nodes("div")  %>%
  `[`(3)             %>%
  html_nodes("div")  %>%
  `[`(4)             %>%
  html_nodes("h2")   %>%
  html_text()
wikiJEL3 <- clean_headers(wikiJEL3)

# Putting them all together:
JEL <- c(wikiJEL1, wikiJEL2, wikiJEL3)
JEL <- JEL[order(JEL)]

# Get the data in the right shape and format (2 columns, 20 rows):
JEL <- str_split_fixed(JEL, " ", 2)
JEL[,1] <- substr(JEL[,1], 0, 1)
JEL <- as.data.frame(JEL)
colnames(JEL) <- c("code", "descr"); JEL$descr <- as.character(JEL$descr)

JEL <- aggregate(JEL$descr, by = list(JEL$code), 
                  FUN = function(x){paste0(x, collapse = " ")})
colnames(JEL) <- c("code", "descr")

### Lemmatize the dictionary:
  lemdict <- lapply(JEL$descr, function(x){lemmastem(as.vector(x), objorfile = "obj")})
  lemdict2 <- lemdict["lemma"]
  test <- dbprep(lemdict)
  