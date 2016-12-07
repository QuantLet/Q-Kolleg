library(httr)
library(magrittr)
library(rvest)
library(stringr)
library(XML)

wiki <- POST("https://en.wikipedia.org/wiki/JEL_classification_codes")

clean_html <- function(htmltext){
  htmltext <- htmltext[substr(htmltext, 0, 3) == "JEL"]
  htmltext <- gsub("JEL: *", "", htmltext)
  htmltext <- gsub("* – *", " ", htmltext)
  htmltext <- htmltext[order(htmltext)]
}

clean_headers <- function(htmltext){
  htmltext <- gsub("* Subcategories\\[edit\\]", "", htmltext)
  htmltext <- gsub(" JEL:", "", htmltext)
  nrchars <- nchar(htmltext)
  headers <- substr(htmltext, nrchars, nrchars)
  htmltext <- paste0(headers, " ", htmltext)
  htmltext <- substr(htmltext, 0, nrchars)
  htmltext <- htmltext[-c(1, length(htmltext)-1, length(htmltext))]
  return(htmltext)
}

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
