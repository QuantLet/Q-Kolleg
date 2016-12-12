# Helper functions for JEL_scraping:

# Clean the html text for second and third hierarchy JEL-codes:
  # Third hierarchy is e.g. A13
  # Second hierarchy is e.g A1
clean_html <- function(htmltext){
  htmltext <- htmltext[substr(htmltext, 0, 3) == "JEL"]
  htmltext <- gsub("JEL: *", "", htmltext)
  htmltext <- gsub("* – *", " ", htmltext)
  htmltext <- htmltext[order(htmltext)]
}

# Clean the html text for first hierarchy JEL-codes:
  # First hierarchy is e.g. A
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