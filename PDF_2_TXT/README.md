
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **PDF_2_TXT** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : PDF_2_TXT

Published in : <‘> Not published <‘>

Description : 'Extracts text data from PDF-documents and saves it as .txt-files. This script
requires the software pdftotext to be installed and requests the location of pdftotext in a pop-up
window'

Keywords : pdf2text, pdftotext

See also : 'ScrapeArticles, ScrapeJEL, ScrapeAbstracts StemTFIDF_Abstracts, StemTFIDF_Articles,
Stem_JEL, Textmining_Abstracts, TopicModelling_Articles'

Author : Ken Schröder, Johannes Stoiber

Submitted : Fri, Feb 10 2017 by Ken Schröder

Input : 800+ .pdf documents

Output : 800+ .txt documents to StemTFIDF_Articles

```


### R Code:
```r
# Important note: 
#   1) Requires installation of pdftotext
#   2) When running the script, select the location of pdftotext
#   3) Translation from PDF to txt takes a while (5 - 10 minutes)
#   4) The American Economic Association papers have been omitted from
#         this code. The textmining procedure is also ommited. The TFIDF
#         matrix may be found under "TopicModelling_Articles/AEA_TFIDF"

########################################################################

### 1. Identify pdftotext location
### 2. Convert PDFs to txt
### 3. Move the .txt-converted files to another folder

########################################################################

setwd("PDF_to_TXT")

### 1. Identify pdftotext locations                                #####
  pdf2txt.path <- file.choose()


### 2. Convert PDFs to txt                                         #####
  mypdfs = list.files(pattern = "*.pdf")
  lapply(mypdfs, function(i){
    system(paste('\"', pdf2txt.path, "\"", paste0(' "', i, '"'), sep = ""), wait = F)})


  ## Some pdf files are marked as password protected.. 
  ## Only keep the pdfs where .txt translation is possible:
  mytxts = list.files(pattern = "*.txt")
  mypdfs = list.files(pattern = "*.pdf")
  succesful.convert = (substr(mypdfs, 0, nchar(mypdfs)-4) %in% substr(mytxts, 0, nchar(mytxts)-4))
  file.remove(mypdfs[!succesful.convert])
  mypdfs = list.files(pattern = "*.pdf")

  
### 3. Move the .txt-converted files to another folder
  ## Move the .txt files to another directory: StemTFIDF_Articles
    # Get the full names of the .txt-converted files:
    oldpath = getwd()
    fromhere = list.files(oldpath, pattern = "*.txt", full.names = T)
    
    # Move two folders down in the path and create a folder called txt
    basepath = dirname(dirname(oldpath))
    newpath = paste0(basepath, "/StemTFIDF_Articles")
    
    # Create another vector with new location full names
    tohere = paste0(newpath, "/", mytxts)
    
    # Migrate the .txt files to the new location
    file.rename(fromhere, tohere)
    # NOTE: The .txt-files for the AEA articles have been omitted

```
