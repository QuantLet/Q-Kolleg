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
