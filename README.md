Name of Quantlet: Q-Kolleg2016

Description: This folder provides quantlets for Q-Kolleg 2016.
Files: 
- This project consists of 2 approaches to text mining that each require multiple steps: 
    1) Scraping of data from CRC 649  (01_Scraping)
    2) Converting scraped data into usable text format for R (02_PDF_to_txt)
    3) Combine text data in a corpus (03_Stem_TDM_TFIDF)
    4) Run the analyses (04_Analysis)
- Every step depends on the outcome of the previous step, but starting points are provided for every step in form of .Rdata-files. The .Rdata-files contain the outcome of the previous step. This allows you to focus on one particular step of the process, without having to deal with preparation steps. 

- Two approaches are used to find coherent structures among the articles:
    1) Clustering of academic articles based on their abstracts
    2) Topic modeling (LDA) based on the entire article
- Files used for approach 1 are labeled as "Abstracts_*", whereas files used for approach are called "Articles_*"
- In the analysis of both approaches, the outcomes will be compared to JEL (Journal of Economic Literature) codes and project groups of the CRC 649.



Keywords: Topic modeling, R, Webscraping, Latent Dirichlet Allocation, Textmining, Clustering, Wordcloud

Author: Ken Schröder, Johannes Stoiber

Submitted:  2016/11/14
