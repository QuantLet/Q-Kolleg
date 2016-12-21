Name of Quantlet: Q-Kolleg2016

Description: This folder provides quantlets for Q-Kolleg 2016.
Files: 
- The "main.R" file loads neccessary files in an approprite order. Run step by step for first time. As long CRC 649 page is not updated or JEL code have not changed, for 2nd time go directly to Textmining_Q-Kolleg"
- The "HF.R" file provides helper functions for whole procedure.
- The "Webscraping.R" file scrapes information from the CRC649 publication website, and saves the information on the Q-Kolleg database, hosted at RDC (Humboldt-University). Only has to be performed before Analysis or if new publications were added at CRC649 website.
- The "Treetagger.R" file removes non-english abstracts and applies natural language processing on the remaining ones. As a result, a new table with name "treetagger" is added to the Q-Kolleg database, with stemmed and  lemmatized versions of the abstracts.
- The "JEL_scraping.R" file scrapes information on JEL Codes from Wikipedia and constructs a dictionary and saves the dictionaries in a new table called "dictionary" in the Q-Kolleg database. 
- The "Textmining_Q-Kolleg.R" file performs textmining in the preliminary way that we have seen in the beginning of the course.
- The "Dictionary_Classification.R" file gets the JEL-dictionaries from the database and matches the abstracts with those JEL-dictionaries.



Keywords: MySQL, RMySQL, database in R, textmining, wordcloud

Author: Ken Schröder, Johannes Stoiber

Submitted:  2016/11/14
