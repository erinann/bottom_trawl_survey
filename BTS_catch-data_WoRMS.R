###########################################################################################
# Work with bottom trawl catch data
#
# Initial code is from Erin Labrecque
# modified by jech
#
# to run this from the command line:
# set the working directory to the directory of the program 
# setwd("/home/jjech/NOAA_Gdrive/sonarpros/R_programs/BTS")
# source("BTS_catch-data.R")


knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, 
                      fig.width = 7, fig.height = 6.5)
library(knitr)
library(tidyverse)
library(fs)
library(lubridate)
library(kableExtra)
library(rfishbase)
library(jsonlite)
library(httr)

### start with a clean slate
rm(list=ls(all=TRUE))

### connect to WoRMs
#AphiaID = 127160
# this is code from the WoRMS to R website
# I used it to play around with, but don't actually use it to match BTS names
#url <- sprintf("https://www.marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaID);
#classificationTree <- fromJSON(url)
#Walk the classification tree
#currentTreeItem = classificationTree
#while (!is.null(currentTreeItem )) {
#  print(sprintf("ID: %d, RANK: %s, ScientificName: %s",
#                currentTreeItem$AphiaID,
#                currentTreeItem$rank,
#                currentTreeItem$scientificname
#  ));
#  #You can access the variables individually
#  #print(currentTreeItem$AphiaID);
#  #print(currentTreeItem$scientificname);
#  #print(currentTreeItem$rank);
#  
#  #Get next item in the tree
#  currentTreeItem <- currentTreeItem$child;
#}

# These lines of code tell R to look in the "data" folder, read in each .csv file,
# and then bind all the row from each file together. All columns are kept.
datapath = c('/media/jjech/Mac Passport/BottomTrawlSurvey_TrawlData')
dta <- dir_ls(path = datapath) %>% 
  lapply(read_csv, col_types=cols(begindatetime=col_datetime("%d/%m/%Y-%H:%M:%S"))) %>% 
  bind_rows()

# filtering rows with missing metadata (begindatetime, beginlat, beginlon)
dta = dta %>% drop_na(begindatetime, beginlat, beginlon)

# making a list of taxa names from the column names
taxa_ls <- names(dta[8:ncol(dta)]) %>% 
  sort()

# converting taxa list to sentence case
taxa_sentence_case <- str_to_sentence(taxa_ls)

# remove unknowns and trash
unknws <- str_subset(taxa_sentence_case, pattern = "^Unknown")
trash<- c("Trash species in catch") 
#outliers <- map(trash, str_subset, string = taxa_sentence_case) %>% 
#  unlist()
species <- taxa_sentence_case[!taxa_sentence_case %in% c(unknws, trash)]

# code from the WoRMS website 
#An vector of names we wan't to match (can be one item only!)
#namesToMatch <- c("Solea soleo", "Abra Albo", "sdqkljflkfjqsmlfds")
namesToMatch <- species[1:10]

#Convert the namesToMatch to a valid REST-url
urlNamesPart <- ""
for (i in 1:length(namesToMatch)) {
  urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[i]);
}

#The url can contain special characters that need to be converted
urlNamesPart <- URLencode(urlNamesPart)

#The dyanmic build of the URL causes an obsolete '&' at the beginning of the string, so remove it
urlNamesPart <- substring(urlNamesPart, 2)

#Build the final REST-url
url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);

#Get the actual data from the URL
matches <- fromJSON(url)

#Handle the data (each requested name has an list of results)
for (i in 1:length(namesToMatch)) {
  cat("Doing: ", namesToMatch[i], '\n')
  #Get the results for the current index
  currentResultList = matches[[i]]
  
  if (length(currentResultList) <= 0) {
    numberOfResults = 0
  } else {
    #Get the number of list entries for the first column
    numberOfResults <- length(currentResultList[[1]])
  }
  
#  #Handle empty data due to no matches found
#  if (is.na(currentResultList[[1]][[1]])) {
#    numberOfResults <- 0
#  }
  print(sprintf("%d Result(s) found for %s", numberOfResults, namesToMatch[i]))
  if (numberOfResults > 0) {
    for (listentry in 1:numberOfResults) {
      print(sprintf("ID: %d, SCIENTIFICNAME: %s, MATCH_TYPE: %s",
                    currentResultList[["AphiaID"]][listentry],
                    currentResultList[["scientificname"]][listentry],
                    currentResultList[["match_type"]][listentry]
      ));
    }
  }
}



