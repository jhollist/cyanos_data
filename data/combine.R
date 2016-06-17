#' Combine data
#'
#' Script for combining 2014 and 2015 New England Regional Cyanobacteria 
#' Monitoring Program data into a single, tidy, data set.
#' As of 2016-05-23, no longer trying to merge.  Focus on cleaning up 
#' 2015 format and getting ready for 2016
library(dplyr)
library(tidyr)

#data14 <- read.csv("data/data_2014.csv",stringsAsFactors = FALSE,na.strings = "")
data15 <- read.csv("data/data_2015.csv",stringsAsFactors = FALSE,na.strings = "")

#2015- two forms didn't merge always.  Some observations with orgid but no lakes 
#and vice versa

#Simplifying
#Throw out commentWB,commentStation,commentSample,commentAnalysis - merge into 
#     single comment field
comment <- vector("character",nrow(data15))
for(i in 1:nrow(data15)){
  comment[i] <- ""
  for(j in names(data15)[grep("comment",names(data15))]){
    if(!is.na(data15[i,][[j]])){
      comment[i] <- paste0(comment[i],j,": ",data15[i,][[j]],"; ") 
    }
  } 
}
data15$comments <- comment
data15$comments[data15$comments == ""] <- NA
data15$comments <- substr(data15$comments,
                          start = 1,
                          stop = nchar(data15$comments) - 1 )
data15 <- data15 %>% 
  select(-commentWB) %>%
  select(-commentStation) %>%
  select(-commentSample) %>%
  select(-commentAnalysis)

#StationID - some meaningful, some auto.
#Yank - sampleID,fieldCrew,photoSample,surfaceWaterCondition,weather,analysisID,
#       analystName,frozen,filtered (protocol doesn't allow for diff), sampleRep 
#       (this is same as duplicate - needs to be separate row in csv),
#       photosAnalysis
data15 <- data15 %>%
  select(-sampleID) %>%
  select(-fieldCrew) %>%
  select(-photoSample) %>%
  select(-surfaceWaterCondition) %>%
  select(-weather) %>%
  select(-analysisID) %>%
  select(-analystName) %>%
  select(-frozen) %>%
  select(-filtered) %>%
  select(-sampleRep) %>%
  select(-photosAnalysis)

#dilution - need to figure out.  Is it standardized in the protocol? It should 
#           be.  Need to figure out how to capture in database to correct.
#           Maybe sample volume and final volume (eg. NLA)
#analysisRep - true replicate.  We should yank this and only keep the first,
#               non-quenched meausrement as subsequent measure of same tube would
#               be degraded.

