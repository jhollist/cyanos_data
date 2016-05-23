#' Combine data
#'
#' Script for combining 2014 and 2015 New England Regional Cyanobacteria 
#' Monitoring Program data into a single, tidy, data set.
#' 
library(dplyr)
library(tidyr)

data14 <- read.csv("data/data_2014.csv",stringsAsFactors = FALSE,na.strings = "")
data15 <- read.csv("data/data_2015.csv",stringsAsFactors = FALSE,na.strings = "")

#2015- two forms didn't merge always.  Some observations with orgid but no lakes 
#and vice versa

#Simplifying
#Throw out commentWB,commentStation,commentSample,commentAnalysis - merge into 
#     single comment field
#StationID - some meaningful, some auto.
#Yank - sampleID,fieldCrew,photoSample,surfaceWaterCondition,weather,analysisID,
#       analystName,frozen,filtered (protocol doesn't allow for diff), sampleRep 
#       (this is same as duplicate - needs to be separate row in csv),
#       photosAnalysis
#dilution - need to figure out.  Is it standardized in the protocol? It should 
#           be.  Need to figure out how to capture in database to correct.
#           Maybe sample volume and final volume (eg. NLA)
#analysisRep - true replicate.  We should yank this and only keep the first,
#               non-quenched meausrement as subsequent measure of same tube would
#               be degraded.
#

#Data 14 - convert measurements to phyco and chla columns
data14_chla <- data14[data14$Parameter=="Chlorophyll",]
data14_phyco <- data14[data14$Parameter=="Phycocyanin",]
