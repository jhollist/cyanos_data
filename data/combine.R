#' Combine data
#'
#' Script for combining 2014 and 2015 New England Regional Cyanobacteria 
#' Monitoring Program data into a single, tidy, data set.
#' 
library(dplyr)
library(tidyr)

data14 <- read.csv("data/data_2014.csv",stringsAsFactors = FALSE,na.strings = "")
data15 <- read.csv("data/data_2015.csv",stringsAsFactors = FALSE,na.strings = "")

#Data 14 - convert measurements to phyco and chla columns
data14_chla <- data14[data14$Parameter=="Chlorophyll",]
data14_phyco <- data14[data14$Parameter=="Phycocyanin",]
