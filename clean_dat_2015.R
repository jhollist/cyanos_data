# Script for cleaning the 2015 data, adding in RI data, and prepping data for 
# 2016 entry.  Idea is that the cleaned 2015 data will be in the format for 2016

library(dplyr)
library(lubridate)
library(stringr)

# Merged data from Bryan
data15 <- read.csv("data/data_2015.csv",stringsAsFactors = FALSE,
                   na.strings = "")
#Set Date/Time
data15$sampleDateTime <- mdy_hms(gsub(" NA"," 00:00:00", 
                                        paste(data15$sampleDate,
                                 str_extract(data15$sampleTime,"\\s[0-9:0-9]+"))
                                 ))
data15$analysisDate <- mdy(data15$analysisDate)
#RI data
ri15_fld <- read.csv("data/ri_field_2015.csv",stringsAsFactors = FALSE)
ri15_lab <- read.csv("data/ri_lab_2015.csv",stringsAsFactors = FALSE)
#Remove Excess Columns and Rows
ri15_fld <- ri15_fld[1:209,1:28]
ri15_lab <- ri15_lab[1:1021,1:17]
#Force SampleID in ri15_fld to character
ri15_fld$sampleID <- as.character(ri15_fld$sampleID)
#Set Date Time
ri15_fld$sampleDate <- dmy(ri15_fld$sampleDate)
ri15_lab$sampleDate <- mdy(ri15_lab$sampleDate)
ri15_lab$analysisDate <- mdy(ri15_lab$analysisDate)
#Join
ri15 <- full_join(ri15_fld,ri15_lab)
#Combine Date time
x <- gsub(" NA"," 00:00 AM",paste(ri15$sampleDate,ri15$sampleTime))
x <- gsub("\\s$"," 00:00 AM",x)
ri15$sampleDateTime <- ymd_hm(x)
ri15 <- ri15 %>%
  select_(.dots = names(data15))
#Add in RI
data15 <- rbind(data15,ri15) 

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
#Yank - fieldCrew,photoSample,surfaceWaterCondition,weather,analysisID,
#       analystName,frozen,filtered (protocol doesn't allow for diff), sampleRep 
#       (this is same as duplicate - needs to be separate row in csv),
#       photosAnalysis
data15 <- data15 %>%
  select(-fieldCrew) %>%
  select(-photoSample) %>%
  select(-surfaceWaterCondition) %>%
  select(-weather) %>%
  select(-analysisID) %>%
  select(-analystName) %>%
  select(-frozen) %>%
  select(-filtered) %>%
  select(-sampleRep) %>%
  select(-photosAnalysis) %>%
  select(-sampleDate) %>%
  select(-sampleTime)

#dilution - need to figure out.  Is it standardized in the protocol? It should 
#           be.  Need to figure out how to capture in database to correct.
#           Maybe sample volume and final volume (eg. NLA)
#analysisRep - true replicate.  We should yank this and only keep the first,
#               non-quenched meausrement as subsequent measure of same tube would
#               be degraded.  Leave this in for now.  Change to numeric with all 
#               current NA as 1, Primary as 1, duplicate as 2

#Fix locations

idx <- which(data15$longitudeSta > 0)
should_be_lat <- data15$longitudeSta[idx]
should_be_long <-  data15$latitudeSta[idx]

data15$longitudeSta[idx] <- should_be_long 
data15$latitudeSta[idx] <- should_be_lat

idx <- which(data15$longitudeSta > 0)
data15$longitudeSta[idx] <- data15$longitudeSta[idx] * -1

#Convert analysisRep to a numeric:
data15$analysisRep[data15$analysisRep=="Primary"] <- "1"
data15$analysisRep[is.na(data15$analysisRep)] <- "1"
data15$analysisRep[data15$analysisRep==""] <- "1"

write.csv(data15,"data_2015.csv")