################################################################################
# Script for cleaning the 2015 data, adding in RI data, and prepping data for 
# 2016 entry.  Idea is that the cleaned 2015 data will be in the format for 2016
# authors: JWH and WBM
# July 2016
################################################################################

################################################################################
#Required packages
library(dplyr)
library(lubridate)
library(stringr)
################################################################################

################################################################################
# Read In Merged data from Bryan
data15 <- read.csv("data/data_2015.csv",stringsAsFactors = FALSE,
                   na.strings = "")

################################################################################

################################################################################
#Set Date/Time
data15$sampleDateTime <- mdy_hms(gsub(" NA"," 00:00:00", 
                                        paste(data15$sampleDate,
                                 str_extract(data15$sampleTime,"\\s[0-9:0-9]+"))
                                 ))
data15$analysisDate <- mdy(data15$analysisDate)
data15$uniqueID <- with(data15,paste(waterbodyID,stationID,sampleID,sampleDate,
                                 analysisDate,analysisID,sep="-"))
################################################################################

################################################################################
#Process and Add RI data
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
ri15$uniqueID <- with(ri15,paste(waterbodyID,stationID,sampleID,sampleDate,
                                 analysisDate,analysisID,sep="-"))
#Remove Standards results
ri15 <- ri15 %>%
  filter(waterbodyID != "")
  

#Combine Date time
x <- gsub(" NA"," 00:00 AM",paste(ri15$sampleDate,ri15$sampleTime))
x <- gsub("\\s$"," 00:00 AM",x)
ri15$sampleDateTime <- ymd_hm(x)
ri15 <- ri15 %>%
  select_(.dots = names(data15))

#Add in RI
data15 <- rbind(data15,ri15) 
################################################################################


################################################################################
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
################################################################################


################################################################################
#Yank - fieldCrew,photoSample,surfaceWaterCondition,weather,
#       analystName,frozen,filtered (protocol doesn't allow for diff), sampleRep 
#       (this is same as duplicate - needs to be separate row in csv),
#       photosAnalysis, sampleDate (converted above), sampleTime (converted above)

data15 <- data15 %>%
  select(-fieldCrew) %>%
  select(-photoSample) %>%
  select(-surfaceWaterCondition) %>%
  select(-weather) %>%
  select(-analystName) %>%
  select(-frozen) %>%
  select(-filtered) %>%
  select(-sampleRep) %>%
  select(-photosAnalysis) %>%
  select(-sampleDate) %>%
  select(-sampleTime)
################################################################################


################################################################################
#dilution - need to figure out.  Is it standardized in the protocol? It should 
#           be.  Need to figure out how to capture in database to correct.
#           Maybe sample volume and final volume (eg. NLA)
################################################################################

################################################################################
#Fix locations

idx <- which(data15$longitudeSta > 0)
should_be_lat <- data15$longitudeSta[idx]
should_be_long <-  data15$latitudeSta[idx]

data15$longitudeSta[idx] <- should_be_long 
data15$latitudeSta[idx] <- should_be_lat

idx <- which(data15$longitudeSta > 0)
data15$longitudeSta[idx] <- data15$longitudeSta[idx] * -1
################################################################################


################################################################################
#analysisRep - true replicate.  We should yank this and only keep the first,
#               non-quenched meausrement as subsequent measure of same tube would
#               be degraded.  Leave this in for now.  Change to numeric with all 
#               current NA as 1, Primary as 1, duplicate as 2
# JWH opted to keep in but converted

#Convert analysisRep to a numeric:
data15$analysisRep[data15$analysisRep=="Primary"] <- "1"
data15$analysisRep[is.na(data15$analysisRep)] <- "1"
data15$analysisRep[data15$analysisRep==""] <- "1"
#Adds numeric for which replicate - code assumes rows are ordered correctly...
while("Duplicate" %in% data15$analysisRep){
  idx <- which(data15$analysisRep == "Duplicate")
  num <- as.character(max(as.numeric(data15$analysisRep[-idx])))
    for(i in idx){
    if(data15$analysisRep[i-1]==num){
      data15$analysisRep[i] <- as.numeric(num) + 1
    }
  }
}
data15$analysisRep <- as.numeric(data15$analysisRep)
################################################################################


################################################################################
#Add comment on non-unique records
data15$comments[data15$uniqueID%in%data15$uniqueID[duplicated(data15$uniqueID)]] <-
  paste(data15$comments[data15$uniqueID%in%data15$uniqueID[duplicated(data15$uniqueID)]],
        "; Non-unique ID")
################################################################################


################################################################################
#Clean up missing orgID and Contact
idx <- which(is.na(data15$orgID))
#Most have a waterbodyID
wbid <- unique(data15[idx,"waterbodyID"]) 
#[1] "MA72-08"              "NHLAK600020802-04-01" "NHLAK700060401-02-02"
#[4] "NHLAK700010804-02-01" "RI0008039L-01"        "RI0008039L-14"       
#[7] "RI0008039L-15"        ""

for(i in wbid){
  if(i != ""){
    org <- unique(data15$orgID[!is.na(data15$orgID) & data15$waterbodyID == i])
    name <- unique(data15$ContactName[!is.na(data15$ContactName) & data15$waterbodyID == i])
    email <- unique(data15$Email[!is.na(data15$Email) & data15$waterbodyID == i])
    phone <- unique(data15$Phone[!is.na(data15$Phone) & data15$waterbodyID == i])
    comment <- paste0(data15$comments[is.na(data15$orgID) & 
                                          data15$waterbodyID==i], 
                        "Org contact: assumed was same as other ", i, ";")
    data15$comments[is.na(data15$orgID) & data15$waterbodyID == i] <- comment
    data15$orgID[is.na(data15$orgID) & data15$waterbodyID == i] <- org
    data15$ContactName[is.na(data15$ContactName) & data15$waterbodyID == i] <- name
    data15$Email[is.na(data15$Email) & data15$waterbodyID == i] <- email
    data15$Phone[is.na(data15$Phone) & data15$waterbodyID == i] <- phone
  }
}

################################################################################


################################################################################
# If both Chla and Phyco not present, why in database???
# Removed those.
data15 <- data15 %>%
  filter(!(is.na(PhycoUGL) & is.na(ChlaUGL)))
################################################################################



################################################################################
#Re-naming columns, becuase I can
cols <- c("org_id","contact_name","email","phone","waterbody_id",
          "waterbody_name","state","town","station_id","station_description",
          "station_type","station_longitude","station_latitude",
          "station_location_source","sample_id","sample_method",
          "sample_depth_m","water_temp_c","analysis_id",
          "analysis_date","dilution","sample_temp_c","chla_ugl","phyco_ugl",
          "analysis_rep","fluorometer_type","sample_data_time","unique_id",
          "comments")
names(data15) <- cols
################################################################################

################################################################################
# Clean up missing waterbody info if 1:1 with other IDs in dataset
# Missing waterbody info.  All observations have a waterbody_id

wbid <- unique(data15$waterbody_id[is.na(data15$waterbody_name)])
for(i in wbid){
  wbname <- unique(data15$waterbody_name[!is.na(data15$waterbody_name) & 
                                 data15$waterbody_id==i])
  comment <- paste0(data15$comments[is.na(data15$waterbody_name) & 
                                               data15$waterbody_id==i], 
                      "water body name: assumed was same as other ",
                      i, ";")

  #reset only if a single name
  if(length(wbname)==1){
    data15$comments[is.na(data15$waterbody_name) & data15$waterbody_id == i] <- comment
    data15$waterbody_name[is.na(data15$waterbody_name) & 
                            data15$waterbody_id==i] <- wbname
  }
}

wbid <- unique(data15$waterbody_id[is.na(data15$state)])
for(i in wbid){
  wbst <- unique(data15$state[!is.na(data15$state) & 
                                           data15$waterbody_id==i])
  comment <- paste0(data15$comments[is.na(data15$state) & 
                                      data15$waterbody_id==i], 
                    "water body state: assumed was same as other ",
                    i, ";")

  #reset only if a single name
  if(length(wbst)==1){
    data15$comments[is.na(data15$state) & data15$waterbody_id == i] <- comment
    data15$state[is.na(data15$state) & 
                            data15$waterbody_id==i] <- wbst
  }
}
################################################################################

################################################################################
# Fix NAs in town with 1:1

wbid <- unique(data15$waterbody_id[is.na(data15$town)])
for(i in wbid){
  wbtwn <- unique(data15$town[!is.na(data15$town) & 
                                data15$waterbody_id==i])
  comment <- paste0(data15$comments[is.na(data15$state) & 
                                      data15$waterbody_id==i], 
                    "water body town: assumed was same as other ",
                    i, ";")
  #reset only if a single name
  if(length(wbtwn)==1){
    data15$comments[is.na(data15$town) & data15$waterbody_id == i] <- comment
    data15$town[is.na(data15$town) &data15$waterbody_id==i] <- wbtwn
  }
}
################################################################################

################################################################################
# Flag all rows with remaining NA (i.e. prob shouldn't assume for other obs)
# Flag based on first 17 columns (i.e. up throuh water temp) but not station
# description.
data15$org_check_na <- apply(is.na(data15[,1:17][,-10]),1,sum)>0
table(data15$org_id[data15$org_check_na])
################################################################################

################################################################################
#Redo unique_id with other IDs
data15 <- data15 %>%
  mutate(unique_id = paste(org_id,waterbody_id,station_id,sample_id,analysis_id,
                           analysis_rep,sep="-"))

################################################################################

################################################################################
#Write final dataset out to csv
write.csv(data15,"data/data_clean_2015.csv",row.names = FALSE)
################################################################################