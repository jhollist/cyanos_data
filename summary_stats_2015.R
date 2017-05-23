#basic stats from 2015
dat15 <- read.csv("data/data_clean_2015.csv")
state_num <- with(dat15, length(unique(paste(state))))
org_num <- with(dat15, length(unique(paste(org_id))))
waterbody_num <- with(dat15, length(unique(paste(org_id,waterbody_id))))
stations_num <- with(dat15, length(unique(paste(org_id,waterbody_id,station_id))))
samples_num <- with(dat15, length(unique(paste(org_id,waterbody_id,station_id,sample_id))))
