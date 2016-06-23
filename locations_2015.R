library(dplyr)
dat15 <- read.csv("data/data_2015.csv")
ri15_fld <- read.csv("data/ri_field_2015.csv")[,1:28]
ri15_fld$sampleID <- factor(ri15_fld$sampleID)
ri15_lab <- read.csv("data/ri_lab_2015.csv")[,1:17]
ri15_lab$sampleID <- factor(ri15_lab$sampleID)
ri15_d <- full_join(ri15_fld,ri15_lab) %>%
  select_(.dots = names(dat15))
dat15_all <- rbind(dat15,ri15_d)
dat15_all <- dat15_all[complete.cases(dat15_all[c("ChlaUGL","PhycoUGL")]),]

loc_all <- dat15 %>% 
  select(waterbodyID, waterbodyName, longitudeSta, latitudeSta,orgID,state,
         sampleID,sampleDate,sampleRep)

loc_ri <- ri15 %>%
  select(waterbodyID, waterbodyName, longitudeSta, latitudeSta,orgID,state,
         sampleID,sampleDate,sampleRep)
  
loc_all <- rbind(loc_all,loc_ri)
loc_all <- loc_all[complete.cases(cbind(loc_all$longitudeSta,loc_all$latitudeSta)),]

loc_all_u <- unique(loc_all)

idx <- which(loc_all_u$longitudeSta > 0)
should_be_lat <- loc_all_u$longitudeSta[idx] 
should_be_long <-  loc_all_u$latitudeSta[idx]

loc_all_u$longitudeSta[idx] <- should_be_long 
loc_all_u$latitudeSta[idx] <- should_be_lat

idx <- which(loc_all_u$longitudeSta > 0)
loc_all_u$longitudeSta[idx] <- loc_all_u$longitudeSta[idx] * -1

plot(loc_all_u$longitudeSta,loc_all_u$latitudeSta)

write.csv(loc_all_u,"locations_2015.csv",row.names = F)

num_orgs <- length(unique(loc_all_u$orgID))
num_wbd <- length(unique(loc_all_u$waterbodyName))
num_locs <- dim(unique(loc_all_u[,c("longitudeSta","latitudeSta")]))[1]
num_samples <- dim(unique(loc_all_u[,c("sampleID","sampleDate","sampleRep")]))[1]
