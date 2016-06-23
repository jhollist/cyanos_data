library(dplyr)
dat15 <- read.csv("data/data_2015.csv")
ri15 <- read.csv("data/ri_field_2015.csv")
loc_all <- dat15 %>% 
  select(waterbodyID, waterbodyName, longitudeSta, latitudeSta,orgID,state)

loc_ri <- ri15 %>%
  select(waterbodyID, waterbodyName, longitudeSta, latitudeSta,orgID,state)
  
loc_all <- na.omit(rbind(loc_all,loc_ri))

loc_all_u <- unique(rbind(loc_all,loc_ri))

idx <- which(loc_all_u$longitudeSta > 0)
should_be_lat <- loc_all_u$longitudeSta[idx] 
should_be_long <-  loc_all_u$latitudeSta[idx]

loc_all_u$longitudeSta[idx] <- should_be_long 
loc_all_u$latitudeSta[idx] <- should_be_lat

idx <- which(loc_all_u$longitudeSta > 0)
loc_all_u$longitudeSta[idx] <- loc_all_u$longitudeSta[idx] * -1

plot(loc_all_u$longitudeSta,loc_all_u$latitudeSta)

write.csv(loc_all_u,"locations_2015.csv",row.names = F)
