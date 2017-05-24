library(tidyverse)
dat2015 <- read_csv("data/data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>%
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution != "1:16") %>%
  filter(dilution != "1:2") %>%
  filter(dilution != "1:4") %>%
  filter(dilution != "1:8")

dat2015_analysis <- dat2015 %>%
  mutate(unique_sample_id  = paste(state,waterbody_id,station_id,sample_id,sample_date)) %>%
  select(unique_sample_id, analysis_id, analysis_date, analysis_rep, chla_ugl, phyco_ugl) %>%
  arrange(unique_sample_id,analysis_date,analysis_id) %>%
  data.frame
         