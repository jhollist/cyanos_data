library(tidyverse)
dat2015 <- read_csv("data/data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>% # does these det lim filters make sense
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24) %>%
  mutate(unique_sample_id  = paste(state,waterbody_id,station_id,sample_id,sample_date))
    
  

dat2015_analysis_tally <- dat2015 %>%
  group_by(unique_sample_id) %>%
  tally() %>% 
  arrange(desc(n))

dat2015 <- dat2015 %>%
  left_join(dat2015_analysis_tally)

# Lists the problems
probs <- dat2015 %>%
  filter(n > 1) %>%
  select(unique_sample_id, analysis_id, analysis_date, analysis_rep, chla_ugl, phyco_ugl,comments) %>%
  arrange(unique_sample_id) %>%
  data.frame

# Issue is we still have some samples with multiple analysese recorded as 
# primary...  Most have differing analysis_id, but which one to choose?  Do a 
# mean?  Not if these were multiple readings of same cuvette as sample would
# quench and readings would change?  
         