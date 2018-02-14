library(tidyverse)
library(here)

cmc <- read_csv(here("data/data_clean_2015.csv"), guess_max = 2819) 
ww <- read_csv(here("../green_murky_warm/data/ww_all.csv"), guess_max = 335230)

cmc_ri_avg_chla <- cmc %>% 
  select(org_id, waterbody_name, station_id, sample_date,
         dilution, chla_ugl) %>%
  filter(dilution == "1:1", org_id == "RIWW") %>%
  mutate(station_id = stringr::str_replace(station_id,pattern = "_[N|O]","")) %>%
  group_by(station_id, sample_date) %>%
  summarize(cmc_avg_chla = mean(chla_ugl)) %>%
  ungroup() %>%
  mutate(station_id = case_when(str_length(station_id) == 4 ~ str_replace(station_id, "WW", "WW0"),
                                TRUE ~ station_id),
         year = 2015)

ww_avg_chla <- ww %>%
  filter(Parameter == "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209",
         Depth <= 3) %>%
  group_by(`Station Name`, Date) %>%
  summarize(ww_avg_chla = mean(Concentration)) %>%
  mutate(station_id = `Station Name`,
         sample_date = lubridate::mdy(Date)) %>%
  ungroup() %>%
  select(station_id, sample_date, ww_avg_chla) %>%
  filter(lubridate::year(sample_date) == 2015) %>%
  mutate(station_id = case_when(str_length(station_id) == 4 ~ str_replace(station_id, "WW", "WW0"),
                                TRUE ~ station_id))

ww_cmc_avg_chla <- ww_avg_chla %>%
  left_join(cmc_ri_avg_chla)

ww_cmc_avg_chla %>%
  na.omit() %>%
  ggplot(aes(x = cmc_avg_chla, y = ww_avg_chla)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
    

cmc_14 <- read_csv("data/data_2014.csv")

cmc_14_ri_avg_chla <- cmc_14 %>%
  filter(Organization == "RIWW", Parameter == "Chlorophyll", Filtered == FALSE, Frozen == TRUE) %>%
  
