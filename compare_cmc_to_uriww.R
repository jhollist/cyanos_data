library(tidyverse)
library(here)

# Pull in 2014-2015 CMC data ----
# Limits data to those that were not filtered and frozen, per the QAPP
# Averages all readings per station and date so we get a single chlorohpyll
# estimate for location and day.

cmc_15 <- read_csv(here("data/data_clean_2015.csv"), guess_max = 2819)
cmc_14 <- read_csv(here("data/data_2014.csv"), guess_max = 8752)

cmc_15_ri_avg_chla <- cmc %>% 
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

cmc_14_ri_avg_chla <- cmc_14 %>%
  filter(Organization == "RIWW", Parameter == "Chlorophyll", Filtered == FALSE, Frozen == TRUE) %>%
  mutate(sample_date = lubridate::ymd(paste(SampleYear,SampleMonth,SampleDay, sep = "-"))) %>%
  group_by(SiteID, sample_date) %>%
  summarize(cmc_avg_chla = mean(Value)) %>%
  ungroup() %>%
  mutate(station_id = case_when(str_length(SiteID) == 4 ~ str_replace(SiteID, "WW", "WW0"),
                                TRUE ~ SiteID),
         year = 2014) %>%
  select(station_id, sample_date, cmc_avg_chla, year)  

cmc_ri_avg_chla <- cmc_15_ri_avg_chla %>%
  bind_rows(cmc_14_ri_avg_chla)

# Pull in URI Watershed Watch data ----
# Limit to 2014 and 2015 and averages measurements in the top 3 meters, but most
# at 1m.  Averages values per station and day.  This allows us to join to CMC.  

ww <- read_csv(here("../green_murky_warm/data/ww_all.csv"), guess_max = 335230)

ww_avg_chla <- ww %>%
  filter(Parameter == "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209",
         Depth <= 3) %>%
  group_by(`Station Name`, Date) %>%
  summarize(ww_avg_chla = mean(Concentration)) %>%
  mutate(station_id = `Station Name`,
         sample_date = lubridate::mdy(Date)) %>%
  ungroup() %>%
  select(station_id, sample_date, ww_avg_chla) %>%
  filter(lubridate::year(sample_date) == 2015 | lubridate::year(sample_date) == 2014) %>%
  mutate(station_id = case_when(str_length(station_id) == 4 ~ str_replace(station_id, "WW", "WW0"),
                                TRUE ~ station_id))

# Join both datasets ----
# retain only site/date combos that are in WW and match in CMC.

ww_cmc_avg_chla <- ww_avg_chla %>%
  left_join(cmc_ri_avg_chla)

# Plot data ----
ww_cmc_chla_gg <- ww_cmc_avg_chla %>%
  na.omit() %>%
  ggplot(aes(x = cmc_avg_chla, y = ww_avg_chla, color = as.character(year))) +
  geom_point() +
  labs(title = "Comparison of Chlorophyll Measurements",
       subtitle = "URI Watershed Watch Trilogy and CMC Beagle",
       x = "CMC Methods: Beagle Chlorophyll (ug/l)",
       y = "URI WW Methods: Trilogy Chlorophyll (ug/l)") +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_discrete("Year")
ww_cmc_chla_gg
ggsave("ww_cmc_chla_compare.jpg", ww_cmc_chla_gg, width = 5, height = 5, 
       units = "in", dpi = 300)
