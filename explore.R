library(tidyverse)
dat2015 <- read_csv("data/data_clean_2015.csv") %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>%
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution != "1:16") %>%
  filter(dilution != "1:2") %>%
  filter(dilution != "1:4") %>%
  filter(dilution != "1:8") %>%
  select(state, waterbody_id, unique_id, chla_ugl, phyco_ugl, sample_date) %>%
  mutate(log_chla = log(chla_ugl), log_phyco = log(phyco_ugl))
  
max_state <- dat2015 %>%
  group_by(state) %>%
  summarize(max_log_chla = max(log_chla), max_log_phyco = max(log_phyco))
max_state

plot(dat2015$log_chla, dat2015$log_phyco)

hist_gg <- dat2015 %>%
  gather(variable, value, 7:8) %>%
  filter(variable == "log_phyco") %>%
  ggplot(aes(x = value, colour = state)) +
  geom_density()
hist_gg
