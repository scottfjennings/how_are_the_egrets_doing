


# this is probably useless


library(tidyverse)

options(scipen = 999)

annual_change <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/colony_changes_bycode")


annual_change <- annual_change %>% 
  filter(species %in% c("BCNH", "GBHE", "GREG", "SNEG")) %>% 
  group_by(year, species) %>% 
  mutate(max.prev.abund = max(prev.yr.nsts, na.rm = TRUE),
         tot.prev.abund = sum(prev.yr.nsts, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(col.wt.max = prev.yr.nsts/max.prev.abund,
         weighted.change.max = per.change * col.wt.max,
         col.wt.prop = prev.yr.nsts/tot.prev.abund,
         weighted.change.prop = per.change * col.wt.prop)

ggplot(annual_change) + 
  geom_point(aes(x = peakactvnsts, y = per.change)) + 
  geom_point(aes(x = peakactvnsts, y = weighted.change.prop), color = "red") + 
  facet_wrap(~species)


annual_change_long <- annual_change %>% 
  select(year, code, species, peakactvnsts, prev.yr.nsts, per.change, contains("weighted.change")) %>% 
  pivot_longer(cols = contains("weighted.change"))

ggplot(annual_change_long) +
  geom_point(aes(x = per.change, y = value, size = prev.yr.nsts, color = name)) +
  facet_wrap(~species, scales = "free")


ggplot(annual_change) +
  geom_point(aes(x = per.change, y = value, size = prev.yr.nsts, color = name)) +
  facet_wrap(~species, scales = "free")

annual_change %>% 
  group_by(year, species) %>% 
  summarise(mean.per.change = mean(per.change, na.rm = TRUE),
            mean.weighted.change = mean(weighted.change.prop, na.rm = TRUE),
            n.colonies = n()) %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean.per.change), color = "red") +
  geom_line(aes(x = year, y = mean.weighted.change)) +
  facet_wrap(~species)
  



abund <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance")


new_col_per_year <- abund %>% 
  distinct(year, code, site.name) %>% 
  group_by(code) %>% 
  filter(year == min(year)) %>% 
  ungroup() %>% 
  arrange(year) 

new_col_per_year %>% 
  count(year) %>% view()

