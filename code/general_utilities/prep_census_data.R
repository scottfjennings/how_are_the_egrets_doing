


# code to compile census data around each hep colony


library(tidyverse)
library(lme4)
options(scipen = 999)


# accessed census tract shapefile here: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# used ArcGIS pro to overlay a 10 km buffer around each colony site over the census tracts.
# only needed tract id and colony code, so simply extracted attribute table to csv.
hep_census_tracts <- read.csv("data/hep_sites_10km_census_tracts.csv") %>% 
  filter(!is.na(code)) %>% 
  select(code, tract = TRACTCE)


tract_pop <- read.csv("data/Vital_Signs__Population___by_tract.csv") %>% 
  rename_all(tolower) %>% 
  mutate(tract = ifelse(tract == 400200 & geoid10 == 6001400100, 400100, tract)) %>% 
  filter(year >= 1990, tract %in% distinct(hep_census_tracts, tract)$tract) 

tract_pop %>% 
  select(tract, year, popdensity) %>% 
  ggplot(group = tract) +
  geom_line(aes(x = year, y = popdensity, group = tract))



tract_pop_90_18 <- tract_pop %>% 
  select(tract, year, popdensity) %>% 
  filter(year %in% c(1990, 2018), tract %in% distinct(hep_census_tracts, tract)$tract) %>% 
  arrange(tract, year)

tract_pop_90_18_change <- tract_pop_90_18 %>% 
  mutate(year = paste("yr_", year, sep = "")) %>% 
  pivot_wider(id_cols = tract, names_from = year, values_from = popdensity) %>% 
  mutate(pop_prop_change = yr_2018/yr_1990)

tract_pop_lms <- lmList(popdensity ~ year|tract, tract_pop)%>% 
  coef() %>% 
  data.frame() %>% 
  rownames_to_column("tract") %>% 
  select(tract, year) %>% 
  mutate(tract = as.numeric(tract))

full_join(tract_pop_90_18_change, tract_pop_lms) %>% 
  ggplot() +
  geom_point(aes(x = pop_prop_change, y = year))



hep_pop_change <- full_join(hep_census_tracts, tract_pop_90_18_change) %>% 
  arrange(code, tract)
