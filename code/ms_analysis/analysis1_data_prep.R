


library(tidyverse)
library(birdnames)
library(here)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")


# prepare data ----
hepdata_location = here("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion)

options(scipen = 999)

start.year = 1995
end.year = 2019

#temp


sfbbo_sites <- read.csv(here("data/all_sfbbo_sites_subreg.csv")) %>% 
  distinct(site.name) %>% 
  mutate(code = row_number() + 900) %>% 
  full_join(read.csv(here("data/all_sfbbo_sites_subreg.csv")))

sfbbo_nests <- read.csv(here("data/CWB_peak_hep_nest_locs_through_2019.csv")) %>% 
  mutate(ColonyName = ifelse(ColonyName == "Lake Merced North" & Latitude == 37.728057, "Lake Merced Mesa", ColonyName)) %>% 
  dplyr::select("site.name" = ColonyName, "species" = SpeciesCode, "year" = SurveyYear, "peakactvnsts" = PeakNumberofNests) %>% 
  full_join(sfbbo_sites) %>% 
  filter(!site.name %in% c("Bacon Island", "Elmwood Correctional", "Sandy Wool Lake"))

analysis_table <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(hep_sites) %>%
  filter(site.name != "Brentwood") %>% 
  bind_rows(sfbbo_nests) %>% 
  filter(!species %in% c("DCCO", "CAEG"), peakactvnsts >= 0, !is.na(year), between(year, start.year, end.year)) %>% 
  #cut_never_nested() %>% 
  dplyr::select(year, subregion, code, species, peakactvnsts) 

saveRDS(analysis_table, here("data/acr_sfbbo_combined"))

analysis_table <- readRDS(here("data/acr_sfbbo_combined"))


# calculate a cumulative rain index following Stenzel and Page 2018
rain_lag <- readRDS(here("data/subreg_rain")) %>% 
  dplyr::select(year = birdyear, subregion, subreg.name, subreg.rain) %>%
  data.frame() %>% 
  arrange(subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(lag1.rain = lag(subreg.rain),
         lag2.rain = lag(subreg.rain, 2),
         wt.lag.subreg.rain = subreg.rain + (lag1.rain/2) + (lag2.rain/3),
         tot.rain.3yr = (subreg.rain + lag1.rain + lag2.rain),
         mean.rain.3yr = (subreg.rain + lag1.rain + lag2.rain)/3) %>% 
  ungroup()

rain_lag %>% 
  select(subreg.rain, wt.lag.subreg.rain, tot.rain.3yr, mean.rain.3yr) %>% 
  plot()


rain_lag %>% 
  filter(between(year, start.year, end.year)) %>% 
  group_by(subregion) %>% 
  summarise(mean.subreg.rain = mean(wt.lag.subreg.rain)) %>% 
  saveRDS(here("data/subreg_mean_rain_lag"))


trend_analysis_table1 <- analysis_table  %>%
  group_by(year, subregion, species) %>% 
  summarise(tot.nests = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  bind_rows(analysis_table %>% # add duplicate of all data with subregion set to "All" 
              group_by(year, species) %>%
              summarise(tot.nests = sum(peakactvnsts)) %>%
              ungroup() %>%
              mutate(subregion = "All")) %>% 
  full_join(expand.grid(species = distinct(analysis_table, species)$species,
                        year = seq(start.year, end.year, by = 1),
                        subregion = subreg_key$subregion)) %>% 
  mutate(tot.nests = ifelse(is.na(tot.nests), 0, tot.nests))


start_end_years <- trend_analysis_table1 %>% 
  filter(tot.nests > 0) %>% 
  group_by(subregion, species) %>% 
  summarise(spp.sub.start.year = min(year) - 1,
            spp.sub.end.year = max(year) + 1) %>% 
  ungroup() %>% 
  mutate(spp.sub.start.year = ifelse(spp.sub.start.year < start.year, start.year, spp.sub.start.year),
         spp.sub.end.year = ifelse(spp.sub.end.year > end.year, end.year, spp.sub.end.year))

spp_subreg_nyears <- trend_analysis_table1 %>%
  filter(tot.nests > 0) %>% 
  group_by(subregion, species) %>% 
  summarise(nyears = n())


# number of colonies and colony size by species
n_colonies_spp <- analysis_table %>%
  mutate(subregion = "All") %>% 
  bind_rows(analysis_table) %>% 
  #  distinct(subregion, year, code, species) %>% 
  group_by(subregion, species, year) %>% 
  summarise(n.colonies = n(),
            tot.nests = sum(peakactvnsts),
            mean.col.size = mean(peakactvnsts),
            sd.col.size = sd(peakactvnsts)) %>% 
  ungroup() %>% 
  left_join(subreg_key) %>% 
  mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"))




# for mapping through species and subregions
spp_subreg <- trend_analysis_table1 %>% 
  right_join(spp_subreg_nyears %>% filter(nyears >= 5) %>% dplyr::select(-nyears)) %>% 
  distinct(species, subregion) %>% 
  mutate(spp.subreg = paste(species, subregion, sep = "_"))
saveRDS(spp_subreg, here("data/spp_subreg"))



trend_analysis_table <- trend_analysis_table1 %>% 
  full_join(start_end_years) %>% 
  mutate(zkeep = ifelse(year >= spp.sub.start.year & year <= spp.sub.end.year, TRUE, FALSE)) %>% 
  arrange(subregion, species, year) %>% 
  filter(zkeep == TRUE) %>% 
  dplyr::select(-zkeep) %>% 
  left_join(rain_lag) %>% 
  left_join(n_colonies_spp %>% select(subregion, species, year, n.colonies, mean.col.size))

saveRDS(trend_analysis_table, here("data/trend_analysis_table"))
