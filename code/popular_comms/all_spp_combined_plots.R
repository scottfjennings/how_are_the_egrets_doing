

library(tidyverse)
library(here)
library(birdnames)


source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

hepdata_location = here("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion)

options(scipen = 999)


sfbbo_nests <- read.csv(here("data/CWB_peak_hep_nest_locs_through_2019.csv")) %>% 
  mutate(ColonyName = ifelse(ColonyName == "Lake Merced North" & Latitude == 37.728057, "Lake Merced Mesa", ColonyName)) %>% 
  dplyr::select("site.name" = ColonyName, "species" = SpeciesCode, "year" = SurveyYear, "peakactvnsts" = PeakNumberofNests) %>% 
  full_join(read.csv(here("data/all_sfbbo_sites_subreg.csv"))) %>% 
  filter(!site.name %in% c("Bacon Island", "Elmwood Correctional", "Sandy Wool Lake"))

analysis_table <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(hep_sites) %>%
  filter(site.name != "Brentwood") %>% 
  bind_rows(sfbbo_nests) %>% 
  filter(!species %in% c("DCCO", "CAEG"), peakactvnsts >= 0, !is.na(year), year > 1990) %>% 
  #cut_never_nested() %>% 
  dplyr::select(year, subregion, code, species, peakactvnsts)


spp_combined <- analysis_table %>% 
  group_by(year, subregion) %>% 
  summarise(total.nests = sum(peakactvnsts)) %>% 
  ungroup() %>%
  bind_rows(analysis_table %>%
              group_by(year) %>%
              summarise(total.nests = sum(peakactvnsts)) %>%
              ungroup() %>% 
              mutate(subregion = "All")) %>% 
  full_join(read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/subregion_key.csv")) %>% 
  mutate(subreg.name = factor(subreg.name),
         subreg.name = relevel(subreg.name, ref = "Entire study area"))

spp_combined %>%
  filter(year != 2020) %>% 
  ggplot() +
  geom_point(aes(x = year, y = total.nests)) +
  stat_smooth(aes(x = year, y = total.nests), span = .9) +
  geom_point(data = filter(spp_combined, year == 2020), aes(x = year, y = total.nests), color = "red") +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  labs(y = "Nest abundance",
       x = "Year",
       title = "All heron and egret species combined") +
  theme_bw() +
  theme(text = element_text(size=10))

ggsave(here("figures/all_spp_combined_subreg.png"), width = 8)



spp_combined %>%
  filter(subreg.name == "Entire study area", year != 2020) %>% 
  ggplot() +
  geom_point(aes(x = year, y = total.nests)) +
  stat_smooth(aes(x = year, y = total.nests), span = .9) +
  geom_point(data = filter(spp_combined, subreg.name == "Entire study area", year == 2020), aes(x = year, y = total.nests), color = "red") +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  labs(y = "Nest abundance",
       x = "Year",
       title = "All heron and egret species combined") +
  theme_bw() +
  theme(text = element_text(size=10))

ggsave(here("figures/all_spp_combined.png"), width = 8)

