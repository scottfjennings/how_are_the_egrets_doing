

library(tidyverse)
library(here)

start.year = 1995
end.year = 2019

source(here("code/hep_trend_utilities.r"))

source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb") %>% 
  select(parent.site.name, subregion, utmnorth, utmeast) %>%
  filter(!is.na(utmnorth)) %>% 
  full_join(sites_subreg <- read.csv(here("data/all_sfbbo_sites_subreg.csv")) %>% 
  dplyr::select(parent.site.name = site.name, subregion, Latitude, Longitude)%>% 
  filter(!is.na(Latitude)))  %>% 
  distinct()

write.csv(hep_sites, here("data/all_sites_coords_subreg.csv"), row.names = FALSE)
