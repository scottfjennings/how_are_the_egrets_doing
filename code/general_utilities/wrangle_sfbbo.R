

library(tidyverse)
library(here)
library(RODBC)


# this chunk was used to help assign subregion to each colony ----
# data/all_sfbbo_sites_subreg.csv has been hand edited in excel to have no missing subregions

#con2 <- odbcConnectAccess2007("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
#out_table <- sqlFetch(con2, "tbl_SFBBOSites_2007_03_29DL") 
#close(con2)

# sfbbo_sites <- read.csv(here("data/CWB_peak_hep_nest_locs_through_2019.csv")) %>% 
#  distinct(ColonyName, Latitude, Longitude) %>% 
#  filter(!is.na(Latitude))

# sfbbo_sites_subreg <- sfbbo_sites %>% 
#  left_join(out_table %>% distinct(SITENAME, HEPCODES_SubReg) %>% rename("ColonyName" = SITENAME)) %>% 
#  arrange(ColonyName)

# write.csv(sfbbo_sites_subreg, here("data/sfbbo_sites_subreg.csv"), row.names = FALSE)

# sfbbo_sites_subreg %>% 
#  filter(is.na(HEPCODES_SubReg)) %>%
#  write.csv(here("data/sfbbo_sites_nosubreg.csv"), row.names = FALSE)

# hand edit "parent.site.name"
# then read back in to generate site.code and parent.site.code if needed (currently not)
# sfbbo_all_sites <- read.csv(here("data/all_sfbbo_sites_subreg.csv")) %>% arrange(site.name) %>% mutate(code = row_number(), code = round(code, 1))

# sfbbo_all_parent_sites <- sfbbo_all_sites %>% arrange(site.name) %>% mutate(parent.code = as.integer(factor(parent.site.name))) %>%  group_by(parent.code) %>%  mutate(code = paste(parent.code, row_number(), sep = "."))



# get nest data ----
# peel off just nest, year, colony name,
# field names to match 

sfbbo_nests <- read.csv(here("data/CWB_peak_hep_nest_locs_through_2019.csv")) %>% 
  mutate(ColonyName = ifelse(ColonyName == "Lake Merced North" & Latitude == 37.728057, "Lake Merced Mesa", ColonyName)) %>% 
  select("site.name" = ColonyName, "species" = SpeciesCode, "year" = SurveyYear, "peakactvnsts" = PeakNumberofNests) %>% 
  full_join(read.csv(here("data/all_sfbbo_sites_subreg.csv")))
  

filter(sfbbo_nests, grepl("Lake Merced", site.name) | grepl("Lakeshore Park\\, Newark", site.name) | grepl("Redwood City Harbor", site.name)) %>% 
  pivot_wider(id_cols = c("site.name", "year"), values_from = peakactvnsts, names_from = species) %>% 
  arrange(site.name, year) %>% 
  view()



# basic data visualization
sfbbo %>% 
 # count(ColonyName, SpeciesCode, SurveyYear) %>% 
  pivot_wider(id_cols = c("ColonyName", "SurveyYear"), values_from = PeakNumberofNests, names_from = SpeciesCode) %>% 
  view()

read.csv(here("data/all_sfbbo_sites_subreg.csv")) %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, color = subregion))








