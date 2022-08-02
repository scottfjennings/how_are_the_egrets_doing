


# combine hep data with predictors

library(tidyverse)
library(devtools)
library(birdnames)

source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")



# read data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hep_abund <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance")

# rainfall data for each colony, from PRISM

hep_prism <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_prism_data/hep_prism_combined")


# calculate the cumulative annual rainfall from march of the previous year through feb of the current year
# use of this rainfall value in a model will assume that the rainfall effect on current year nest abundance acts on productivity the previous breeding season and on survival of adults and juv from previous breeding season through onset of current breeding season
birdyear_rain <- hep_prism %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(birdyear = ifelse(month <= 2, year, year + 1)) %>% 
  group_by(parent.code, birdyear) %>% 
  summarise(birdyear.rain = sum(rain.mm)) %>% 
  ungroup()

# combine data ----

hep_pop_birdyear_rain <- hep_abund %>% 
  filter(peakactvnsts >= 0) %>% 
  separate(code, into = "parent.code", sep = "\\.", remove = FALSE) %>% 
  mutate(parent.code = as.numeric(parent.code)) %>% 
  group_by(parent.code, year, species) %>% 
  summarise(nest.abund = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  full_join(., rename(birdyear_rain, year = birdyear))


  
  
  
  