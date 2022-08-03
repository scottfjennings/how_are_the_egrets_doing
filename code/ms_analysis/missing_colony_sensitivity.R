
library(tidyverse)
library(tidymodels)
library(MASS)
library(birdnames)
library(here)


#source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")
source(here("code/hep_trend_utilities.r"))
source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

# prepare data ----
hepdata_location = here("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion)

options(scipen = 999)

start.year = 1995
end.year = 2019

sfbbo_nests <- read.csv(here("data/CWB_peak_hep_nest_locs_through_2019.csv")) %>% 
  mutate(ColonyName = ifelse(ColonyName == "Lake Merced North" & Latitude == 37.728057, "Lake Merced Mesa", ColonyName)) %>% 
  dplyr::select("site.name" = ColonyName, "species" = SpeciesCode, "year" = SurveyYear, "peakactvnsts" = PeakNumberofNests) %>% 
  full_join(read.csv(here("data/all_sfbbo_sites_subreg.csv"))) %>% 
  filter(site.name != "Elmwood Correctional")



# calculate a cumulative rain index following Stenzel and Page 2018
rain_lag <- readRDS(here("data/subreg_rain")) %>% 
  dplyr::select(year = birdyear, subregion, subreg.name, subreg.rain) %>%
  data.frame() %>% 
  arrange(subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(subreg.rain = subreg.rain + (lag(subreg.rain)/2) + (lag(subreg.rain, 2)/3))

analysis_table <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(hep_sites) %>%
  bind_rows(sfbbo_nests) %>% 
  filter(species %in% c("GREG", "GBHE"), peakactvnsts >= 0, !is.na(year), between(year, start.year, end.year)) %>% 
  #cut_never_nested() %>% 
  dplyr::select(year, subregion, site.name, species, peakactvnsts) %>% 
  full_join(rain_lag)


check_slice <- analysis_table %>% 
  filter(!is.na(peakactvnsts)) %>% 
  dplyr::select(subregion, site.name, species, year, peakactvnsts) %>% 
  slice_sample(prop = 0.95) %>% 
  distinct(subregion, site.name, species, year) %>%  
  mutate(sliced = TRUE) %>% 
  full_join(analysis_table %>%
              filter(!is.na(peakactvnsts)) %>% 
              distinct(subregion, site.name, species, year, peakactvnsts)) %>% 
  arrange(subregion, site.name, species, year)


test_missing_col_sensitivity <- function(analysis_table) {

  fit_mod <- function(ana_table) {
    zmod <- ana_table %>% 
      glm.nb(data = ., formula = tot.nests ~ poly(year, 2) + subreg.rain)
    
    znewdat <- data.frame(year = seq(start.year, end.year),
                                               subreg.rain = mean(ana_table$subreg.rain))
    
    zpred = predict(zmod, newdata = znewdat, se = TRUE) %>% 
      bind_cols(znewdat) %>% 
        mutate(estimate = exp(fit),
         lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)))
  }

zz <- analysis_table %>% 
  filter(!is.na(peakactvnsts)) %>% 
  dplyr::select(subregion, site.name, species, year, peakactvnsts) %>% 
  slice_sample(prop = 0.95) %>% 
  group_by(subregion, species, year) %>%
  summarise(tot.nests = sum(peakactvnsts)) %>%
  ungroup() 
  
  xx <- zz %>%
  filter(tot.nests > 0, between(year, start.year, end.year)) %>%
  group_by(subregion, species) %>% 
  summarise(num.years = n()) %>%
    filter(num.years > 5) %>% 
  left_join(zz) %>% 
    left_join(rain_lag) %>% 
  dplyr::select(-num.years) %>% 
  group_by(subregion, species) %>%
  nest() %>% 
   mutate(
     zpreds = data %>% map(fit_mod)
   ) %>% 
    unnest(cols = c(zpreds)) %>% 
    dplyr::select(subregion, species, year, estimate)
}

zoof <- test_missing_col_sensitivity(analysis_table)

fooz <- replicate(1000, test_missing_col_sensitivity(analysis_table))

rep_to_df <- function(z.inds){
gooz <- data.frame(fooz[z.inds]) %>% 
  rename(subregion = 1,
         species = 2,
         year = 3,
         estimate = 4) %>% 
  mutate(n.rep = z.inds[[1]][[1]])

}
d = 1:4000
z_inds <- split(d, ceiling(seq_along(d)/4))
bootz <- map_df(z_inds, rep_to_df)


bootz_out <- bootz %>% 
  group_by(subregion, species, year) %>% 
  summarise(mean.est = mean(estimate),
            lci = quantile(estimate, 0.025),
            uci = quantile(estimate, 0.975)) %>% 
  ungroup()


saveRDS(bootz_out, here("output/sensitivity_bootz"))


bootz_out %>% 
  filter(species == "GREG") %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean.est)) +
  geom_ribbon(aes(x = year, ymin = lci, ymax = uci), alpha = 0.25) +
  facet_wrap(~subregion, scales = "free_y")