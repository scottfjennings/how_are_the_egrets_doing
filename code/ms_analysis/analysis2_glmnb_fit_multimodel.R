
library(tidyverse)
library(MASS)
library(AICcmodavg)
library(birdnames)
library(here)


#source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")


#' Fit a set of negative binomial glm models on untransformed nest abundance for a species in a subregion
#'
#' @param zspp 4 letter code for species
#' @param zsubreg subregion code (use codes defined in "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/subregion_key.csv")
#'
#' @return a list with an element for each fitted model object
#'
#' @examples
#' spp_subreg_mods_glmnb <- map2(spp_subreg$species, spp_subreg$subregion, fit_multi_mods_glmbn)
#' names(spp_subreg_mods_glmnb) <- spp_subreg$spp.subreg
fit_multi_mods_glmbn <- function(zspp, zsubreg) {
  
  zdat <- trend_analysis_table %>% 
    filter(species == zspp, subregion == zsubreg) 
  
  
  zmods = list(
  "year2_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2) + wt.lag.subreg.rain),
  "year_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year + wt.lag.subreg.rain),
  #"lnyear_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994) + wt.lag.subreg.rain),
  
  "year2_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2) + poly(wt.lag.subreg.rain, 2)),
  "year_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year + poly(wt.lag.subreg.rain, 2)),
  #"lnyear_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994) + poly(wt.lag.subreg.rain, 2)),
  
  "year2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2)),
  "year" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year),
  #"lnyear" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994)),
  "rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ wt.lag.subreg.rain),
  "rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(wt.lag.subreg.rain, 2)),
  
  "intercept" = MASS::glm.nb(data = zdat, formula = tot.nests ~ 1)
  )
}


trend_analysis_table <- readRDS(here("data/trend_analysis_table"))

spp_subreg <- readRDS(here("data/spp_subreg")) %>%
  mutate(species = factor(species, levels = c("GREG", "GBHE", "SNEG", "BCNH"))) %>% 
  full_join(subreg_key) %>% 
  arrange(species, subreg.name) %>% 
  filter(spp.subreg != "SNEG_SUS")



spp_subreg_mods <- map2(spp_subreg$species, spp_subreg$subregion, fit_multi_mods_glmbn)
names(spp_subreg_mods) <- spp_subreg$spp.subreg


#' Fit a set of negative binomial glm models on untransformed nest abundance for a species in a subregion
#'
#' @param zspp 4 letter code for species
#' @param zsubreg subregion code (use codes defined in "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/subregion_key.csv")
#'
#' @return a list with an element for each fitted model object
#'
#' @examples
#' spp_subreg_mods_glmnb <- map2(spp_subreg$species, spp_subreg$subregion, fit_multi_mods_glmbn)
#' names(spp_subreg_mods_glmnb) <- spp_subreg$spp.subreg
fit_multi_mods_glmbn_simple <- function(zspp, zsubreg) {
  
  zdat <- trend_analysis_table %>% 
    filter(species == zspp, subregion == zsubreg) 
  
  
  zmods = list(
    "year2_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2) + wt.lag.subreg.rain),
    "year_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year + wt.lag.subreg.rain),
    #"lnyear_rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994) + wt.lag.subreg.rain),
    
    #"year2_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2) + poly(wt.lag.subreg.rain, 2)),
    "year_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year + poly(wt.lag.subreg.rain, 2)),
    #"lnyear_rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994) + poly(wt.lag.subreg.rain, 2)),
    
    "year2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(year, 2)),
    "year" = MASS::glm.nb(data = zdat, formula = tot.nests ~ year),
    #"lnyear" = MASS::glm.nb(data = zdat, formula = tot.nests ~ log(year - 1994)),
    "rain" = MASS::glm.nb(data = zdat, formula = tot.nests ~ wt.lag.subreg.rain),
    "rain2" = MASS::glm.nb(data = zdat, formula = tot.nests ~ poly(wt.lag.subreg.rain, 2)),
    
    "intercept" = MASS::glm.nb(data = zdat, formula = tot.nests ~ 1)
  )
}


SNEG_SUS <- fit_multi_mods_glmbn_simple("SNEG", "SUS")


spp_subreg_mods$SNEG_SUS <- SNEG_SUS


saveRDS(spp_subreg_mods, here("fitted_models/spp_subreg_mods"))


