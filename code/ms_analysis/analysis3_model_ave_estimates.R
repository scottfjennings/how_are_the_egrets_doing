




library(tidyverse)
library(MASS)
library(AICcmodavg)
library(birdnames)
library(here)


#source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

spp_subreg <- readRDS(here("data/spp_subreg")) %>%
  mutate(species = factor(species, levels = c("GREG", "GBHE", "SNEG", "BCNH"))) %>% 
  full_join(subreg_key) %>% 
  arrange(species, subreg.name)






#' mod_predictions_link
#' 
#' calculate model estimates (predictions) for a species and subregion on the scale of the link function
#'
#' @param zspp_subreg 4 letter species code pasted to subregion code, e.g. GREG_All
#'
#' @return data frame
#' @export
#'
#' @examples
mod_avg_predictions <- function(zspp_subreg) {
  
  spp_subreg <- data.frame(spp.subreg = zspp_subreg)%>%
    separate(spp.subreg, c("spp", "subregion"), sep = "_") %>% 
    left_join(subreg_key) %>% 
    mutate(species = spp,
           spp = translate_bird_names(species, "alpha.code", "common.name"),
           spp.subreg.out = paste(spp, subreg.name, sep = ", ")) %>% 
    left_join(readRDS(here("data/subreg_mean_rain_lag")))
  
  znewdat <- readRDS(here("data/trend_analysis_table")) %>% 
    dplyr::select(year, subregion, species, tot.nests) %>% 
    right_join(spp_subreg %>% dplyr::select(species, subregion, "wt.lag.subreg.rain" = mean.subreg.rain))
    
    #data.frame(year = seq(1995, 2019), wt.lag.subreg.rain = spp_subreg$mean.subreg.rain)
  
  zmods <- readRDS(here("fitted_models/spp_subreg_mods"))[[zspp_subreg]]
  
  zspp_pred <- modavgPred(zmods, modnames = names(zmods), newdata = znewdat)$matrix.output %>% 
    data.frame() %>% 
    bind_cols(znewdat)  %>%
    mutate(alpha.code = zspp_subreg)
  
  return(zspp_pred)
}


mod_avg_predictions("GREG_PNM")

all_mod_av_preds <- map_df(spp_subreg$spp.subreg, mod_avg_predictions)

saveRDS(all_mod_av_preds, here("fitted_models/all_mod_av_preds"))






#' mod_predictions_link
#' 
#' calculate model estimates (predictions) for a species and subregion on the scale of the link function
#'
#' @param zspp_subreg 4 letter species code pasted to subregion code, e.g. GREG_All
#'
#' @return data frame
#' @export
#'
#' @examples mod_avg_predictions_per_change("GREG_All")
mod_avg_per_change <- function(zspp_subreg) {
  
  spp_subreg <- data.frame(spp.subreg = zspp_subreg)%>%
    separate(spp.subreg, c("species", "subregion"), sep = "_") %>% 
    left_join(readRDS(here("data/subreg_mean_rain_lag")))
  

  znewdat <- readRDS(here("data/trend_analysis_table")) %>% 
    dplyr::select(year, subregion, species, tot.nests) %>% 
    right_join(spp_subreg %>% dplyr::select(species, subregion, "wt.lag.subreg.rain" = mean.subreg.rain)) %>% 
    filter(year == min(year) | year == max(year)) %>% 
    arrange(-year) %>% 
    data.frame()
  
  
  zmods <- readRDS(here("fitted_models/spp_subreg_mods"))[[zspp_subreg]]
  
  zmods_effects <- modavgEffect(zmods, newdata = znewdat, conf.level = 0.95)$Matrix.output %>% 
    data.frame() %>% 
    rename("effect" = mod.avg.pred)
  names(zmods_effects) <- paste("marginal.", names(zmods_effects), sep = "")
  
  zspp_pred <- modavgPred(zmods, modnames = names(zmods), newdata = znewdat)$matrix.output %>% 
    data.frame() %>% 
    mutate(year = znewdat$year)
  
per.change <- 100 * (zmods_effects[1,1]/zspp_pred[2,1])
per.change.LCI <- 100 * (zmods_effects[1,3]/zspp_pred[2,1])
per.change.UCI <- 100 * (zmods_effects[1,4]/zspp_pred[2,1])

per_change_out <- cbind(per.change, per.change.LCI, per.change.UCI) %>% 
  data.frame() %>% 
  bind_cols(zmods_effects) %>% 
  dplyr::mutate(alpha.code = zspp_subreg)
  
  return(per_change_out)
}


all_mod_av_preds_per_change <- map_df(spp_subreg$spp.subreg, mod_avg_per_change)


saveRDS(all_mod_av_preds_per_change, here("fitted_models/all_mod_av_per_change"))


