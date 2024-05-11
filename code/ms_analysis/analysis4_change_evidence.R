



library(tidyverse)
library(MASS)
library(AICcmodavg)
library(birdnames)
library(here)


#source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")




get_all_aic <- function(zspp_subreg) {
  zmods <- readRDS(here("fitted_models/spp_subreg_mods"))[[zspp_subreg]]
  
  aic = aictab(zmods) %>% 
    data.frame() %>% 
    mutate(spp.subreg = zspp_subreg)
}


spp_subreg <- readRDS(here("data/spp_subreg")) %>%
  mutate(species = factor(species, levels = c("GREG", "GBHE", "SNEG", "BCNH"))) %>% 
  full_join(subreg_key) %>% 
  arrange(species, subreg.name) 


all_aic <- map_df(spp_subreg$spp.subreg, get_all_aic) 

year_best <- all_aic %>% 
  filter(Delta_AICc == 0 & str_detect(Modnames, "year")) %>%
  distinct(spp.subreg) %>%  
  mutate(year.best = 1)

interpect_best <- all_aic %>% 
  filter(Delta_AICc == 0 & str_detect(Modnames, "intercept")) %>% 
  distinct(spp.subreg) %>% 
  mutate(intercept.best = 1)

year_competitive <- all_aic %>% 
  filter(between(Delta_AICc, 0.0001, 2) & str_detect(Modnames, "year")) %>% 
  distinct(spp.subreg) %>% 
  mutate(year.competitive = 1)

intercept_competitive <- all_aic %>% 
  filter(between(Delta_AICc, 0.0001, 2) & str_detect(Modnames, "intercept")) %>%
  distinct(spp.subreg) %>%  
  mutate(intercept.competitive = 1)



change_evidence <- all_aic %>% 
  filter(Delta_AICc <= 2) %>% 
  group_by(spp.subreg) %>% 
  summarise(Modnames = paste(Modnames, collapse = ", ")) %>% 
  mutate(change.evidence = ifelse(str_detect(Modnames, "year") & !str_detect(Modnames, "intercept"), "strong", "weak")) %>%
  separate(spp.subreg, c("species", "subregion"), sep = "_") %>% 
  left_join(subreg_key) %>% 
  left_join(spp_subreg) %>% 
  full_join(year_best) %>% 
  full_join(interpect_best) %>% 
  full_join(year_competitive) %>% 
  full_join(intercept_competitive) %>% 
  mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"),
         species = factor(species, levels = levels(spp_subreg$species)))

saveRDS(change_evidence, here("fitted_models/change_evidence"))


change_evidence <- readRDS(here("fitted_models/change_evidence"))

filter(change_evidence, (year.best == 1 | year.competitive == 1) & (intercept.best == 1 | intercept.competitive == 1)) %>% nrow()


