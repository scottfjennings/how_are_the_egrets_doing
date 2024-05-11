


library(tidyverse)
library(here)
library(birdnames)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
hepdata_location = here("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")



# using model predictions from the main analysis ----
# all these functions are in HEP_utility_functions.R
ouc_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion) %>% 
  filter(subregion == "OUC")

ouc_tot_annual_nests <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  right_join(ouc_sites) %>%
  filter(species %in% c("GREG", "GBHE"), peakactvnsts >= 0, !is.na(year)) %>%
  group_by(year, subregion, species) %>% 
  summarise(tot.nests = sum(peakactvnsts)) %>% 
  ungroup()




preds <- readRDS(here("output/model_predictions")) %>% 
  right_join(ouc_tot_annual_nests)


start_end_preds <- preds %>% 
  group_by(species, subregion) %>% 
  mutate(zyear = case_when(year == min(year) ~ "min.year",
                           year == max(year) ~ "max.year",
                           TRUE ~ NA)) %>% 
  ungroup() %>% 
  filter(!is.na(zyear)) %>% 
  pivot_wider(id_cols = c(species, subregion), names_from = zyear, values_from = estimate) %>% 
  mutate(per.change = ((max.year/min.year)) * 100,
         common.name = translate_bird_names(species, "alpha.code", "common.name"),
         common.name = factor(common.name, levels = c("Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron")),
         zgroup = ifelse(per.change > 300, "change > 300%", "change < 300%"))

# can filter to just subregion == "OUC" for the W Marin colonies
start_end_preds %>% 
  filter(subregion == "OUC", species %in% c("GREG", "GBHE")) %>% 
  ggplot() +
  geom_point(aes(color = per.change, x = per.change, y = common.name), size = 3) +
  geom_segment(aes(x = per.change, y = common.name, xend = 100, yend = common.name, color = per.change)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 100) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue", midpoint = 100)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("% change")



l300 <- start_end_preds %>%
  filter(per.change < 300) %>% 
  ggplot(aes(x = per.change, y = subregion)) +
  geom_point(aes(color = per.change, size = 3)) +
  geom_segment(aes(x = per.change, y = subregion, xend = 100, yend = subregion, color = per.change)) +
  geom_vline(xintercept = 100) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue", midpoint = 100)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("% change") +
  facet_grid(common.name ~ zgroup)

g300 <- start_end_preds %>%
  filter(per.change > 300) %>% 
  ggplot(aes(x = per.change, y = subregion)) +
  geom_point(aes(color = per.change, size = 3)) +
  geom_segment(aes(x = per.change, y = subregion, xend = 100, yend = subregion, color = per.change)) +
  geom_vline(xintercept = 100) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue", midpoint = 100)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("% change") +
  facet_grid(common.name ~ zgroup)

cowplot::plot_grid(l300, g300)




# refitting models for just Tomales Bay ----

tbay_parent_codes <- c(50, 152, 119, 122, 143, 83, 114, 32, 160, 113)


# prepare data ----
hepdata_location = here("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
tbay_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion) %>% 
  filter(parent.code %in% tbay_parent_codes)

options(scipen = 999)


tbay_analysis_table <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  right_join(tbay_sites) %>%
  filter(species %in% c("GREG", "GBHE"), peakactvnsts >= 0, !is.na(year)) %>% 
  #cut_never_nested() %>% 
  dplyr::select(year, subregion, code, species, peakactvnsts) 

# calculate a cumulative rain index following Stenzel and Page 2018
rain_lag <- readRDS(here("data/subreg_rain")) %>% 
  dplyr::select(year = birdyear, subregion, subreg.name, subreg.rain) %>%
  data.frame() %>% 
  arrange(subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(subreg.rain = subreg.rain + (lag(subreg.rain)/2) + (lag(subreg.rain, 2)/3))

subreg_mean_rain_lag <- rain_lag %>% 
  filter(between(year, min(tbay_analysis_table$year), max(tbay_analysis_table$year))) %>% 
  group_by(subregion) %>% 
  summarise(mean.subreg.rain = mean(subreg.rain, na.rm = TRUE))

trend_analysis_table <- tbay_analysis_table  %>%
  group_by(year, subregion, species) %>% 
  summarise(tot.nests = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  left_join(rain_lag)


tbay_spp_subreg <- trend_analysis_table %>% 
  distinct(species, subregion) %>% 
  mutate(spp.subreg = paste(species, subregion, sep = "_"))


tbay_spp_subreg_mods <- map2(tbay_spp_subreg$species, tbay_spp_subreg$subregion, fit_mods_glmbn_year2)
names(tbay_spp_subreg_mods) <- tbay_spp_subreg$spp.subreg



tbay_preds <- map2_df(tbay_spp_subreg_mods, names(tbay_spp_subreg_mods), get_preds_glmnb)


tbay_start_end_preds <- tbay_preds %>% 
  group_by(species, subregion) %>% 
  mutate(zyear = case_when(year == min(year) ~ "first.year",
                           year == max(year) ~ "last.year",
                           TRUE ~ NA)) %>% 
  ungroup() %>% 
  filter(!is.na(zyear)) %>% 
  pivot_wider(id_cols = c(species, subregion), names_from = zyear, values_from = estimate) %>% 
  mutate(per.change = ((last.year/first.year) - 1) * 100,
         per.change = round(per.change, 1),
         common.name = translate_bird_names(species, "alpha.code", "common.name"),
         common.name = factor(common.name, levels = c("Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron")),
         zgroup = ifelse(per.change > 300, "change > 300%", "change < 300%"))

tbay_start_end_preds %>% 
  ggplot() +
  geom_point(aes(color = per.change, x = per.change, y = common.name), size = 3) +
  geom_segment(aes(x = per.change, y = common.name, xend = 0, yend = common.name, color = per.change)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 0) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue", midpoint = 0)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("% change")



# combining Tomales and all W Marin
bind_rows(tbay_start_end_preds %>% 
            mutate(common.name = paste(common.name, "Tomales Bay")),
          start_end_preds %>% filter(subregion == "OUC", species %in% c("GREG", "GBHE")) %>% mutate(common.name = paste(common.name, "All W. Marin"))) %>% 
  ggplot() +
  geom_point(aes(color = per.change, x = per.change, y = common.name), size = 3) +
  geom_segment(aes(x = per.change, y = common.name, xend = 0, yend = common.name, color = per.change)) +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 0) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue", midpoint = 0)  +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("% change")




# combining Tomales and all W Marin
bind_rows(tbay_preds %>% mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"),
                                area = "Tomales Bay") %>% 
            left_join(trend_analysis_table %>% select(year, species, tot.nests)),
          preds %>% filter(subregion == "OUC", species %in% c("GREG", "GBHE")) %>% 
            mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"),
                   area = "All W. Marin")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = estimate, color = area)) +
  geom_ribbon(aes(x = year, ymin = lci, ymax = uci, fill = area), alpha = 0.25) +
  geom_point(aes(x = year, y = tot.nests, color = area))  + 
  facet_wrap(~common.name, scales = "free_y") +
  labs(y = "Nest abundance",
       x = "Year",
       title = "",
       color = "",
       fill = "") +
  theme_bw() +
  theme(text = element_text(size=8))

