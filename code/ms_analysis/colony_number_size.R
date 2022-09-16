

library(tidyverse)
#library(cowplot)
library(birdnames)
library(flextable)
library(here)


#source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.r")
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

col_size_table <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(hep_sites) %>%
  bind_rows(sfbbo_nests) %>% 
  filter(!species %in% c("DCCO", "CAEG"), peakactvnsts > 0, !is.na(year), between(year, start.year, end.year)) %>% 
  #cut_never_nested() %>% 
  dplyr::select(year, subregion, code, species, peakactvnsts) %>% 
  left_join(subreg_key)


# figure with colony size and # colonies by subregion, for each species separately ----
col_size_plotter <- function(zspp) {

num_cols <- col_size_table %>% 
  filter(species == zspp) %>%
  group_by(year, subreg.name) %>% 
  summarise(n.col = n()) %>% 
  ungroup() %>% 
  full_join(col_size_table %>% 
              filter(species == zspp) %>%
              group_by(subreg.name) %>%
              summarise(y.max = max(peakactvnsts)) %>%
              ungroup() %>%
              mutate(y.min = (y.max/-90) * 2))
  

col_size_table %>% 
  filter(species == zspp) %>% 
  ggplot()+
  geom_boxplot(aes(x = year, y = peakactvnsts, group = year)) +
  geom_text(data=num_cols, aes(label = n.col, y = y.min, x = year), colour="grey20", size=2) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) + 
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x) * 0.7, (max(x) + 1) * 1.3))))) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  labs(y = "Colony size",
       x = "Year",
       title = translate_bird_names(zspp, "alpha.code", "common.name")) +
  theme_bw() +
  theme(text = element_text(size=8))
}


col_size_plotter("GREG")
ggsave(here("figures/greg_col_size_num.png"), width = 8)






# table with mean # colonies and mean colony size ----

col_size_table_out <- col_size_table %>% 
  group_by(year, subreg.name, species) %>% 
  summarise(n.col = n()) %>% 
  ungroup() %>% 
  group_by(subreg.name, species) %>% 
  summarise(mean.n.col = mean(n.col),
            sd.n.col = sd(n.col)) %>% 
  full_join(col_size_table %>%
              group_by(subreg.name, species) %>%
              summarize(mean.col.size = mean(peakactvnsts),
                        sd.col.size = sd(peakactvnsts))) 
  
  col_size_table_out_wide <- col_size_table_out %>% 
  mutate(n.col.out = paste(round(mean.n.col, 1), " (", round(sd.n.col, 1), ")", sep = ""),
         mean.col.out = paste(round(mean.col.size, 1), " (", round(sd.col.size, 1), ")", sep = "")) %>% 
    select(subreg.name, species, contains("out")) %>% 
    pivot_wider(id_cols = subreg.name, names_from = species, values_from = contains("out")) %>% 
    select(subreg.name, contains("GREG"), contains("GBHE"), contains("SNEG"), contains("BCNH"))

flextable(col_size_table_out_wide) %>% 
      add_header_row(values = c("", "Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron"), colwidths = c(1, 2, 2, 2, 2)) %>%
    set_header_labels(subreg.name = "Subregion",
                      n.col.out_GREG = "Mean (st. dev.)\n# colonies",
                      mean.col.out_GREG = "Mean (st. dev.)\ncolony size",
                      n.col.out_GBHE = "Mean (st. dev.)\n# colonies",
                      mean.col.out_GBHE = "Mean (st. dev.)\ncolony size",
                      n.col.out_SNEG = "Mean (st. dev.)\n# colonies",
                      mean.col.out_SNEG = "Mean (st. dev.)\ncolony size",
                      n.col.out_BCNH = "Mean (st. dev.)\n# colonies",
                      mean.col.out_BCNH = "Mean (st. dev.)\ncolony size") %>% 
  autofit() %>% 
  fit_to_width(max_width = 7.5) %>%
    align(j = 2:9, align = "center", part = "all") %>% 
  save_as_docx(path = here("output/col_size_num_table.docx"))
            



# single figure for # of colonies across the entire study area ----

col_size_table %>% 
  group_by(species, year) %>% 
  summarise(n.col = n()) %>% 
  ungroup() %>% 
  mutate(species = translate_bird_names(species, "alpha.code", "common.name"),
         species = factor(species, levels = c("Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron"))) %>% 
  ggplot(aes(x = year, y = n.col, color = species)) +
  #geom_point() +
  geom_line() +
  stat_smooth(method = "lm", formula = y~1) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), labels = seq(0, 90, by = 5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Total active colonies",
       color = "")


ggsave(here("figures/total_active_colonies.png"), width = 8)
