

library(tidyverse)
library(birdnames)
library(here)


abund <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance")

colonies <- abund %>% 
  filter(year > 1988, peakactvnsts > 0, species %in% c("GREG", "GBHE", "SNEG", "BCNH")) %>% 
  group_by(year, species) %>% 
  summarise(num.colonies = n()) %>% 
  ungroup() %>% 
  mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"))

theme_wbird <- function (base_size = 12, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_line(colour = "gray80"),
            panel.grid.minor = element_line(colour = "gray80")
    )   
}


ggplot(colonies) +
  geom_line(aes(x = year, y = num.colonies, color = common.name)) +
  labs(y = "Number of active colonies monitored",
       x = "",
       color = "") +
  theme_wbird() +
    theme(legend.position="bottom")

ggsave(here("documents/report/number_of_colonies.png"), width = 7, height = 6)
