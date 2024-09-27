


library(tidyverse)
library(here)
library(cowplot)
library(birdnames)


mod_avg_per_change <- readRDS(here("fitted_models/all_mod_av_per_change")) %>% 
  filter(str_detect(alpha.code, "All")) %>% 
  mutate(alpha.code = str_replace(alpha.code, "_All", ""),
         common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"))

spp_order <- mod_avg_per_change  %>% 
  arrange(per.change) 

spp.order = spp_order$common.name


mod_avg_per_change <- mod_avg_per_change %>% 
  mutate(common.name = factor(common.name, levels = spp.order))
zback_color = "white"

ggplot(data = mod_avg_per_change, aes(x = per.change, y = common.name)) +
    geom_segment(aes(x = -100, y = common.name, xend = max(per.change), yend = common.name), color = 'gray85') +
    geom_point(aes(color = per.change, size = 3)) +
    geom_segment(aes(x = per.change, y = common.name, xend = 0, yend = common.name, color = per.change)) +
    geom_vline(xintercept = 0)  +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = "",
         y = "",
         title = "Percent change in heron and egret nesting abundance,\nSF Bay Area, CA, 1995 to 2019") +
    scale_y_discrete(limits = rev(levels(mod_avg_per_change$common.name))) +
  scale_x_continuous(breaks = seq(-30, 30, by = 10), limits = c(-30, 30)) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue")+
  theme(plot.margin = unit(c(.5, 1, 0, 0), "cm"))  +
  theme(text = element_text(size=12, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA),
        plot.title = element_text(hjust = 0.5))




ggsave(here("figures/HEP_overall_per_change.png"))

