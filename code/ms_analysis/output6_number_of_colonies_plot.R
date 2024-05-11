


library(tidyverse)
library(here)
library(birdnames)
library(grid)
library(cowplot)
library(scales)


custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/hep_trend_utilities.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")


options(scipen = 999)



analysis_table <- readRDS(here("data/acr_sfbbo_combined")) %>%
  filter(peakactvnsts > 0)

# species per colony
spp_per_colony <- analysis_table %>% 
  filter(peakactvnsts > 0) %>% 
  distinct(year, code, species) %>% 
  group_by(year, code) %>% 
  summarise(num.spp = n()) %>% 
  ungroup() %>% 
  group_by(num.spp) %>% 
  summarise(num.years.spp = n())

spp_per_colony_wide <- spp_per_colony %>% 
  pivot_wider(id_cols = code, names_from = num.spp, values_from = num.years.spp)



  summarise(mean.spp = mean(num.spp),
            max.spp = max(num.spp),
            min.spp = min(num.spp))

# number of colonies and colony size by species
n_colonies_spp <- analysis_table %>%
  mutate(subregion = "All") %>% 
  bind_rows(analysis_table) %>% 
#  distinct(subregion, year, code, species) %>% 
  group_by(subregion, species, year) %>% 
  summarise(n.colonies = n(),
            tot.nests = sum(peakactvnsts),
            mean.col.size = mean(peakactvnsts),
            sd.col.size = sd(peakactvnsts)) %>% 
  ungroup() %>% 
  left_join(subreg_key) %>% 
  mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"))


# number of colonies and colony size species combined
# for this one first need to add all species at colony scale
n_colonies_combined <- analysis_table %>% 
  mutate(subregion = "All") %>% 
  bind_rows(analysis_table) %>% 
  group_by(year, subregion, code) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() %>% 
#  distinct(subregion, year, code) %>% 
  group_by(subregion, year) %>% 
  summarise(n.colonies = n(),
            tot.nests = sum(peakactvnsts),
            mean.col.size = mean(peakactvnsts),
            sd.col.size = sd(peakactvnsts)) %>% 
  ungroup() %>%  
  left_join(subreg_key) %>% 
    mutate(common.name = "All species combined")

n_colonies <- bind_rows(n_colonies_combined, n_colonies_spp) %>% 
  mutate(common.name = factor(common.name, levels = c("All species combined", "Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron")))


# basic summaries for the paper

n_colonies_combined %>% filter(subregion == "All", common.name == "All species combined", year >= 1995) %>% view()

n_colonies_spp %>% filter(common.name != "All species combined") %>% group_by(subregion, common.name) %>% summarise(mean.n.col = mean(n.colonies)) %>% view()

n_colonies_spp %>% filter(common.name != "All species combined") %>% group_by(common.name) %>% filter(n.colonies == min(n.colonies) | n.colonies == max(n.colonies)) %>% distinct(common.name, n.colonies) %>% view()


n_colonies_spp %>% filter(common.name != "All species combined") %>% group_by(common.name) %>% filter(mean.col.size == min(mean.col.size) | mean.col.size == max(mean.col.size)) %>% distinct(common.name, mean.col.size) %>% view()


# 1 part plot with each subregion the same size ----

n_colonies %>% 
  filter(year > 1994) %>% 
  ggplot() +
  #  geom_area(aes(x = year, y = n.colonies, fill = common.name), size = 1) +
  #  geom_col(aes(x = year, y = n.colonies, fill = common.name), , position = "dodge2") +
  geom_line(aes(x = year, y = n.colonies, color = common.name)) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks= pretty_breaks()) + 
  expand_limits(y = 0) + 
  labs(x = "Year",
       y = "# colonies",
       color = "")+ 
  theme(text = element_text(size=8),
        legend.position="bottom")

ggsave(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/number_colonies.png", sep = ""), width = 7, height = 9, dpi = 600)


##

analysis_table %>% 
  left_join(subreg_key) %>% 
  mutate(common.name = translate_bird_names(species, "alpha.code", "common.name"),
         common.name = factor(common.name, levels = c("All species combined", "Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron"))) %>% 
  filter(year > 1994, species == "GREG") %>% 
  ungroup() %>% 
  ggplot()+
  geom_boxplot(aes(x = year, y = peakactvnsts, group = year)) +
  geom_point(aes(x = year, y = peakactvnsts)) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw()


# 1 part plot with each subregion the same size ----

n_colonies_spp  %>% 
  mutate(common.name = factor(common.name, levels = c("All species combined", "Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron"))) %>% 
  filter(year > 1994) %>% 
  ggplot() +
  geom_point(aes(x = year, y = n.colonies, color = common.name), size = 1, position=position_dodge(width = .5)) +
  stat_smooth(aes(x = year, y = n.colonies, color = common.name), se = FALSE) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks= pretty_breaks()) + 
  expand_limits(y = 0) + 
  labs(x = "Year",
       y = "# colonies",
       color = "")+ 
  theme(text = element_text(size=8),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggsave(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/number_colonies_smooth.png", sep = ""), width = 7, height = 7, dpi = 600)


# mean colony size ----
n_colonies_spp  %>% 
  mutate(common.name = factor(common.name, levels = c("All species combined", "Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron"))) %>% 
  filter(year > 1994) %>% 
  ggplot() +
  geom_point(aes(x = year, y = mean.col.size, color = common.name), size = 1, position=position_dodge(width = .5)) +
  stat_smooth(aes(x = year, y = mean.col.size, color = common.name), se = FALSE) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks= pretty_breaks()) + 
  expand_limits(y = 0) + 
  labs(x = "Year",
       y = "Mean colony size",
       color = "")+ 
  theme(text = element_text(size=8),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggsave(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/colony_size_smooth.png", sep = ""), width = 7, height = 7, dpi = 600)


# number of colonies and mean colony size together ----

col_size_num_plotter <- function(zspp) {
zplot <- n_colonies_spp  %>% 
  filter(year > 1994, species == zspp) %>% 
  full_join(subreg_key) %>% 
  group_by(subregion, species) %>% 
  mutate(biggest.y = ifelse(mean.col.size > n.colonies, mean.col.size, n.colonies),
         max.y = ifelse(max(biggest.y) > 10, ceiling(max(biggest.y) * 1.1), 10),
         max.y = replace_na(max.y, 10)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = year, y = n.colonies), alpha = 0.6) +
  geom_point(aes(x = year, y = mean.col.size), size = 1, position=position_dodge(width = .5)) +
  stat_smooth(aes(x = year, y = mean.col.size), se = FALSE, color = "black") +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks= pretty_breaks()) + 
  expand_limits(y = 0) + 
  labs(x = "Year",
       y = "Mean colony size (points, line)\n# colonies (bars)",
       title = translate_bird_names(zspp, "alpha.code", "common.name")) + 
  theme(text = element_text(size=8),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    geom_blank(aes(y = max.y))

ggsave(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/colony_size_num_", zspp, ".png", sep = ""), width = 7, height = 7, dpi = 600)

return(zplot)
}

col_size_num_plotter("GREG")
col_size_num_plotter("GBHE")
col_size_num_plotter("SNEG")
col_size_num_plotter("BCNH")






# 2 part plot with entire subregion larger

n_col_plot_subreg <- n_colonies %>% 
  filter(subregion != "All", year > 1994) %>% 
  ggplot() +
#  geom_area(aes(x = year, y = n.colonies, fill = common.name), size = 1) +
#  geom_col(aes(x = year, y = n.colonies, fill = common.name), , position = "dodge2") +
  geom_line(aes(x = year, y = n.colonies, color = common.name)) +
  facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks= pretty_breaks()) + 
  expand_limits(y = 0) + 
  labs(x = "",
       y = "",
       fill = "")+ 
  theme(legend.position="none",
        text = element_text(size=8),
        plot.margin = unit(c(0, .75, 0.75, 0.75), "cm"))




n_col_plot_allreg <- n_colonies %>% 
  filter(subregion == "All") %>%  
  ggplot() +
  #geom_area(aes(x = year, y = n.colonies, fill = common.name), size = 1) +
  #geom_col(aes(x = year, y = n.colonies, fill = common.name), position = "dodge2") +
  geom_line(aes(x = year, y = n.colonies, color = common.name)) +
  geom_vline(xintercept = 1994) +
  facet_wrap(~subreg.name, labeller = labeller(subreg.name = label_wrap_gen(30))) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  expand_limits(y = 0) + 
  labs(x = "",
       y = "",
       fill = "") +
  theme(text = element_text(size=8),
        legend.title=element_blank(),
        plot.margin = unit(c(0.5, .75, 0.75, 0.75), "cm"),
        axis.text.x = element_text(angle = 45, hjust=1.25, vjust = 1.5),
        legend.position="top") +
  guides(fill=guide_legend(ncol=2))


ylab <- ggdraw() + 
  draw_label(
    "# colonies",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) 

 
plot_grid(n_col_plot_allreg,
          #shift_legend(n_col_plot_subreg),
          n_col_plot_subreg,
          ncol = 1,
          align = "v", axis = "l")  + 
  draw_label("Year", x=0.5, y=  0, vjust=0.25, angle= 0) +
  draw_label("# colonies", x=  0, y=0.5, vjust= 0, angle=90)
  

ggsave(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/number_colonies_2panel.png", sep = ""), width = 7, height = 9, dpi = 600)




library(plotly)
library(scatterplot3d)


zz <- filter(n_colonies_spp, species == "GREG")
scatterplot3d(x = zz$mean.col.size, y = zz$n.colonies, z = zz$tot.nests)


# 

colony_peakactvnsts <- right_join(n_colonies, readRDS(here("data/trend_analysis_table"))) %>% 
  filter(!is.na(n.colonies)) 

colony_peakactvnsts %>% 
  ggplot() +
  geom_point(aes(x = n.colonies, y = tot.nests, color = species)) +
  facet_wrap(~subreg.name, scales = "free")

colony_peakactvnsts %>% 
  select(species, subregion, n.colonies, tot.nests) %>% 
  cor()

group_cor <- function(zspp, zsubreg) {
zdat <- filter(colony_peakactvnsts, species == zspp, subregion == zsubreg)
zcor <- cor(zdat$n.colonies, zdat$tot.nests) %>% 
  data.frame() %>% 
  mutate(species = zspp,
         subregion = zsubreg)
}

spp_subreg <- readRDS(here("data/spp_subreg")) %>%
  mutate(species = factor(species, levels = c("GREG", "GBHE", "SNEG", "BCNH"))) %>% 
  full_join(subreg_key) %>% 
  arrange(species, subreg.name) 



all_cor <- map2_df(spp_subreg$species, spp_subreg$subregion, group_cor)
