

library(tidyverse)
library(here)
library(ggpubr)

start.year = 1995
end.year = 2019

options(scipen = 999)

source(here("code/ms_analysis/hep_trend_utilities.r"))

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb") %>% 
  distinct(parent.code, parent.site.name, subregion)


sites_subreg <- read.csv(here("data/all_sfbbo_sites_subreg.csv")) %>% 
  dplyr::select(parent.site.name = site.name, subregion) %>% 
  bind_rows(hep_sites %>% dplyr::select(parent.site.name, subregion))


colony_rain_season <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_prism_data/hep_prism_combined") %>% 
  left_join(hep_sites) %>% 
  bind_rows(readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_prism_data/sfbbo_prism_combined") %>% 
              rename("parent.site.name" = site.name)) %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(birdyear = ifelse(month <= 9, year, year + 1),
         rain.season = case_when(between(month, 3, 6) ~ "spring", # Mar, Apr, May, Jun = spring
                                 between(month, 7, 10) ~ "dry", # Jul, Aug, Sep, Oct = dry season
                                 month > 10 | month < 3 ~ "winter")) %>% # Nov, Dec, Jan, Feb, Mar = winter 
  group_by(parent.site.name, birdyear, rain.season) %>% 
  summarise(season.year.rain = sum(rain.mm)) %>% 
  ungroup()


colony_rain <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_prism_data/hep_prism_combined") %>% 
  left_join(hep_sites) %>% 
  bind_rows(readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_prism_data/sfbbo_prism_combined") %>% 
              rename("parent.site.name" = site.name)) %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(birdyear = ifelse(month <= 6, year, year + 1)) %>% 
  group_by(parent.site.name, birdyear) %>% 
  summarise(colony.year.rain = sum(rain.mm)) %>% 
  ungroup() 

subreg_rain <- colony_rain %>% 
  left_join(sites_subreg) %>% 
  bind_rows(colony_rain %>% mutate(subregion = "All")) %>% 
  group_by(subregion, birdyear) %>% 
  summarise(subreg.rain = mean(colony.year.rain)) %>% 
  ungroup() %>% 
  group_by(subregion) %>% 
  mutate(mean.subreg.rain = mean(subreg.rain),
         rain.dev = subreg.rain - mean.subreg.rain,
         rain.dev.label = ifelse(rain.dev > 0, "Above average", "Below average")) %>% 
  full_join(subreg_key)

saveRDS(subreg_rain, here("data/subreg_rain"))



### summarize, visualize

lm_eqn <- function(df){
  # https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
  m <- lm(subreg.rain ~ birdyear, df)
  
  #eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
  #                 list(a = format(unname(coef(m)[1]), digits = 2),
  #                      b = format(unname(coef(m)[2]), digits = 2),
  #                      r2 = format(summary(m)$r.squared, digits = 3)))
  #as.character(as.expression(eq))
  r2 <- summary(m)$r.squared
}


subreg_rain_nested <- subreg_rain %>% 
  filter(between(birdyear, start.year, end.year)) %>% 
  group_by(subreg.name) %>% 
  nest()


rain_model <- function(df) {
  lm(subreg.rain ~ birdyear, data = df)
}
  
subreg_rain_nested <- subreg_rain_nested %>%
  mutate(model = map(data, rain_model))


rain_mod_r2 <- subreg_rain_nested %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  select(subreg.name, adj.r.squared, p.value) %>% 
  mutate(across(c(adj.r.squared, p.value), ~format(round(., 2), nsmall = 2, trim=TRUE)),
         r2.out = paste("~R^{2} == ", adj.r.squared, sep = ""),
         p.out = paste("P-value = ", p.value, sep = ""))

subreg_rain %>% 
  filter(between(birdyear, start.year, end.year)) %>% 
  ggplot() +
  geom_col(aes(x = birdyear, y = subreg.rain)) +
  geom_smooth(aes(x = birdyear, y = subreg.rain), method = "lm", color = "black", se = TRUE, size = .5, formula = y ~ x) +
  geom_text(data = rain_mod_r2, aes(x = 2011.5, y = 1600, label = r2.out), parse = TRUE, size = 2, hjust = 0.25) +
  geom_text(data = rain_mod_r2, aes(x = 2011.5, y = 1400, label = p.out), size = 2) +
  facet_wrap(~subreg.name, labeller = labeller(subreg.name = label_wrap_gen(30))) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Annual rainfall (mm)")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


  

ggsave(here("figures/subregion_rain_plot.png"), width = 7, height = 7, dpi = 300)


subreg_rain %>% 
  ggplot() +
  geom_col(aes(x = birdyear, y = rain.dev, fill = rain.dev.label)) +
  scale_fill_manual(values = c("blue", "red")) +
  stat_smooth(aes(x = birdyear, y = rain.dev), method = "lm", color = "black", se = FALSE, size = .5) +
  facet_wrap(~subreg.name, labeller = labeller(subreg.name = label_wrap_gen(30))) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Deviation from average rainfall",
       fill = "") +
  theme(text = element_text(size=8))

ggsave(here("figures/rain_deviation.png"), width = 7, height = 4)



# looking at residuals

  get_rain_resids <- function(ana_table) {
    znewdat = expand.grid(birdyear = seq(start.year, end.year))
    zmod <- ana_table %>% 
      lm(data = ., formula = colony.year.rain ~ birdyear)
    
zmod_preds <- predict(zmod, newdat = znewdat, se.fit = FALSE)%>% 
      data.frame() %>% 
  rename(fit = 1) %>% 
      bind_cols(znewdat)
  }

colony_rain_trend <- colony_rain %>% 
  filter(between(birdyear, start.year, end.year)) %>% 
  group_by(parent.site.name) %>%  
  nest() %>% 
   mutate(
     mod = data %>% map(get_rain_resids)
   ) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = c(mod))


colony_rain_resid <- colony_rain_trend %>% 
  left_join(colony_rain) %>% 
  mutate(colony.rain.resid = -1 * (season.year.rain - fit)^2) %>% # this is the Kelly et al 2014 treatment of rain "To linearize the effects of seasonal rainfall on annual nest abundance, the effects of total rainfall were modeled as rainfall suitability, with a quadratic decline during increasingly wet or dry years, relative to long-term average rainfall: x’=−(x− ˉx)2."
  arrange(parent.code, rain.season, birdyear) %>% 
  group_by(parent.code, rain.season) %>% 
  mutate(t1.resid = lag(colony.rain.resid),
         t2.resid = lag(colony.rain.resid, n = 2)) %>% 
  ungroup()
  
colony_rain_resid_long <- colony_rain_resid %>% 
  pivot_longer(cols = contains("resid"))

colony_rain_resid_wide <- colony_rain_resid_long %>%
  dplyr::select(-fit, -season.year.rain) %>% 
  filter(rain.season != "dry") %>% # don't want any summer rain
  filter(!(rain.season == "winter" & name == "t2.resid")) %>% # don't want rain 3 winters ago 
  filter(!(rain.season == "spring" & name == "colony.rain.resid")) %>% # don't want current spring rain
  mutate(name = paste(rain.season, name, sep = ".")) %>% 
  group_by(parent.code, birdyear) %>% 
  mutate(#mean.lags.resid = mean(value, na.rm = TRUE),
         tot.lags.resid = sum(value, na.rm = TRUE)) %>% 
  arrange(parent.code, birdyear)
  
colony_rain_resid_wide %>% 
    filter(parent.code == 1) %>% 
ggplot() +
  geom_line(aes(x = birdyear, y = value, color = name)) +
 # geom_line(aes(x = birdyear, y = mean.lags.resid)) +
  geom_line(aes(x = birdyear, y = tot.lags.resid), linetype = 2)


subreg_rain <- colony_rain_resid_wide %>% 
  distinct(birdyear, parent.code, tot.lags.resid) %>% 
  left_join(hep_sites) %>% 
  group_by(birdyear, subregion) %>% 
  summarise(subreg.rain = mean(tot.lags.resid))
  