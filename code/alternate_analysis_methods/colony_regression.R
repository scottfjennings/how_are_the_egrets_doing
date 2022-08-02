




library(tidyverse)
library(tidymodels)
library(boot)
library(here)

source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")

source(here("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R"))




hepdata_location = here("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, subregion)

options(scipen = 999)



# calculate separate trend for each subregion across the entire study period ----

dat <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(hep_sites) %>% 
  cut_never_nested()%>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
  arrange(code, species, year) %>% 
  group_by(code, species) %>% 
  mutate(keep = cut_lead_trail_zeros(peakactvnsts)) %>% 
  filter(keep == TRUE) %>% 
  dplyr::select(-keep)
  


subreg_weight <- dat %>%
  filter(between(year, 1994, 2019), peakactvnsts >= 0) %>% 
  group_by(subregion, code, species) %>% 
  summarise(n.counts = n(),
            count.prod = prod(peakactvnsts + 1),
            geom.mean = count.prod ^ (1/n.counts) - 1) %>% 
  ungroup() %>% 
  arrange(subregion, code, species)
  

analysis_table <- dat %>% 
  filter(peakactvnsts >= 0, !is.na(year), between(year, 1994, 2019)) %>% 
  select(year, subregion, code, species, peakactvnsts) %>%  
  mutate(log.nests = log(peakactvnsts + 1))
  


  fit_mod <- function(ana_table) {
    zmod <- ana_table %>% 
      lm(data = ., formula = log.nests ~ year)
    
    zmod_coef <- coef(zmod)[2] %>% 
      data.frame() %>% 
      rename(year.coef = 1)
  }

  


trend_coefs <- analysis_table %>% 
  group_by(subregion, code, species) %>%
  mutate(tot.nests = sum(peakactvnsts),
         num.years = n()) %>%
  filter(tot.nests > 0, num.years > 1) %>% 
  select(-tot.nests, -num.years) %>% 
  nest() %>% 
   mutate(
     mod = data %>% map(fit_mod)
   )  %>% 
  unnest(cols = c(mod))


weighted_col_trends <- trend_coefs %>% 
  select(-data) %>% 
  left_join(subreg_weight) %>% 
  mutate(wt.col.trend = year.coef * geom.mean)

weighted_subreg_trends <- weighted_col_trends %>% 
  group_by(subregion, species) %>% 
  summarise(subreg.trend = sum(wt.col.trend),
            subreg.wt = sum(geom.mean)) %>% 
  ungroup() %>% 
  mutate(weighted.subreg.trend = subreg.trend/subreg.wt)



weighted_subreg_trends %>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
  ggplot() +
  geom_point(aes(x = subregion, y = weighted.subreg.trend, color = subregion)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~species, scales = "free_y")

# bootstrapping for CI on the trends, this also gives the coef values fitted to the whole dataset: id == "Apparent"
trend_boot <- analysis_table %>% 
  group_by(subregion, code, species) %>%
  mutate(tot.nests = sum(peakactvnsts),
         num.years = n()) %>%
  filter(tot.nests > 0, num.years > 1) %>% 
  group_modify(
      ~ bootstraps(., times = 1000, apparent = TRUE) %>%
        mutate(
          model = map(splits, ~ lm(log.nests ~ year, data = .)),
          coefs = map(model, tidy)
        ) 
    )

# save output
trend_boot %>% 
  dplyr::select(code, species, id, coefs) %>% 
  saveRDS(here("data/trend_boot_coefs"))


long_boot <- trend_boot %>% 
  select(-splits, -model) %>% 
  unnest(coefs) %>% 
  filter(term == "year") %>% 
  ungroup() %>% 
  select(subregion, code, species, id, year.coef = estimate)


weighted_long_boot <- long_boot  %>% 
  filter(!is.na(year.coef)) %>% 
  left_join(subreg_weight)  %>% 
  mutate(wt.col.trend = year.coef * geom.mean)%>% 
  group_by(subregion, species, id) %>% 
  summarise(subreg.trend = sum(wt.col.trend),
            subreg.wt = sum(geom.mean)) %>% 
  ungroup() %>% 
  mutate(weighted.subreg.trend = subreg.trend/subreg.wt)

weighted_trend_coefs <- weighted_long_boot %>% 
  filter(id == "Apparent")
  
weighted_trend_ci <- weighted_long_boot %>% 
  filter(id != "Apparent") %>% 
  group_by(subregion, species) %>% 
  summarise(uci = quantile(weighted.subreg.trend, 0.975),
            lci = quantile(weighted.subreg.trend, 0.025))


weighted_trend_coefs_ci %>% 
  full_join(weighted_trend_coefs, weighted_trend_c)


# calculate separate trend for each 5 year period ----
year_blocks <- data.frame(year = seq(1995, 2019),
                          yr.block = c(rep("95_99", length.out = 5), rep("00_04", length.out = 5), rep("05_09", length.out = 5), rep("10_14", length.out = 5), rep("15_19", length.out = 5))) %>% 
  mutate(yr.block = factor(yr.block, levels = c("95_99", "00_04", "05_09", "10_14", "15_19")))

dat <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/hep_annual_nest_abundance") %>%
  full_join(year_blocks) %>% 
  full_join(hep_sites) %>% 
  cut_never_nested()


subreg_weight <- dat %>%
  filter(between(year, 1995, 2019), peakactvnsts >= 0) %>% 
  group_by(subregion, yr.block, code, species) %>% 
  summarise(n.counts = n(),
            count.prod = prod(peakactvnsts + 1),
            geom.mean = count.prod ^ (1/n.counts) - 1) %>% 
  ungroup() %>% 
  arrange(subregion, code, species, yr.block)
  

analysis_table <- dat %>% 
  filter(peakactvnsts >= 0, !is.na(year), between(year, 1995, 2019)) %>% 
  select(year, yr.block, subregion, code, species, peakactvnsts) %>%  
  mutate(log.nests = log(peakactvnsts + 1))
  


  fit_mod <- function(ana_table) {
    zmod <- ana_table %>% 
      lm(data = ., formula = log.nests ~ year)
    
    zmod_coef <- coef(zmod)[2] %>% 
      data.frame() %>% 
      rename(year.coef = 1)
  }

  


trend_coefs <- analysis_table %>% 
  group_by(subregion, yr.block, code, species) %>%
  mutate(tot.nests = sum(peakactvnsts),
         num.years = n()) %>%
  filter(tot.nests > 0, num.years > 1) %>% 
  select(-tot.nests, -num.years) %>% 
  nest() %>% 
   mutate(
     mod = data %>% map(fit_mod)
   )  %>% 
  unnest(cols = c(mod))


weighted_col_trends <- trend_coefs %>% 
  select(-data) %>% 
  left_join(subreg_weight) %>% 
  mutate(wt.col.trend = year.coef * geom.mean)


weighted_subreg_trends <- weighted_col_trends %>% 
  group_by(subregion, species, yr.block) %>% 
  summarise(subreg.trend = sum(wt.col.trend),
            subreg.wt = sum(geom.mean)) %>% 
  ungroup() %>% 
  mutate(weighted.subreg.trend = subreg.trend/subreg.wt)


weighted_subreg_trends %>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
  full_join(data.frame(yr.block = factor(c("95_99", "00_04", "05_09", "10_14", "15_19"), levels = c("95_99", "00_04", "05_09", "10_14", "15_19")),
                       yr.block.num = seq(1, 5))) %>% 
  ggplot() +
  geom_line(aes(x = yr.block.num, y = weighted.subreg.trend, color = subregion)) +
  facet_wrap(~species, scales = "free_y")


  
  
  