


# fit some models with basic autocorr structure
# use data from prepare_date.R


library(tidyverse)
library(nlme)
library(MASS)
library(AICcmodavg)
library(birdnames)

options(scipen = 999)

# little more data prep. fill all 0s so autocorr works
spp_year <- hep_pop_birdyear_rain %>% 
  group_by(year, species) %>% 
  summarise(nest.abund = sum(nest.abund)) %>% 
  ungroup() %>% 
  filter(!is.na(species), year >= 1989) %>% 
  pivot_wider(id_cols = year, names_from = species, values_from = nest.abund) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  pivot_longer(cols = c("BCNH", "GBHE", "GREG", "SNEG", "DCCO", "CAEG"), names_to = "species", values_to = "nest.abund")

spp_rain_year <- hep_pop_birdyear_rain %>% 
  group_by(year) %>% 
  summarise(mean.rain = mean(birdyear.rain)) %>% 
  ungroup() %>% 
  right_join(spp_year) %>% 
  filter(year > 1994, year < 2020)
  
znewdata <- data.frame(year = distinct(spp_rain_year, year),
                       mean.rain = mean(spp_rain_year$mean.rain))

theme_wbird <- function (base_size = 12, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_line(colour = "gray80"),
            panel.grid.minor = element_line(colour = "gray80")
    )   
}

#
# temporal autocor AR1 ----
fit_mods_ar1 <- function(zspecies) {
# year only
year_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ year, data = ., correlation = corAR1(form =~ year), method = "ML")

# rain only, but still with year AR1
rain_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain, data = ., correlation = corAR1(form =~ year), method = "ML")

# rain and year, but still with year AR1
year_rain_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain + year, data = ., correlation = corAR1(form =~ year), method = "ML")

# rain^2 and year, but still with year AR1
year_rain2_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain + I(mean.rain^2) + year, data = ., correlation = corAR1(form =~ year), method = "ML")

# year^2, but still with year AR1
year2_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ year + I(year^2), data = ., correlation = corAR1(form =~ year), method = "ML")


# rain^2 and year^2, but still with year AR1
year2_rain2_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain + I(mean.rain^2) + year + I(year^2), data = ., correlation = corAR1(form =~ year), method = "ML")

mods <- list("year_ar1" = year_ar1, 
             "rain_ar1" = rain_ar1, 
             "year_rain_ar1" = year_rain_ar1, 
             "year_rain2_ar1" = year_rain2_ar1,
             "year2_ar1" = year2_ar1,
             "year2_rain2_ar1" = year2_rain2_ar1)
return(mods)
}

# plotting best model
best_mod_plot_ar1 <- function(best_mod, spp) {

best_pred <- predictSE.gls(best_mod, newdata = znewdata, se.fit=T) %>% 
  data.frame() %>% 
  bind_cols(znewdata) %>% 
  mutate(lwr = fit - 1.96*se.fit,
         upr = fit + 1.96*se.fit)

best_plot <- ggplot(best_pred) +
  geom_line(aes(x = year, y = fit)) +
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr), alpha = 0.05) + 
  geom_point(data = filter(spp_rain_year, species == spp), aes(x = year, y = nest.abund))

return(best_plot)
}



greg_mods_ar1 <- fit_mods_ar1("GREG")
gbhe_mods_ar1 <- fit_mods_ar1("GBHE")
sneg_mods_ar1 <- fit_mods_ar1("SNEG")
bcnh_mods_ar1 <- fit_mods_ar1("BCNH")


greg_aic_ar1 <- aictab(greg_mods_ar1, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1", "year2_ar1", "year2_rain2_ar1"))
gbhe_aic_ar1 <- aictab(gbhe_mods_ar1, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1", "year2_ar1", "year2_rain2_ar1"))
sneg_aic_ar1 <- aictab(sneg_mods_ar1, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1", "year2_ar1", "year2_rain2_ar1"))
bcnh_aic_ar1 <- aictab(bcnh_mods_ar1, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1", "year2_ar1", "year2_rain2_ar1"))

greg_best_ar1 <- greg_mods_ar1[filter(greg_aic_ar1, Delta_AICc == 0)$Modnames][[1]]
gbhe_best_ar1 <- gbhe_mods_ar1[filter(gbhe_aic_ar1, Delta_AICc == 0)$Modnames][[1]]
sneg_best_ar1 <- sneg_mods_ar1[filter(sneg_aic_ar1, Delta_AICc == 0)$Modnames][[1]]
bcnh_best_ar1 <- bcnh_mods_ar1[filter(bcnh_aic_ar1, Delta_AICc == 0)$Modnames][[1]]
 

best_mod_plot_ar1(gbhe_best_ar1, "GBHE")
best_mod_plot_ar1(greg_best_ar1, "GREG")
best_mod_plot_ar1(sneg_best_ar1, "SNEG")
best_mod_plot_ar1(bcnh_best_ar1, "BCNH")




# glm-negative binomial ----
fit_mods_nb <- function(zspecies) {
# year only
year_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ year, data = .)

# rain only, but still with year nb
rain_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ mean.rain, data = .)

# rain and year, but still with year nb
year_rain_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ mean.rain + year, data = .)

# rain^2 and year, but still with year nb
year_rain2_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ mean.rain + I(mean.rain^2) + year, data = .)

# year^2, but still with year nb
year2_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ year + I(year^2), data = .)


# rain^2 and year^2, but still with year nb
year2_rain2_nb <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  glm.nb(nest.abund ~ mean.rain + I(mean.rain^2) + year + I(year^2), data = .)

mods <- list("year_nb" = year_nb, 
             "rain_nb" = rain_nb, 
             "year_rain_nb" = year_rain_nb, 
             "year_rain2_nb" = year_rain2_nb,
             "year2_nb" = year2_nb,
             "year2_rain2_nb" = year2_rain2_nb)
return(mods)
}

# plotting best model
best_mod_plot_nb <- function(best_mod, spp) {

ilink <- family(best_mod)$linkinv
 best_pred = predict(best_mod, znewdata, se.fit=TRUE, type='link') %>% 
  data.frame() %>% 
  cbind(znewdata) %>% 
  ungroup() %>% 
  mutate(predicted = ilink(fit),
         lci = ilink(fit - (1.96 * se.fit)),
         uci = ilink(fit + (1.96 * se.fit)))
  

best_plot <- ggplot(best_pred) +
  geom_line(aes(x = year, y = predicted)) +
  geom_ribbon(aes(x = year, ymin = lci, ymax = uci), alpha = 0.25) + 
  geom_point(data = filter(spp_rain_year, species == spp), aes(x = year, y = nest.abund)) +
  labs(title = translate_bird_names(spp, "alpha.code", "common.name"),
       y = "# nests",
       x = "") +
  theme_wbird()

return(best_plot)
}



greg_mods_nb <- fit_mods_nb("GREG")
gbhe_mods_nb <- fit_mods_nb("GBHE")
sneg_mods_nb <- fit_mods_nb("SNEG")
bcnh_mods_nb <- fit_mods_nb("BCNH")


greg_aic_nb <- aictab(greg_mods_nb, c("year_nb", "rain_nb", "year_rain_nb", "year_rain2_nb", "year2_nb", "year2_rain2_nb"))
gbhe_aic_nb <- aictab(gbhe_mods_nb, c("year_nb", "rain_nb", "year_rain_nb", "year_rain2_nb", "year2_nb", "year2_rain2_nb"))
sneg_aic_nb <- aictab(sneg_mods_nb, c("year_nb", "rain_nb", "year_rain_nb", "year_rain2_nb", "year2_nb", "year2_rain2_nb"))
bcnh_aic_nb <- aictab(bcnh_mods_nb, c("year_nb", "rain_nb", "year_rain_nb", "year_rain2_nb", "year2_nb", "year2_rain2_nb"))

greg_best_nb <- greg_mods_nb[filter(greg_aic_nb, Delta_AICc == 0)$Modnames][[1]]
gbhe_best_nb <- gbhe_mods_nb[filter(gbhe_aic_nb, Delta_AICc == 0)$Modnames][[1]]
sneg_best_nb <- sneg_mods_nb[filter(sneg_aic_nb, Delta_AICc == 0)$Modnames][[1]]
bcnh_best_nb <- bcnh_mods_nb[filter(bcnh_aic_nb, Delta_AICc == 0)$Modnames][[1]]
 

best_mod_plot_nb(gbhe_best_nb, "GBHE")
best_mod_plot_nb(greg_best_nb, "GREG")
best_mod_plot_nb(sneg_best_nb, "SNEG")
best_mod_plot_nb(bcnh_best_nb, "BCNH")

