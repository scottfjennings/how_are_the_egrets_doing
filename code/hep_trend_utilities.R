




# helper table for subregions
subreg_key <- read.csv("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/subregion_key.csv") %>% 
  mutate(subreg.name = factor(subreg.name, levels = c("Entire study area", 
                                                      "Outer Pacific Coast, North",
                                                      "Outer Pacific Coast, South", 
                                                      "Russian River, Laguna de Santa Rosa", 
                                                      "Northern Napa County",
                                                      "San Pablo Bay", 
                                                      "Central San Francisco Bay", 
                                                      "Suisun Bay", 
                                                      "Interior East Bay",
                                                      "South San Francisco Bay", 
                                                      "Santa Clara Valley")))
                     



# fit models ----
# 
#' Fit linear model on log transformed nest abundance
#'
#' @param zspp 
#' @param zsubreg 
#'
#' @return
#' @export
#'
#' @examples 
#' spp_subreg_mods_log <- map2(spp_subreg$species, spp_subreg$subregion, fit_mods_log_trans)
#' names(spp_subreg_mods_log) <- spp_subreg$spp.subreg

fit_mods_log_trans <- function(zspp, zsubreg) {
    
    zmod <- trend_analysis_table %>% 
      filter(species == zspp, subregion == zsubreg) %>% 
      lm(data = ., formula = log.nests ~ year)

  }




#' Fit linear model on untransformed nest abundance
#'
#' @param zspp 
#' @param zsubreg 
#'
#' @return
#' @export
#'
#' @examples
#' spp_subreg_mods <- map2(spp_subreg$species, spp_subreg$subregion, fit_mods_no_trans)
#' names(spp_subreg_mods) <- spp_subreg$spp.subreg
fit_mods_no_trans <- function(zspp, zsubreg) {
    
    zmod <- trend_analysis_table %>% 
      filter(species == zspp, subregion == zsubreg) %>% 
      lm(data = ., formula = tot.nests ~ year)

  }




#' Fit negative binomial glm on untransformed nest abundance
#'
#' @param zspp 
#' @param zsubreg 
#'
#' @return
#' @export
#'
#' @examples
#' spp_subreg_mods_glmnb <- map2(spp_subreg$species, spp_subreg$subregion, fit_mods_glmbn)
#' names(spp_subreg_mods_glmnb) <- spp_subreg$spp.subreg
fit_mods_glmbn <- function(zspp, zsubreg) {
    
    zmod <- trend_analysis_table %>% 
      filter(species == zspp, subregion == zsubreg) %>% 
      glm.nb(data = ., formula = tot.nests ~ year + subreg.rain)

  }


#' Fit negative binomial glm on untransformed nest abundance with year^2 as predictor
#'
#' @param zspp 
#' @param zsubreg 
#'
#' @return
#' @export
#'
#' @examples
#' spp_subreg_mods_glmnb <- map2(spp_subreg$species, spp_subreg$subregion, fit_mods_glmbn)
#' names(spp_subreg_mods_glmnb) <- spp_subreg$spp.subreg
fit_mods_glmbn_year2 <- function(zspp, zsubreg) {
    
    zmod <- trend_analysis_table %>% 
      filter(species == zspp, subregion == zsubreg) %>% 
      glm.nb(data = ., formula = tot.nests ~ poly(year, 2) + subreg.rain)

  }




# getting model components ----

#' extract model coefficients and their CI 
#'
#' @param zmod 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples
#' coef_ci_log <- map2_df(spp_subreg_mods_log, names(spp_subreg_mods_log), get_coefs_cis)
#' coef_ci <- map2_df(spp_subreg_mods, names(spp_subreg_mods), get_coefs_cis)
#' coef_ci_glmnb <- map2_df(spp_subreg_mods_glmnb, names(spp_subreg_mods_glmnb), get_coefs_cis)
get_coefs_cis <- function(zmod, zmod.name) { 
  
  spp_subreg <- zmod.name %>% 
    data.frame() %>% 
    rename(spp.subreg = 1) %>% 
    separate(spp.subreg, into = c("species", "subregion"))
  
  coefs <- coef(zmod) %>%
    data.frame() %>% 
    rename("coef" = 1) %>% 
    rownames_to_column("varb") 
  
  cis  <- confint(zmod) %>%
    data.frame() %>% 
    rename(lci = 1, uci = 2) %>% 
    rownames_to_column("varb")
  
  
  coef_ci <- full_join(coefs, cis) %>% 
#  mutate(subreg.trend.estimate = 100 * (exp(coef)-1),
#         trend.est.lci = 100 * (exp(lci)-1),
#         trend.est.uci = 100 * (exp(uci)-1)) %>% 
    bind_cols(spp_subreg)
}



#' Extract and backtransform model predictions and their CI from the lm on log transformed abundance
#'
#' @param zmod 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples
#' preds_log <- map2_df(spp_subreg_mods_log, names(spp_subreg_mods_log), get_preds_log)
get_preds_log <- function(zmod, zmod.name) { 
  
  spp_subreg <- zmod.name %>% 
    data.frame() %>% 
    rename(spp.subreg = 1) %>% 
    separate(spp.subreg, into = c("species", "subregion"))
  
  ana_table <- trend_analysis_table %>% 
    right_join(spp_subreg)
    
  znewdat = data.frame(year = seq(min(ana_table$year), max(ana_table$year)))
    
zmod_preds <- predict(zmod, newdat = znewdat, type = "response", se = TRUE)%>% 
      data.frame() %>% 
      bind_cols(znewdat) %>%
  #mutate(lci = fit - (1.96 * se.fit),
  #       uci = fit + (1.96 * se.fit))
  mutate(estimate = exp(fit),
         lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit))) %>% 
    bind_cols(spp_subreg)
  }





#' Extract model predictions and their CI from the lm on untransformed abuncance 
#'
#' @param zmod 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples
#' preds_lm <- map2_df(spp_subreg_mods, names(spp_subreg_mods), get_preds_lm)
get_preds_lm <- function(zmod, zmod.name) { 
  
  spp_subreg <- zmod.name %>% 
    data.frame() %>% 
    rename(spp.subreg = 1) %>% 
    separate(spp.subreg, into = c("species", "subregion"))
  
  ana_table <- trend_analysis_table %>% 
    right_join(spp_subreg)
    
  znewdat = data.frame(year = seq(min(ana_table$year), max(ana_table$year)))
    
zmod_preds <- predict(zmod, newdat = znewdat, type = "response", se = TRUE)%>% 
      data.frame() %>% 
      bind_cols(znewdat) %>%
  mutate(lci = fit - (1.96 * se.fit),
         uci = fit + (1.96 * se.fit)) %>% 
  rename(estimate = fit) %>% 
    bind_cols(spp_subreg)
  }



#' Extract and backtransform model predictions and their CI from the Negative Binomial GLMs
#'
#' @param zmod 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples
#' preds_glmnb <- map2_df(spp_subreg_mods_glmnb, names(spp_subreg_mods_glmnb), get_preds_glmnb)
get_preds_glmnb <- function(zmod, zmod.name) { 
  
  spp_subreg <- zmod.name %>% 
    data.frame() %>% 
    rename(spp.subreg = 1) %>% 
    separate(spp.subreg, into = c("species", "subregion"), sep = "_")
  
  ana_table <- trend_analysis_table %>% 
    right_join(spp_subreg)
  
  sub_rain <- readRDS(here("data/subreg_rain")) %>% 
    right_join(spp_subreg) %>% 
    distinct(mean.subreg.rain)
    
  znewdat = data.frame(year = seq(min(ana_table$year), max(ana_table$year)),
                       subreg.rain = sub_rain$mean.subreg.rain)
    
ilink <- family(zmod)$linkinv
 best_pred = predict(zmod, znewdat, se.fit=TRUE, type='link') %>% 
  data.frame() %>% 
  cbind(znewdat) %>% 
  ungroup() %>% 
  mutate(estimate = ilink(fit),
         lci = ilink(fit - (1.96 * se.fit)),
         uci = ilink(fit + (1.96 * se.fit))) %>% 
    bind_cols(spp_subreg)
}








