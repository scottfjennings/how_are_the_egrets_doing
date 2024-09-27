




# helper table for subregions
subreg_key <- read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/subregion_key.csv") %>% 
  mutate(subreg.name = str_replace(subreg.name, "River, Laguna", "River and Laguna"),
         subreg.name = factor(subreg.name, levels = c("Entire study area", 
                                                      "Outer Pacific Coast, North",
                                                      "Outer Pacific Coast, South", 
                                                      "Russian River and Laguna de Santa Rosa", 
                                                      "Northern Napa County",
                                                      "San Pablo Bay", 
                                                      "Central San Francisco Bay", 
                                                      "Suisun Bay", 
                                                      "Interior East Bay",
                                                      "South San Francisco Bay", 
                                                      "Santa Clara Valley")),
         tidal = ifelse(subregion %in% c("RUR", "NNC", "IEB", "SCV"), FALSE, TRUE),
         tidal = ifelse(subregion == "All", NA, tidal))
                     



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
      MASS::glm.nb(data = ., formula = tot.nests ~ poly(year, 2) + subreg.rain)

  }






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
    separate(spp.subreg, into = c("species", "subregion"), sep = "_")
  
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
  
  sub_rain <- subreg_mean_rain_lag %>%  
    #readRDS(here("data/subreg_rain")) %>%
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





# from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
# move legend to free facet spot

shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable::gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- cowplot::gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- cowplot::gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- cowplot::gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


plot_pred_comparison <- function(zspp) {
  raw_dat <- trend_analysis_table1 %>% 
    filter(species == zspp) %>% 
    dplyr::select(species, subregion, year, tot.nests)
  
  
  
  pred_dat <- preds %>% 
    filter(species == zspp) %>% 
    full_join(raw_dat) %>% 
    full_join(subreg_key) %>% 
    group_by(subregion, species) %>% 
    mutate(text.y = ceiling(max(tot.nests) * 1.1))
  
  
  mod_pred_plot <- pred_dat %>% 
    ggplot() +
    geom_line(aes(x = year, y = estimate), size = 0.5) +
    geom_ribbon(aes(x = year, ymin = lci, ymax = uci), alpha = 0.25) +
    geom_point(aes(x = year, y = tot.nests), size = 0.5)  + 
    scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) + 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.3))))) +
    facet_wrap(~subreg.name, scales = "free_y", labeller = labeller(subreg.name = label_wrap_gen(30))) +
    labs(y = "Nest abundance",
         x = "Year",
         title = "") +
    theme_bw() +
    theme(text = element_text(size=8))
  
  ggsave(paste("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/figures/mod_pred_plot_", zspp, ".png", sep = ""), mod_pred_plot, width = 7, height = 5, dpi = 600)
  
  mod_pred_plot
  
}




#' mod_dev_explained
#' 
#' get deviance explained from a particular model for a particular species
#'
#' @param zspp 
#' @param zmod 
#'
#' @return
#' @export
#'
#' @examples  brac_year_dev <- mod_dev_explained("BRAC", "year")
mod_dev_explained <- function(zspp, zmod) {
  zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zmod]]
  
  dev_explained <- data.frame(dev.expl = 1 - (zspp_mod$deviance/zspp_mod$null.deviance),
                              alpha.code = zspp,
                              Modnames = zmod) %>% 
    mutate(dev.expl = round(dev.expl, 2))
  return(dev_explained)
}



fix_mod_name_out <- function(zvarb) {
  zvarb = gsub("_", " + ", zvarb)
  zvarb = gsub("2", "^2^", zvarb)
  zvarb = gsub("year", "Year", zvarb)
  zvarb = gsub("rain", "Rain", zvarb)
  zvarb = gsub("lnYear", "log\\(Year\\)", zvarb)
} 


#' shift_legend
#' 
#' move plot legend to free facet spot. from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
#'
#' @param p the ggplot object
#'
#' @return
#' @export
#'
#' @examples
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable::gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- cowplot::gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable::gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- cowplot::gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}
