


# some basic data visualizations
# use data objects from prepare_data.R


 # how does nest abundance relate to bird year rainfall?
  hep_pop_birdyear_rain %>% 
    filter(species %in% zsppz) %>% 
  ggplot() +
    geom_point(aes(x = birdyear.rain, y = nest.abund)) +
    facet_wrap(~species, scales = "free")
    
  
  
  
#  how does nest abundance relate to previous year abundance?
  
    hep_pop_birdyear_rain %>% 
    filter(species %in% zsppz) %>% 
      arrange(species, parent.code, year) %>% 
      group_by(species, parent.code) %>% 
      mutate(prev.nest.abund = lag(nest.abund)) %>% 
      ungroup() %>% 
      ggplot() +
      geom_point(aes(prev.nest.abund, nest.abund)) +
      facet_wrap(~species, scales = "free")

    
  # auto corr in nest abundance for entire study area?
    
hep_pop_birdyear_rain %>% 
  filter(species == "GREG") %>% 
  group_by(year) %>% 
  summarise(nest.abund = sum(nest.abund)) %>% 
  ungroup() %>% 
  acf(.$nest.abund)

    
hep_pop_birdyear_rain %>% 
  filter(species == "GBHE", year > 1989) %>% 
  group_by(year) %>% 
  summarise(nest.abund = sum(nest.abund)) %>% 
  ungroup() %>% 
  acf(.)
