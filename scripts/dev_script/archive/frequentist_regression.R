



# Regression build for supplemental material

# build a linear model through time to estimate if concentrations showed a significant directional trend through time
# and to detect if trophic position may have had a signficant impact on trend 

# load libraries used in addition to paper summary script
library(broom)

# run lines 1-63 from paper summary script
str(raw_plot_data)

# df prep
regress_df <- 
  raw_plot_data %>% 
  dplyr::select(spp,metal,year,interp_levels) %>% 
  ungroup()

attr(regress_df, which = "groups") <- NULL

# bring in the trophic position estimates from Gagne et al. 2018
trophic_p <- read.csv('~/heavy_metal_birds/data/tp_through_time.csv')
# round to hundredths
trophic_p[,3:6] <- round(trophic_p[,3:6], digits = 3)

# assign LAAL to BFAL and RFBO to BRBO
BFAL <- filter(trophic_p, spp == "LAAL") %>% mutate(spp = "BFAL")
RFBO <- filter(trophic_p, spp == "BRBO") %>% mutate(spp = "RFBO")
trophic_p <- rbind(trophic_p, BFAL, RFBO)
trophic_p <- filter(trophic_p, !spp == "TP")
trophic_p$spp <- droplevels(trophic_p$spp)

trophic_p$spp    <- as.character(trophic_p$spp) %>% as.factor()
regress_df$spp <- as.character(regress_df$spp) %>% as.factor()

# join metal readings with trophic position of bird at particular year
regress_df <- left_join(regress_df, trophic_p, by = c("year","spp")) %>%  select(-X)

str(regress_df)

# build regression
metals <- levels(regress_df$metal)
mod_blank <- NULL
for(i in 1:length(metals)) {
  
  print(metals[i])
  
mod_sum <- regress_df %>% 
  filter(metal == metals[i]) %>% 
  lm( log(1/interp_levels) ~ year + tp_med, data = .) %>% 
  tidy() %>% 
  mutate(metal = metals[i])
  
mod_blank <- rbind(mod_blank,mod_sum)
  
}

# model table
mod_blank





