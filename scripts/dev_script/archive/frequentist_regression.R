str(raw_plot_data)

ggplot(raw_plot_data)+
  geom_point(aes(x = year, y = interp_levels, color = spp))+
  facet_wrap(~metal, scales = 'free_y')

regress_df <- 
  raw_plot_data %>% 
  dplyr::select(spp,metal,year,interp_levels) %>% 
  ungroup()

attr(regress_df, which = "groups") <- NULL


ggplot(regress_df)+
  geom_point(aes(x = year, y = log(1/interp_levels), color = spp))+
  facet_wrap(~metal, scales = 'free_y')

ggplot(regress_df)+
  geom_point(aes(x = year, y = log(1/interp_levels), color = spp))
  

# bring in the trophic position estimates from Gagne et al. 2018
trophic_p <- read.csv('~/heavy_metal_birds/data/tp_through_time.csv')
# round to hundredths
trophic_p[,3:6] <- round(trophic_p[,3:6], digits = 3)

# plot it quick to look at it 
ggplot(trophic_p,aes(x = year, y = tp_med, color = spp))+
  geom_line(size = 1)+
  #scale_color_brewer(palette = "Dark2")+
  themeo

BFAL <- filter(trophic_p, spp == "LAAL") %>% mutate(spp = "BFAL")
RFBO <- filter(trophic_p, spp == "BRBO") %>% mutate(spp = "RFBO")
trophic_p <- rbind(trophic_p, BFAL, RFBO)
trophic_p <- filter(trophic_p, !spp == "TP")
trophic_p$spp <- droplevels(trophic_p$spp)

levels(trophic_p$spp)    
levels(regress_df$spp)

trophic_p$spp    <- as.character(trophic_p$spp) %>% as.factor()
regress_df$spp <- as.character(regress_df$spp) %>% as.factor()

# join metal readings with trophic position of bird at particular year
regress_df <- left_join(regress_df, trophic_p, by = c("year","spp")) %>%  select(-X)


str(regress_df)



# build a linear model through time to estimate if concentrations showed a significant directional trend through time
# and to detect if trophic position may have had a signficant impact on trend 
# how do we interpret the effect of trophic position?

# 
library(broom)

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

mod_blank

library(stargazer)

stargazer(mod_blank)



# What do we want to know from Stuart's approach? 

# Broad scale trend patterns
# 1. Does the metal show a significant incresing or decresing concentration in the species tissue through time?
# 2. 


regress_df %>% 
  split(.$spp) %>% 
  map(~ lm( log(1/interp_levels) ~ year + tp_med, data = .x)) %>% 
  map_dfr(~ as.data.frame(tidy(.)))
 


library(broom)














#regress_df <- regress_df[complete.cases(regress_df), ]             # remove rows of mismatched NA, listwise deletion #
