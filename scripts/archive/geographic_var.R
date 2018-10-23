##############################
###  Popular ggPlot theme  ###
##############################
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

geo_fea <- read.delim('/Users/tgagne/heavy_metal_birds/data/geog_CAHFS_metals_feathers.csv', skip = 2, na.strings = "--") 
   #%>% filter(year > 2013)

unique_id <- paste0("B",seq(1,dim(geo_fea)[1])) # create a unique ID for each specimen
metals <- cbind(unique_id, geo_fea)             # add that to the dataframe

# reshape the dataframe so every row is the contamination level for a single metal for a single specimen
metals_values_gathered <- dplyr::select(metals, region, unique_id, spp, year, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) %>% 
  gather(key = metal, value = measurement, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) 

# reshape the dataframe so every row is the min detection level for a single metal for a single specimen
metals_limits_gathered <- dplyr::select(metals, region, unique_id, spp, year, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl) %>% 
  gather(key = metal_limit, value = reference_limit, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl)

# join those dataframes 
joined_metal <- cbind(metals_values_gathered,ref_limit = metals_limits_gathered$reference_limit)

# create new column - if measurement level is NA then use min detection
joined_metal$interp_levels <- if_else(is.na(joined_metal$measurement),joined_metal$ref_limit/2,joined_metal$measurement) # using half of ref limit per Gains reccomendation

#clean up
rm(metals_limits_gathered,metals_values_gathered,metals,unique_id)

str(joined_metal)

# because some more recent years have multiple observations per year, return average by year
#joined_metal <- joined_metal %>% group_by(region, spp,metal,year) %>%
#  summarise(interp_levels = mean(interp_levels), ref_limit = mean(ref_limit)) %>%
#  ungroup() %>% 
#  group_by(spp,metal) 

joined_metal$metal <- as.factor(joined_metal$metal)                              # metal to factor
joined_metal <- filter(joined_metal, !(metal %in% c("As",'Hg') & year < 1980) )  # filter Hg and As newer than 1980

# quantile winsorising
# Cutoff based on observations of grouped metals. Univariate outliers by metal level
# upper quantile in response to inflatted positive values
#joined_metal <- 
#  joined_metal %>% 
#  group_by(metal) %>% 
#  mutate(interp_levels = Winsorize(interp_levels,probs = c(0,0.90)))

joined_metal <- filter(joined_metal, spp %in% c("SOTE","BRNO")) 
  
ggplot(joined_metal, aes(x=region,y = interp_levels))+
  geom_boxplot()+
  facet_grid(metal~spp, scales = "free_y")

# bind appropriate TP to SOTE and BRNO TP data from Gagne et al. 2018b
geo_tp <- data.frame(region = c("American Samoa","American Samoa", "Hawaiian Islands", "Hawaiian Islands","Florida","Florida"),
           spp    = c("SOTE","BRNO","SOTE","BRNO","SOTE","BRNO"),
           tp_med     = c(4.05,3.99,3.79,3.64,3.61,3.60))
geo_tp

# then lookup appropriate TTC from Cambell
lookup_table <- read.csv("data/cambell_TTC.csv") # Cambell spline model fits


joined_metal$metal <- fct_recode(joined_metal$metal, 
           Arsenic = "As",
           Cadmium   = "Cd",
           Lead       = "Pb",
           Mercury    = "Hg",
           Molybdenum = "Mo",
           Zinc       = "Zn",
           Copper    = "Cu",
           Manganese  = "Mn",
           Iron      = "Fe"
)

str(joined_metal)
str(lookup_table)

geo_tp <- left_join(geo_tp,lookup_table, by = "tp_med")
geo_tp
str(geo_tp)

te <- left_join(joined_metal,geo_tp, by = c("spp","region","metal"))
te

str(te)
str(joined_metal)
str(geo_tp)



levels(geo_tp$metal)
levels(joined_metal$metal)








##################





#medians w/95% quantile interval
bar <- position_dodge(width = 0.2)
ggplot(a)+
  geom_point(aes(x=location,y=middle,color=spp),position = bar)+
  geom_errorbar(aes(x=location, ymin = lower, ymax = upper,color=spp),position = bar,width = .2)+
  geom_line(aes(x=location,y=middle,group=spp,color=spp), position = bar)+
  scale_color_brewer(palette = "Dark2")+
  ylab("trophic position")+
  xlab(" ")+
  theme_classic()+
  theme(strip.background = element_blank(),
        #axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        #axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        legend.position = c(0.8, .85),
        strip.text=element_text(hjust=0))




ggplot(joined_metal,aes(x = year, group = region, fill = region))+
  geom_histogram()




