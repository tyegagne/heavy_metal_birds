# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)

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

# set working directory and read in csv
setwd('/Users/tgagne/Dropbox (MBA)/Seabird TL Project/data') # running from Tylers computer
#setwd('C:/Users/omax0085/Dropbox/Seabird TL Project/data') # running from Lizzies computer
getwd()

# tyler dataframe manipulation
 # you may have to install this
metals <- read.csv('heavy_metal_June6.csv' , header = T,na.strings = "--" )
unique_id <- paste0("B",seq(1,dim(metals)[1])) # create a unique ID for each specimen
metals <- cbind(unique_id, metals)             # add that to the dataframe

str(metals) # check the structure

# reshape the dataframe so every row is the contamination level for a single metal for a single specimen
metals_values_gathered <- select(metals, unique_id, spp, year, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) %>% 
  gather(key = metal, value = measurement, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) 

# reshape the dataframe so every row is the min detection level for a single metal for a single specimen
metals_limits_gathered <- select(metals, unique_id, spp, year, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl) %>% 
  gather(key = metal_limit, value = reference_limit, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl)

# join those dataframes 
joined_metal <- cbind(metals_values_gathered,ref_limit = metals_limits_gathered$reference_limit)

# create new column - if measurement level is NA then use min detection
joined_metal$interp_levels <- if_else(is.na(joined_metal$measurement),joined_metal$ref_limit,joined_metal$measurement)

#clean up
rm(metals_limits_gathered,metals_values_gathered,metals,unique_id)

# because some more recent years have multiple observations per year, return average by year
joined_metal <- joined_metal %>% group_by(spp,metal,year) %>%
  summarise(interp_levels = mean(interp_levels), ref_limit = mean(ref_limit)) %>%
  ungroup() %>% 
  group_by(spp,metal) 

#metal to factor
joined_metal$metal <- as.factor(joined_metal$metal)

# extreme value species
#joined_metal <- filter(joined_metal, !spp %in% c("BUPE", "SOTE","RFBO","BFAL") )
#joined_metal <- filter(joined_metal, !metal %in% c("As",'Hg') )
joined_metal <- filter(joined_metal, metal %in% c("As",'Hg') )


# metals by species facetted 
ggplot(joined_metal,aes(x = year, y = (interp_levels), color = spp, group = spp))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  facet_wrap(~metal, scales = "free_y")+
  #scale_color_brewer(palette = "Dark2")+
  themeo

# species by metal facetted
ggplot(joined_metal,aes(x = year, y = (interp_levels), color = metal, group = metal))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  facet_wrap(~spp, scales = "free_y")+
  scale_x_continuous(limits = c(1980,2017))+
  scale_y_continuous(limits = c(0,32))+
  scale_color_brewer(palette = "Dark2")+
  themeo

# interpolating values between years
# build dummy data.frame
spp      <- levels(droplevels(joined_metal$spp))
metals   <-  levels(droplevels(joined_metal$metal))
dummy_df <- NULL
yearvec  <- data.frame(year = seq(min(joined_metal$year), max(joined_metal$year)))

# Build empty dataframe with relavant fields
for(s in 1:length(spp)) {
    met <- NULL
  for(m in 1:length(metals)){
      rep <- NULL
      rep <- cbind(spp = spp[s], metal = metals[m],yearvec)
      met <- rbind(met,rep)
  }
  dummy_df <- rbind(dummy_df,met)
}

dummy_df$interp_levels <- NA
sc <- merge(joined_metal,dummy_df, by = c("spp","metal","year"), all = T)
sc$interp_levels.y <- NULL
sc$interp_levels <- sc$interp_levels.x
sc$interp_levels.x <- NULL
joined_metal <- sc

# cleanup
rm(sc,yearvec,met,rep,dummy_df,m,s)

# linear interpolation of NAs
joined_metal <- joined_metal %>% 
  group_by(spp,metal) %>% 
  mutate(ip.value = na.approx(interp_levels, rule = 2)) #linear interpolation
  #mutate(ip.value = na.approx(interp_levels, rule = 2)) %>% 
  #mutate(ip.value = na.approx(interp_levels, rule = 2)) %>% 

joined_metal %>% 
  ggplot(aes(year,ip.value,color = metal))+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~spp, scales = "free_y")

joined_metal %>% 
  group_by(spp,metal) %>% 
  mutate(time = seq(1,n())) %>%
  mutate(ip.value = approx(time,interp_levels,time)$y) %>% 
  select(-time) %>% 
  ggplot(aes(year,ip.value,color = metal))+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~spp)


# attempt to bootstrap estimates
# what we want:
# It would be great to for each metal for each species across time fit a number of smoothers
# Predict across the whole range from 1890 to 2015
# adjusted bootstrap loop starting dataframe
joined_metal <- data.frame(year = joined_metal$year, 
                           spp = joined_metal$spp, 
                           metal = joined_metal$metal, 
                           interp_levels = joined_metal$ip.value)

joined_metal %>% 
  ggplot(aes(year,interp_levels,color = metal))+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~spp, scales = "free_y")+
  themeo

joined_metal$metal <- as.factor(joined_metal$metal)
year <- seq(from = 1890, to = 2015,by = 1)
sppx <-   levels(droplevels(joined_metal$spp))
metals <- levels(droplevels(joined_metal$metal))
all_df <- NULL
rep <- NULL

for( i in 1:length(sppx)) {
  
  one_spp <- NULL
  # select a single species
  one_spp <- subset(joined_metal, spp == sppx[i] )

  for( x in 1:length(metals)){
    # select a single metal
    one_spp_one_metal <- filter(one_spp, metal == metals[x])
    
    for( b in 1:10){ 
      modeled <- NULL
      # randomly sample ~80% observations
      one_spp_one_metal_samp <- sample_frac(one_spp_one_metal, size = .2)
      # fit a model to the sample
      smooth_mod <- loess(interp_levels ~ year, data = one_spp_one_metal_samp, span = .5)
      # predict across whole year range
      modeled <- predict(object = smooth_mod, newdata =  year)
      replic <- data.frame(year,modeled,b, spp = sppx[i], metal = metals[x])
      rep <- rbind(rep,replic)
    }
  }
}

str(rep)
colnames(rep) <- c("year","modeled","rep","spp","metal")
all_df <- rep


# scratch
str(all_df)

ggplot()+
  geom_point(data = (all_df),aes(year,modeled,color = metal), size = .2)+
  facet_wrap(~spp, scales = 'free_y')+
  scale_color_brewer(palette = "Dark2")+
  themeo

ggplot()+
  geom_point(data = all_df,aes(year,modeled,color = spp), size = .2)+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~metal, scales = 'free_y')+
  themeo

ggplot()+
  geom_point(data = all_df,aes(x = year, y = modeled, color = spp),size = .2)+
  geom_point(data = joined_metal,aes(x = year, y = interp_levels, color = spp),size = .2, color = "black")+
  #geom_line(size = .2)+
  facet_wrap(~metal, scales = "free_y")+
  scale_color_brewer(palette = "Dark2")+
  themeo

ggplot(all_df,aes(x = year, y = modeled, color = metal))+
  geom_point(data = all_df,aes(x = year, y = modeled, color = metal),size = .2)+
  geom_point(data = joined_metal,aes(x = year, y = interp_levels, color = metal),size = .2, color = "black")+
  #geom_line(size = .2)+
  facet_wrap(~spp, scales = "free_y")+
  scale_color_brewer(palette = "Dark2")+
  themeo


# bring in the trophic position estimates from Gagne et al. 2018
trophic_p <- read.csv('tp_through_time.csv')
str(trophic_p)

# plot it quick to look at it 
ggplot(trophic_p,aes(x = year, y = tp_med, color = spp))+
  geom_line()+
  themeo

ggplot(trophic_p,aes(x = year, y = tp_med, color = spp))+
  geom_line()+
  facet_wrap(~spp)+
  themeo

# merge this in with the metals data
# I want NAs for all years that don't have metal readings, to then interpolate.
# trophic species levels 
# equate levels
# which in this case means building a dataframe with TPs labeled with factors from the heavy metals


levels(trophic_p$spp)
levels(joined_metal$spp)

# dragging through this.

# BRBO == BRBO
# BRNO == BRNO
# BUPE == BUPE
# LAAL == LAAL
# SOTE == SOTE
# WHTE == WHTE
# WTSH == WTSH
# WTTR == WTTR

# BFAL == LAAL in the TrophicTP
# RFBO == BRBO in the TrophicTP

str(trophic_p)

BFAL <- filter(trophic_p, spp == "LAAL") %>% mutate(spp = "BFAL")
RFBO <- filter(trophic_p, spp == "BRBO") %>% mutate(spp = "RFBO")

trophic_p <- rbind(trophic_p, BFAL, RFBO)
trophic_p <- filter(trophic_p, !spp == "TP")

levels(trophic_p$spp)

joined_all <- left_join(joined_metal, trophic_p, by = c("year","spp")) %>%  select(-X)

ggplot(data = joined_all, aes(x = year, y = interp_levels))+
  geom_line(aes(color = metal)) + 
  facet_wrap(~spp, scales = "free_y")+
  geom_line(aes(y = tp_med^5), color = "black") +
  themeo




ggplot(data = joined_all)+
  geom_line(aes(x = year, y = tp_med )) + 
  facet_wrap(~spp)+
  themeo



