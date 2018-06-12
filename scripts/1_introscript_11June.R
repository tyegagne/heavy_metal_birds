# load libraries
library(ggplot2)
library(dplyr)

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
setwd('C:/Users/omax0085/Dropbox/Seabird TL Project/data') # running from Lizzies computer
getwd()
metals <- read.csv('heavy_metal_June6.csv' , header = T,na.strings = "--" )

# look at structure, check read in
str(metals)
View(metals)

# If Mn is NA than set the value to the detection limit
metals$Mn <- if_else( is.na(metals$Mn), metals$Mn_rl, metals$Mn )

# filter out data for one species, in this case BUPE
metals_BUPE <- dplyr::filter(metals, spp == 'BUPE')

# R base plotting
plot(metals_BUPE$year, metals_BUPE$Pb)

# ggplot quick plotting
qplot(metals_BUPE$year, metals_BUPE$Pb) + geom_line()

# ggplot regular plotting, allows for lots of manipulation
ggplot(metals_BUPE,aes(x = year))  + 
  geom_line(aes(y = Pb)) +
  geom_line(aes(y = Mn))

# facetting
# beginning plotting just the raw points
ggplot(metals,aes(x = year))+
  geom_point(aes(y = Mn),se = F, color = "green")+
  geom_point(aes(y = Pb),se = F, color = "red")+
  geom_point(aes(y = Cu),se = F, color = "orange")+
  geom_point(aes(y = Cd),se = F, color = "purple")+
  facet_wrap(~spp, scales = "free_y") +
  theme_classic()

# connect the raw points
ggplot(metals,aes(x = year))+
  geom_line(aes(y = Mn), color = "green")+
  geom_line(aes(y = Pb), color = "red")+
  geom_line(aes(y = Cu), color = "orange")+
  geom_line(aes(y = Cd), color = "purple")+
  facet_wrap(~spp, scales = "free_y") +
  theme_classic()

# apply basic ggplot smoother over
ggplot(metals,aes(x = year))+
  geom_smooth(aes(y = Mn), span = .6, se = F, color = "green")+
  geom_smooth(aes(y = Pb), span = .6, se = F, color = "red")+
  geom_smooth(aes(y = Cu), span = .6, se = F, color = "orange")+
  geom_smooth(aes(y = Cd), span = .6, se = F, color = "purple")+
  facet_wrap(~spp, scales = "free_y") +
  theme_classic()











# tyler dataframe manipulation
library(tidyr) # you may have to install this

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

# plot by metal, by species
library(tidyverse)

# because some more recent years have multiple observations per year, return average by year
joined_metal <- joined_metal %>% group_by(spp,metal,year) %>%
  summarise(interp_levels = mean(interp_levels), ref_limit = mean(ref_limit)) %>%
  ungroup() %>% 
  group_by(spp,metal) 

#metal to factor
joined_metal$metal <- as.factor(joined_metal$metal)

# extreme value species
joined_metal <- filter(joined_metal, !spp %in% c("BUPE", "SOTE","RFBO","BFAL") )
joined_metal <- filter(joined_metal, !metal %in% c("As",'Hg') )


ggplot(joined_metal,aes(x = year, y = (interp_levels), color = spp, group = spp))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  facet_wrap(~metal, scales = "free_y")+
  themeo

ggplot(joined_metal,aes(x = year, y = (interp_levels), color = metal, group = metal))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  facet_wrap(~spp)+
  themeo




# interpolating values between years
# build dummy data.frame
# Build empty dataframe with relavant fields

spp      <- levels(droplevels(joined_metal$spp))
metals   <-  levels(droplevels(joined_metal$metal))
dummy_df <- NULL
yearvec  <- data.frame(year = seq(min(joined_metal$year), max(joined_metal$year)))

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


# linear interpolation of NAs
library(zoo)
joined_metal <- joined_metal %>% 
  group_by(spp,metal) %>% 
  #arrange(spp,metal,year) %>% 
  mutate(ip.value = na.approx(interp_levels, rule = 2)) %>% #linear interpolation
  #mutate(ip.value = na.approx(interp_levels, rule = 2)) %>% 
  #mutate(ip.value = na.approx(interp_levels, rule = 2)) %>% 

  ggplot(aes(year,ip.value,color = metal))+
  geom_point()+
  facet_wrap(~spp)

joined_metal %>% 
  group_by(spp,metal) %>% 
  #arrange(spp,metal,year) %>% 
  mutate(time = seq(1,n())) %>%
  mutate(ip.value = approx(time,interp_levels,time)$y) %>% 
  select(-time) %>% 
  
  ggplot(aes(year,ip.value,color = metal))+
  geom_point()+
  facet_wrap(~spp)




str(all_df)
all_df$modeled <- as.character(all_df$modeled) %>% as.numeric()
all_df$year <- as.character(all_df$year) %>% as.numeric()


ggplot()+
  geom_point(data = sample_n(all_df,5000),aes(year,modeled,color = metal))+
  facet_wrap(~spp, scales = 'free_y')

ggplot()+
  geom_point(data = all_df,aes(year,modeled,color = spp))+
  facet_wrap(~metal, scales = 'free_y')

ggplot()+
  geom_point(data = all_df,aes(x = year, y = modeled, color = spp),size = .2)+
  geom_point(data = joined_metal,aes(x = year, y = measurement, color = spp),size = .2)+
  #geom_line(size = .2)+
  facet_wrap(~metal, scales = "free_y")+
  themeo

ggplot(all_df,aes(x = year, y = modeled, color = metal))+
  geom_point(data = all_df,aes(x = year, y = modeled, color = metal),size = .2)+
  geom_point(data = joined_metal,aes(x = year, y = measurement, color = metal),size = .2)+
  #geom_line(size = .2)+
  facet_wrap(~spp, scales = "free_y")+
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





#Lizzies attempt to combine metals and trophic level for one species and one metal



# attempt to bootstrap estimates
# what we want:
# It would be great to for each metal for each species across time fit a number of smoothers
# Predict across the whole range from 1890 to 2015

# in a 'for' loop

joined_metal$metal <- as.factor(joined_metal$metal)
year <- seq(from = 1890, to = 2015,by = 1)
spp <- levels(droplevels(joined_metal$spp))
metals <- levels(droplevels(joined_metal$metal))
all_df <- NULL

for( i in 1:length(spp)) {
  
  # select a single species
  one_spp <- filter(joined_metal, spp == spp[i] )
  metal_df <- NULL
  
  for( x in 1:length(metals)){
    
    # select a single metal
    one_spp_one_metal <- filter(one_spp, metal == metals[x])
    rep <- NULL
    
    for( b in 1:10){ 
      # randomly sample ~80% observations
      one_spp_one_metal_samp <- sample_frac(one_spp_one_metal, size = .8)
      # fit a model to the sample
      smooth_mod <- loess(interp_levels ~ year, data = one_spp_one_metal_samp, span = 1)
      # predict across whole year range
      modeled <- predict(object = smooth_mod, newdata =  year)
      
      replic <- data.frame(cbind(year,modeled,b))
      rep <- rbind(rep,replic)
      
      
    }
    
    metal_rep <- cbind(rep,metal = metals[x])
    metal_df <- rbind(metal_df,metal_rep)
    
  }
  
  spp_rep <- cbind(metal_df,spp = spp[i])
  all_df <- rbind(all_df,spp_rep)
  
}


# adjusted bootstrap loop
joined_metal <- data.frame(year = joined_metal$year, spp = joined_metal$spp, metal = joined_metal$metal, interp_levels = joined_metal$ip.value)


joined_metal$metal <- as.factor(joined_metal$metal)
year <- seq(from = 1890, to = 2015,by = 1)
spp <- levels(droplevels(joined_metal$spp))
metals <- levels(droplevels(joined_metal$metal))
all_df <- NULL

rep <- NULL

for( i in 1:length(spp)) {
  
  # select a single species
  one_spp <- filter(joined_metal, spp == spp[i] )
 #metal_df <- NULL
  
  for( x in 1:length(metals)){
    
    # select a single metal
    one_spp_one_metal <- filter(one_spp, metal == metals[x])
    #rep <- NULL
    
    for( b in 1:10){ 
      # randomly sample ~80% observations
      one_spp_one_metal_samp <- sample_frac(one_spp_one_metal, size = .8)
      # fit a model to the sample
      smooth_mod <- loess(interp_levels ~ year, data = one_spp_one_metal_samp, span = 1)
      # predict across whole year range
      modeled <- predict(object = smooth_mod, newdata =  year)
      
      replic <- data.frame(cbind(year,modeled,b, spp = spp[i], metal = metals[x]))
      rep <- rbind(rep,replic)
      
    }
    #metal_rep <- rep
    #metal_df <- rbind(metal_df,metal_rep)
  }
  #spp_rep <- metal_df
  #all_df <- rbind(all_df,spp_rep)
}

str(rep)

colnames(rep) <- c("year","modeled","rep","spp","metal")

all_df <- rep







# scratch

# attempted in tidy, not successfull yet
library(broom)
booted <- joined_metal %>% 
  bootstrap(50) %>%
  group_by(spp, metal) %>%
  do(augment(smooth.spline(.$year, .$spp, df=4), .)) %>%
  ungroup()

str(booted)  

ggplot(aes(x = year, y = .fitted, color = spp, group = spp))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  facet_wrap(~metal, scales = "free_y")+
  themeo
