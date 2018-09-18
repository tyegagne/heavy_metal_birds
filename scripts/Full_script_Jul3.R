# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(DescTools)
library(gtools)
library(minpack.lm) 
library(RColorBrewer)


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

# set working space with here and read in csv
library(here)

# dataframe read in
here()

metals <- read.csv( 'data/heavy_metal_June6.csv', header = T,na.strings = "--" )

unique_id <- paste0("B",seq(1,dim(metals)[1])) # create a unique ID for each specimen
metals <- cbind(unique_id, metals)             # add that to the dataframe

str(metals) # check the structure

# reshape the dataframe so every row is the contamination level for a single metal for a single specimen
metals_values_gathered <- dplyr::select(metals, unique_id, spp, year, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) %>% 
  gather(key = metal, value = measurement, Mn, Fe, Cu, Zn, As, Mo, Cd, Hg, Pb) 

# reshape the dataframe so every row is the min detection level for a single metal for a single specimen
metals_limits_gathered <- dplyr::select(metals, unique_id, spp, year, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl) %>% 
  gather(key = metal_limit, value = reference_limit, Mn_rl, Fe_rl, Cu_rl, Zn_rl, As_rl, Mo_rl, Cd_rl, Hg_rl, Pb_rl)

# join those dataframes 
joined_metal <- cbind(metals_values_gathered,ref_limit = metals_limits_gathered$reference_limit)

# create new column - if measurement level is NA then use min detection
joined_metal$interp_levels <- if_else(is.na(joined_metal$measurement),joined_metal$ref_limit/2,joined_metal$measurement) # using half of ref limit per Gains reccomendation

#clean up
rm(metals_limits_gathered,metals_values_gathered,metals,unique_id)

# because some more recent years have multiple observations per year, return average by year
joined_metal <- joined_metal %>% group_by(spp,metal,year) %>%
  summarise(interp_levels = mean(interp_levels), ref_limit = mean(ref_limit)) %>%
  ungroup() %>% 
  group_by(spp,metal) 


joined_metal$metal <- as.factor(joined_metal$metal)                              # metal to factor
joined_metal <- filter(joined_metal, !spp %in% c("RFBO","BFAL") )                # single/~2-3 year observations in recent 
joined_metal <- filter(joined_metal, !(metal %in% c("As",'Hg') & year < 1980) )  # filter Hg and As newer than 1980


# quantile winsorising
# Cutoff based on observations of grouped metals. Univariate outliers by metal level
# upper quantile in response to inflatted positive values
joined_metal <- 
  joined_metal %>% 
  group_by(metal) %>% 
  mutate(interp_levels = Winsorize(interp_levels,probs = c(0,0.90)))

# plots of metals by species facetted 
metal_by_spp <- ggplot(joined_metal,aes(x = year, y = (interp_levels), color = spp, group = spp))+
  geom_point(size = .2)+
  geom_line(size = .2)+
  scale_color_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  themeo

metal_by_spp + facet_wrap(~metal, scales = "free_y", ncol = 1)
metal_by_spp + facet_grid(metal~spp, scales = "free_y"); rm(metal_by_spp)

# uncorrected concentrations by metal
ggplot(joined_metal,aes(x = year, y = (interp_levels), color = spp, group = spp))+
  geom_point(size = 1)+
  geom_line(size = .75)+
  scale_color_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  themeo + facet_wrap(~metal, scales = "free_y")


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
rm(sc,yearvec,met,rep,dummy_df,m,s, metals, spp)

####
####
# linear interpolation of NAs
joined_metal <- 
  joined_metal %>% 
  dplyr::group_by(spp,metal) %>% 
  # this when uncommented extends extrapolation to min max years
  # the other interpolation line has to be hashed out
  # mutate(ip.value = na.approx(interp_levels, rule = 2)) 
  dplyr::mutate(time = seq(1,n())) %>%
  dplyr::mutate(ip.value = approx(time,interp_levels,time)$y) %>% 
  dplyr::select(-time)
####
####

# this plot checks the interpolation between years
joined_metal %>% 
  ggplot(aes(year,ip.value,color = metal))+
  geom_point()+
  scale_color_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  facet_wrap(~spp, scales = "free_y")+
  themeo

# reorder factor levels to match current order
joined_metal$metal <- fct_relevel(f = joined_metal$metal, c("As", "Cd", "Cu", "Fe","Hg", "Mn","Mo","Pb", "Zn")) 

# grouped by raw metal ensemble mean plot
ensem <- joined_metal %>% 
  dplyr::group_by(metal,year) %>% 
  dplyr::mutate(metal_ensemble = mean(ip.value, na.rm =T)) %>% 
  ggplot(aes(x = year, y = metal_ensemble, group = metal))+
  #geom_point(size = .5, color = "grey")+
  geom_line(size = 1, color = "grey")+ 
  #geom_smooth(show.legend = F, color = "black", size = .25)+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  scale_x_continuous(expand = c(0,0)) + 
  themeo
# plots of metals by species facetted 
metal_by_spp <- ggplot(joined_metal,aes(x = year, y = (ip.value), color = spp, group = spp))+
  #geom_point(size = .2, show.legend = F)+
  geom_line(size = .2, show.legend = F)+
  scale_x_continuous(expand = c(0,0)) + 
  scale_color_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  themeo
spp_raw <- metal_by_spp + facet_wrap(~metal, scales = "free_y", ncol = 1)

gridExtra::grid.arrange(spp_raw, ensem, ncol = 2)


# bring in the trophic position estimates from Gagne et al. 2018
trophic_p <- read.csv('data/tp_through_time.csv')
# round to hundredths
trophic_p[,3:6] <- round(trophic_p[,3:6], digits = 2)

# plot it quick to look at it 
tp_plot <- ggplot(trophic_p,aes(x = year, y = tp_med, color = spp))+
  geom_line()+
  themeo

# facet it by species, 'TP' is the ensemble of all of these spp
tp_plot + facet_wrap(~spp)

# merge this in with the metals data
# I want NAs for all years that don't have metal readings, to then interpolate.
# trophic species levels 
# equate levels
# which in this case means building a dataframe with TPs labeled with factors from the heavy metals

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


###
####
# Correction with ensemble TP decline instead of by species tp trajectory
# If you run the next 10 lines it will move forward with locating correction TTC values based 
# on ensemble of TP rather than species specific TPs. 
# Skip these 10 lines if you prefer to run corrections with spp specific trophic positions rather than ensemble.
tp_null <- NULL; spp <- levels(trophic_p$spp); ensem_tp <- subset(trophic_p, spp == "TP")
for(s in 1:length(spp)){
  tp_duh <-  data.frame(X = "x", spp = spp[s], 
                        year = seq(1891,2015,by = 1), 
                        tp_med = ensem_tp$tp_med, 
                        tp_upper = ensem_tp$tp_upper,
                        tp_lower = ensem_tp$tp_lower)
  tp_null <- rbind(tp_null,tp_duh)}
trophic_p <- tp_null
###
####


BFAL <- filter(trophic_p, spp == "LAAL") %>% mutate(spp = "BFAL")
RFBO <- filter(trophic_p, spp == "BRBO") %>% mutate(spp = "RFBO")
trophic_p <- rbind(trophic_p, BFAL, RFBO)
trophic_p <- filter(trophic_p, !spp == "TP")
trophic_p$spp <- droplevels(trophic_p$spp)

levels(trophic_p$spp)    
levels(joined_metal$spp)

trophic_p$spp    <- as.character(trophic_p$spp) %>% as.factor()
joined_metal$spp <- as.character(joined_metal$spp) %>% as.factor()

# join metal readings with trophic position of bird at particular year
joined_all_t <- left_join(joined_metal, trophic_p, by = c("year","spp")) %>%  select(-X)



### ### ### ### ### ### ### ### ### #
### Trophic Transfer Coefficients ###
### ### ### ### ### ### ### ### ### #
TTC <- read.csv('data/SuedelTTC.csv')

str(TTC)

ggplot(TTC,aes(x = TL, y = ttc))+
  geom_point()+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y")+
  scale_x_continuous(limits = c(0,5))+
  themeo


metals <- levels(TTC$metal)
#metals <- metals[c(1,2,4,5,6,7)] Prior Logistic NLS run
lookup_table <- NULL

for(i in 1:length(metals)){
  
  # Logistic sigmoidal fit - deprecated now
  # ----------------------
  # subset a single metal from the TTC table
  # TTC_sub <- subset(TTC, metal == metals[i])
  # find starting values of the logistic equation that will fit the TTC x TL data
  # starters <- coef(lm(logit(ttc/100) ~ TL, data = TTC_sub))
  # define the logistic equation to be fit
  # TTC_form <- ttc~phi1/(1+exp(-(phi2+phi3*TL)))
  # list of starting values
  # start <- list(phi1=30,phi2=starters[1] + .001 ,phi3=starters[2] + .001)
  # non linear least squares fit
  # fitTypical <- nlsLM(TTC_form, data=TTC_sub, start=start, trace = T)
  # dataframe of predictions of TTC accross range of TP i.e. 0 - 5
  # newdata <- data.frame(TL = seq(0,5, by = .01),ttc = predict(fitTypical, newdata = data.frame(TL = seq(0,5, by = .01))), metal = metals[i])
  # build dataframe of all metal's TTCs
  # lookup_table <- rbind(lookup_table,newdata)
  
  # Constrained spline fit
  # ----------------------
  TTC_sub <- subset(TTC, metal == metals[i])
  x <- TTC_sub$TL
  y <- TTC_sub$ttc
  x <- c(0,x)
  y <- c(1,y)
  weights <- seq(1,1, length.out = length(x))
  weights[1] <- 100
  spline_mod <- smooth.spline(x, y ,w = weights, df = 3)
  prediction <- predict(object = spline_mod, x = seq(0,5,by = 0.01))
  plot(x, y, xlim = c(0,5), main = metals[i])
  abline(h = 1, lty = "dashed")
  prediction$y <- ifelse(prediction$y < 0, .01, prediction$y )
  newdata <- data.frame(TL = seq(0,5, by = .01),ttc = prediction$y, metal = metals[i])
  lines(newdata$TL,newdata$ttc)
  lookup_table <- rbind(lookup_table,newdata)

}

# does it look like it should?
str(lookup_table)

# Copper is cadmium, and Iron and Mg will self represent as Zinc
# Cu <- subset(lookup_table, metal == "Cadmium"); Cu$metal <- "Copper" %>% as.factor(); str(Cu) # Run if log/sig mods
Mn <- subset(lookup_table, metal == "Zinc"); Mn$metal <- "Manganese" %>% as.factor(); str(Mn)
Fe <- subset(lookup_table, metal == "Zinc"); Fe$metal <- "Iron" %>% as.factor(); str(Fe)

# lookup_table <- rbind(lookup_table,Cu,Mn,Fe) # Run if log/sig mods
lookup_table <- rbind(lookup_table,Mn,Fe)


a <- ggplot(lookup_table, aes(TL, ttc))+
  geom_line()+
  geom_point(data = TTC,aes(x = TL, y = ttc), size = 1)+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1 )+
  themeo

b <- ggplot(lookup_table, aes(TL, ttc*10))+ # correction shows what 10ppm at base would look like at various TPs
  geom_line()+
  geom_hline(yintercept = 10, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo

gridExtra::grid.arrange(a,b, ncol = 2)

# in order to join the TTC lookup table with the metals levels table we need matching 
# metal factor levels 
# and Trophic positions as factors?

# First change, metal factors in the lookup table to appropriate levels in the metals levels


# > levels(joined_all_t$metal)
# [1] "As" "Cd" "Cu" "Fe" "Hg" "Mn" "Mo" "Pb" "Zn"

# > levels(lookup_table$metal)
# [1] "Arsenic"    "Cadmium"    "Lead"       "Mercury"    "Molybdenum" "Zinc"   

# Whats missing in the lookup table: Copper (can be fixed quick), Iron, Mn, 
# Cadmium appears to fit almost the exact same model as Copper, Copper will be Cadmium
joined_all <- joined_all_t

# paper figure
b + 
  annotate("rect",
           xmin = min(joined_all$tp_med, na.rm = T), 
           xmax = max(joined_all$tp_med, na.rm = T), 
           ymin = -Inf, 
           ymax = Inf, 
           alpha = .5)+
  facet_wrap(~metal, scales = "free_y", ncol = 2)+
  labs(title = "Trophic transfer of an environment level of 10 ppm", 
       y = "parts per million", 
       x = "trophic position")

ggplot(lookup_table, aes(as.numeric(as.character(TL)), ttc*1, color = metal))+ 
  geom_line(size = 5, alpha = .6)+
  geom_hline(yintercept = 1, lty = "dashed")+
  labs(title = "Trophic transfer of an environment level of 1 ppm",
       x = "trophic position", y = "ppm")+
  annotate("rect",
           #xmin = min(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           #xmax = max(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           xmin = 3.5,
           xmax = 4.5,
           ymin = -Inf, 
           ymax = Inf, 
           alpha = .5) +
  #annotate("text", x = .2, y = 0, label = "Biodilution", size = 15, hjust = 0, alpha = .25)+
  #annotate("text", x = .2, y = 2, label = "Biomagnification", size = 15, hjust = 0, alpha = .25)+
  annotate("text", x = 4.9, y = .8, label = "italic('Biodilution')", size = 3, hjust = 1, alpha = .5, parse = T)+
  annotate("text", x = 4.9, y = 1.2, label = "italic('Biomagnification')", size = 3, hjust = 1, alpha = .5, parse = T)+
  annotate("text", x = 0, y = 8, label = "italic('Sueddel et al. 1983')", size = 3, hjust = 0, alpha = .5, parse = T)+
  annotate("text", 
           x =  mean(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           y = 5, vjust = 0,
           label = "seabird trophic range", 
           size = 4, hjust = 0, alpha = 1, angle = 90, color = "white")+
  scale_x_continuous(expand = c(0,0), limits = c(-0.2,5))+
  # scale_y_continuous( limits = c(-.1,8.1), expand = c(0,0) )+
  scale_color_brewer(palette = "Paired")+
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1,2,3,4,5,6,7,8,9,10), limits = c(0,10) )+
  themeo +
  theme(legend.position = c(0.25, 0.65)
        # , text=element_text(size=16, family="Calibri")
  )+
  guides(color=guide_legend(ncol=2))


levels(joined_all$metal) # What want to change
levels(lookup_table$metal) # what we want to change it to

joined_all <- joined_all %>% ungroup() %>% 
  mutate( metal = fct_recode(metal, 
                             Arsenic = "As",
                             Cadmium   = "Cd",
                             Lead       = "Pb",
                             Mercury    = "Hg",
                             Molybdenum = "Mo",
                             Zinc       = "Zn",
                             Copper    = "Cu",
                             Manganese  = "Mn",
                             Iron      = "Fe"
  ))

str(lookup_table)
lookup_table <- lookup_table %>% mutate(tp_med = TL) %>% select(-TL)

joined_all$metal   <- as.character(joined_all$metal) %>% as.factor()
lookup_table$metal <- as.character(lookup_table$metal) %>% as.factor()

joined_all$tp_med   <- as.character(joined_all$tp_med) %>% as.factor()
lookup_table$tp_med   <- as.character(lookup_table$tp_med) %>% as.factor()

levels(joined_all$metal) # What want 
levels(lookup_table$metal)


# READ IN THE CAMBELL TTCs, eliminantes all suedel script above.
lookup_table <- read.csv("data/cambell_TTC.csv") # Cambell spline model fits
#lookup_table <- read.csv("cambell_TTC_logmods.csv") # Cambell log model model fits


lookup_table$tp_med <- as.factor(lookup_table$tp_med)

joined_all <- left_join(joined_all,lookup_table, by = c("metal","tp_med"))
#corrected <- joined_all %>% mutate(corrected_metal_level = interp_levels * ttc)  # think the bug is that interp levels should be ref as ip.value?
corrected <- joined_all %>% mutate(corrected_metal_level = ip.value / ttc) # TTC suggest that you divide by in order to represent the environmental availability given what you know it is at a particular TP

ggplot(corrected )+
  geom_line(aes(x = year, y = (corrected_metal_level), color = spp))+
  #geom_line(aes(x = year, y = interp_levels, color = spp))+
  facet_wrap(~metal, scales = "free_y")

ggplot(corrected )+
  geom_point(aes(x = year, y = (corrected_metal_level), color = metal))+
  #geom_line(aes(x = year, y = interp_levels, color = spp))+
  facet_wrap(~spp, scales = "free_y")

ggplot(corrected,aes(x = year, y = corrected_metal_level, group = spp) )+
  geom_line()+
  facet_wrap(~metal, scales = "free_y", ncol = 1)

# grouped by raw metal ensemble mean plot
ensem <- corrected %>% dplyr::group_by(metal,year) %>% 
  dplyr::mutate(metal_ensemble = mean(corrected_metal_level, na.rm =T)) %>% 
  ggplot(aes(x = year, y = metal_ensemble, group = metal))+
  geom_point(size = .5, color = "grey")+
  geom_line(size = .5, color = "grey")+ geom_smooth(show.legend = F, color = "black", size = .25)+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  scale_x_continuous(expand = c(0,0)) + 
  themeo
# plots of metals by species facetted 
metal_by_spp <- ggplot(corrected,aes(x = year, y = corrected_metal_level, color = spp, group = spp))+
  #geom_point(size = .2, show.legend = F)+
  geom_line(size = .2, show.legend = F)+
  scale_x_continuous(expand = c(0,0)) + 
  scale_color_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  themeo
spp_corrected <- metal_by_spp + facet_wrap(~metal, scales = "free_y", ncol = 1)

gridExtra::grid.arrange(spp_corrected, ensem, ncol = 2)
# To Do: 
# ppm global generation


gridExtra::grid.arrange(spp_raw,spp_corrected,ncol = 2)

# SCRATCH

# Metals vs. TP through time

str(corrected)




corrected %>% 
  ggplot(aes(x = as.numeric(tp_med), y = ip.value))+
  geom_path()+
  facet_grid(metal~spp, scales = "free_y")













  