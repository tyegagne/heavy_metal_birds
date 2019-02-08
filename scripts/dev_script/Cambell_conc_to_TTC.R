library(tidyverse)


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

theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          strip.text=element_text(hjust=0) )}

# set working directory and read in csv
conc_comp <- read.csv('~/heavy_metal_birds/data/concentration_compilation.csv', na.strings = '-') %>%  # read in the reference from which environmental 
                                                                              # concentrations where scraped
                                      filter(reference == "Cambell_2005") %>% # Cambell 2005 in this case is Baffin Bay pelagic study
                                      drop_na(concentration)                  # drop any NAs in the concentration data
# look at it quick
str(conc_comp)
ggplot(conc_comp,aes(x = published.TL,y = concentration))+
  geom_point()+
  facet_wrap(~Metal, scales = "free_y")

par(mfrow = c(4,4))                                # Establish the plotting frame                    
metals <- droplevels(conc_comp$Metal) %>% levels() # What metals are in that reference,  build a vector
cambell_lookup <- NULL                             # empty dataframe to put in complete TTC lookup data
raw_met_data <- NULL
 
for(e in 1:length(metals)){   # i.e. : for each metal, do this:
  
  # subset on the TL and concentration data for a single metal as an x and a y vector and combine in to a data.frame
  x_base = subset(conc_comp, Metal == metals[e])[,"published.TL"]
  y_base = subset(conc_comp, Metal == metals[e])[,"concentration"]
  df <- data.frame(x = x_base,y = y_base)
  
  # fit 3 degree spline to raw concentration data
  spline_mod <- smooth.spline(df$x, df$y, df = 3)
  prediction <- predict(object = spline_mod, x = seq(0,5,by = 0.01))
  
  # plot this data, raw and predicted
  plot(df$x, df$y, xlab = "trophic position", ylab = "concentration (ppm)",pch = 20, xlim = c(0,5), main = metals[e])
  lines(prediction)
  
  # obtain 0 intercept of the model
  mod_intercept <- as.data.frame(prediction)[1,2]
  mod_intercept
  
  # Because baseline concentration cannot be below 0 if recorded in tissues at any concentration, then correct here
  # We need to settle on an approach in order to address intercept, i.e. envrionmental concentration approximation
  # I've currently settled on:
  mod_intercept <- ifelse(mod_intercept <= 0,min(df$y)+mad(df$y), mod_intercept)
  # The above script assumes the y-intercept as the minimum value observed plus the median absolute deviation.
  # This is akin to minimum plus one standard deviation. Though MAD is robust to extreme values.
  # The justification of this decision is such that we avoid overestimating biomagnification due to extremely low values
  # of concentration in a particular tissue from the reference study.
  
  # correct the concentration data given the intercepts, modeled vs real
  # this converts the concentrations to multipliers (TTC) given that we know (modeled) the environmental level i.e. TP = 0
  mod_corrected_TTC <- df$y / mod_intercept
  
  # Add at TTC of 1 at 0, i.e. environmental TTC = 1, no multiplier
  x <- c(0,df$x)
  y <- c(1,mod_corrected_TTC)

  # establish a weighting vector for the spline model
  # This vector anchors the spline at (0,1), while allowing for flexibility in all values of x > 0
  weights <- seq(1,1, length.out = length(x))
  weights[1] <- 1000
  
  # fit a spline to the derived TTCs while respective of the weighting vector
  est_mod <- smooth.spline(x, y, df = 3, w = weights) 
  # predict across the whole range of trophic positions, from TP = 0 - 5
  est_prediction <- predict(object = est_mod, x = seq(0,5,by = 0.01)) %>% as.data.frame()
  
  # Plot it with raw TTC underneath modeled TTCs
  plot(x, y, pch = 20, ylab = "transfer coefficient", xlab = "trophic position", xlim = c(0,5))
  lines(est_prediction)
  # make a dataframe of predicted TTCs
  metal_lookup <- data.frame(tp_med = est_prediction$x,ttc = est_prediction$y,metal = metals[e])
  raw_met <- data.frame(tp_med = x, ttc_raw = y, metal = metals[e])
  # bind this to the data frame of other metals
  cambell_lookup <- rbind(metal_lookup,cambell_lookup)
  raw_met_data   <- rbind(raw_met_data,raw_met)

}
 
#cambell_lookup$tp_med <- as.factor(cambell_lookup$tp_med)
# match these lookup metals to our data
 #levels(joined_all$metal) # What want 
 #[1] "Arsenic"    "Cadmium"    "Copper"     "Iron"       "Lead"       "Manganese"  "Mercury"    "Molybdenum" "Zinc"  
 
levels(cambell_lookup$metal)
#[1] "Zinc"          "Molybdenum"    "Methylmercury" "Manganese"     "Lead"          "Copper"        "Cadmium"       "Arsenic" 
cambell_lookup$metal <- plyr::revalue(cambell_lookup$metal, c("Methylmercury"="Mercury"))
raw_met_data$metal <- plyr::revalue(raw_met_data$metal, c("Methylmercury"="Mercury"))


# Iron will self represent as Zinc
Fe <- subset(cambell_lookup, metal == "Zinc"); Fe$metal <- "Iron" %>% as.factor(); str(Fe)
cambell_lookup <- rbind(cambell_lookup,Fe)
cambell_lookup$metal <- as.character( cambell_lookup$metal ) %>% as.factor() # reorder alphabetically
 
# below zero adjustment methods
# two approaches:
# 
# 1: simply convert any TTC below 0, to 0
#cambell_lookup$ttc <- ifelse( cambell_lookup$ttc <= 0, 0,  cambell_lookup$ttc)

# 2: Standardize all biolution TTCs from 1 to -Inf to 1 to 0.
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
cambell_lookup$ttc[cambell_lookup$ttc <= 1] <- range01(cambell_lookup$ttc[cambell_lookup$ttc <= 1])
 
# write.csv(cambell_lookup,"cambell_TTC.csv", row.names = F)


# correction shows what 1 ppm at base would look like at various TPs
b <- ggplot(cambell_lookup, aes(as.numeric(as.character(tp_med)), ttc*1  ))+ 
  geom_line()+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo

b + annotate("rect",
           xmin = min(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           xmax = max(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           ymin = -Inf, 
           ymax = Inf, 
           alpha = .5)+
  facet_wrap(~metal, ncol = 2, scales = "free_y")+
  labs(title = "Trophic transfer of an environment level of 1 ppm", 
       y = "parts per million", 
       x = "trophic position")

# Overlay Figure
# paper figure 2
# correction shows what 1ppm at base would look like at various TPs
ggplot(cambell_lookup, aes(as.numeric(as.character(tp_med)), ttc*1, color = metal))+ 
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
  annotate("text", x = 0, y = 8, label = "italic('Cambell et al. 2005')", size = 3, hjust = 0, alpha = .5, parse = T)+
  annotate("text", 
           x =  mean(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           y = 5, vjust = 0,
           label = "seabird trophic range", 
           size = 4, hjust = 0, alpha = 1, angle = 90, color = "white")+
  scale_x_continuous(expand = c(0,0), limits = c(-0.2,5))+
 # scale_y_continuous( limits = c(-.1,8.1), expand = c(0,0) )+
  scale_color_brewer(palette = "Paired")+
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1,2,3,4,5,6,7,8) )+
  themeo +
  theme(legend.position = c(0.25, 0.65)
       # , text=element_text(size=16, family="Calibri")
        )+
  guides(color=guide_legend(ncol=2))
  

# reorder metal factors by degree of magnification
cambell_lookup %>% group_by(metal) %>% summarise(max(ttc)) %>% arrange( `max(ttc)`)

cambell_lookup$metal <- fct_relevel(cambell_lookup$metal, c('Mercury','Arsenic','Molybdenum','Iron','Zinc','Manganese','Cadmium','Copper','Lead') )
raw_met_data$metal <- fct_relevel(raw_met_data$metal, c('Mercury','Arsenic','Molybdenum','Iron','Zinc','Manganese','Cadmium','Copper','Lead') )

raw_met_data %>% filter(metal == "Mercury" & tp_med > 3 & ttc_raw > 6) 

# facetted
ggplot(cambell_lookup, aes(as.numeric(as.character(tp_med)), ttc*1, color = metal))+ 
  geom_point(data = raw_met_data,aes(x = tp_med, y = ttc_raw, color = metal)) +
  geom_line(size = 2, alpha = .9)+
  geom_hline(yintercept = 1, lty = "dashed")+
  labs(title = "Trophic transfer of an environment level of 1 ppm",
       x = "trophic position", y = "ppm", subtitle = "Cambell et al. 2005")+
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
  #annotate("text", x = 4.9, y = .8, label = "italic('Biodilution')", size = 3, hjust = 1, alpha = .5, parse = T)+
  #annotate("text", x = 4.9, y = 1.2, label = "italic('Biomagnification')", size = 3, hjust = 1, alpha = .5, parse = T)+
  #annotate("text", x = 0, y = 8, label = "italic('Cambell et al. 2005')", size = 3, hjust = 0, alpha = .5, parse = T)+
  #annotate("text", 
  #         x =  mean(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
  #         y = 5, vjust = 0,
  #         label = "seabird trophic range", 
  #         size = 4, hjust = 0, alpha = 1, angle = 90, color = "white")+
  scale_x_continuous(expand = c(0,0), limits = c(-0.2,5))+
  scale_y_continuous( limits = c(-.1,8.1), expand = c(0,0) )+
  scale_color_brewer(palette = "Paired")+
  #scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1,2,3,4,5,6,7,8) )+
  themeo +
  theme(legend.position = c(0.75, 0.1)
        # , text=element_text(size=16, family="Calibri")
  )+
  guides(color=guide_legend(ncol=2))+
  facet_wrap(~metal, ncol = 2)


