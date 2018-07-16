
library(ggplot2)
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

# set working directory and read in csv
setwd('/Users/tgagne/heavy_metal_birds/data') # running from Tylers computer
conc_comp <- read.csv('concentration_compilation.csv', na.strings = '-') %>%  # read in the reference from which environmental concentrations where scraped
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

for(e in 1:length(metals)){   # i.e. : for each metal, do this:
  
  # subset on the TL and concentration data for a single metal as an x and a y vector and combine in to a data.frame
  x_base = subset(conc_comp, Metal == metals[e])[,"published.TL"]
  y_base = subset(conc_comp, Metal == metals[e])[,"concentration"]
  df <- data.frame(x = x_base,y = y_base)
  
  # plot this data
  plot(df$x, df$y, xlab = "trophic position", ylab = "concentration (ppm)",pch = 20, xlim = c(0,5), main = metals[e])
  
  # fit 3 degree spline to raw concentration data
  spline_mod <- smooth.spline(df$x, df$y, df = 3)
  prediction <- predict(object = spline_mod, x = seq(0,5,by = 0.01))

  lines(prediction)
  
  # obtain 0 intercept of the model
  mod_intercept <- as.data.frame(prediction)[1,2]
  mod_intercept
  
  # Because baseline concentration cannot be below 0 if recorded in tissues at any concentration, then correct here
  # We need to settle on an approach in order to address intercept, i.e. envrionmental concentration approximation
  # I've currently settled on:
  mod_intercept <- ifelse(mod_intercept <= 0,min(df$y)+mad(df$y), mod_intercept)
  
  # correct the concentration data given the intercepts, modeled vs real
  mod_corrected_TTC <- df$y / mod_intercept
  
  x <- c(0,df$x)
  y <- c(1,mod_corrected_TTC)

  weights <- seq(1,1, length.out = length(x))
  weights[1] <- 1000
  
  est_mod <- smooth.spline(x, y, df = 3, w = weights) 
  est_prediction <- predict(object = est_mod, x = seq(0,5,by = 0.01)) %>% as.data.frame()
  

  plot(x, y, pch = 20, ylab = "transfer coefficient", xlab = "trophic position", xlim = c(0,5))
  lines(est_prediction)
  
  metal_lookup <- data.frame(tp_med = est_prediction$x,ttc = est_prediction$y,metal = metals[e])
  
  cambell_lookup <- rbind(metal_lookup,cambell_lookup)

}
 
cambell_lookup$tp_med <- as.factor(cambell_lookup$tp_med)
# match these lookup metals to our data
 #levels(joined_all$metal) # What want 
 #[1] "Arsenic"    "Cadmium"    "Copper"     "Iron"       "Lead"       "Manganese"  "Mercury"    "Molybdenum" "Zinc"  
 
 levels(cambell_lookup$metal)
 #[1] "Zinc"          "Molybdenum"    "Methylmercury" "Manganese"     "Lead"          "Copper"        "Cadmium"       "Arsenic" 
 cambell_lookup$metal <- plyr::revalue(cambell_lookup$metal, c("Methylmercury"="Mercury"))
 
 # Iron will self represent as Zinc
 Fe <- subset(cambell_lookup, metal == "Zinc"); Fe$metal <- "Iron" %>% as.factor(); str(Fe)
 cambell_lookup <- rbind(cambell_lookup,Fe)
 cambell_lookup$metal <- as.character( cambell_lookup$metal ) %>% as.factor() # reorder alphabetically
 
 #write.csv(cambell_lookup,"cambell_TTC.csv", row.names = F)
 
str(cambell_lookup)

b <- ggplot(cambell_lookup, aes(as.numeric(as.character(tp_med)), ttc*1))+ # correction shows what 10ppm at base would look like at various TPs
  geom_line()+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo


# paper figure
b + 
  annotate("rect",
           xmin = min(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           xmax = max(as.numeric(as.character(joined_all$tp_med)), na.rm = T), 
           ymin = -Inf, 
           ymax = Inf, 
           alpha = .5)+
  facet_wrap(~metal, ncol = 2, scales = "free_y")+
  labs(title = "Trophic transfer of an environment level of 1 ppm", 
       y = "parts per million", 
       x = "trophic position")






# plot of magnification

par(mfrow = c(5,1),mar=c(1,1,1,1))

runif(50)

plot(runif(500*5),runif(500*5))
plot(runif(500*4),runif(500*4))
plot(runif(500*3),runif(500*3))
plot(runif(500*2),runif(500*2))
plot(runif(500*1),runif(500*1))
