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
conc_comp <- read.csv('concentration_compilation.csv', na.strings = '-') %>%  # read in the reference from which environmental 
  # concentrations where scraped
  filter(reference == "Cambell_2005") %>% # Cambell 2005 in this case is Baffin Bay pelagic study
  drop_na(concentration)   

par(mfrow = c(4,4))

metals <- levels(droplevels(conc_comp$Metal))

metals = "Arsenic" 
i = 1

for(i in 1:length(metals)){

Cambell_Hg <- subset(conc_comp, Metal == metals[i])
str(Cambell_Hg)

plot(Cambell_Hg$published.TL, Cambell_Hg$concentration, xlim = c(0,5))
plot(Cambell_Hg$published.TL, log(Cambell_Hg$concentration), xlim = c(0,5))

Hg_mod <- lm(log(concentration) ~ published.TL, data = Cambell_Hg)

TL_range <-  data.frame( published.TL = seq(0,5,by = .1))

conc <- predict(object = Hg_mod,newdata = TL_range) %>% data.frame()

lines(TL_range$published.TL, conc$.)

plot(Cambell_Hg$published.TL, Cambell_Hg$concentration, xlim = c(0,5))
lines(TL_range$published.TL, exp(conc$.) ) 


plot(TL_range$published.TL, exp(conc$.) / exp(Hg_mod$coefficients[1])*1, col = "blue" , type = "l", main = metals[i],xlim = c(0,5)) 
points(Cambell_Hg$published.TL, Cambell_Hg$concentration / exp(Hg_mod$coefficients[1]), col = "blue") 
lines(TL_range$published.TL, exp(conc$.) / exp(Hg_mod$coefficients[1])+1000)

Hg_mod$coefficients[2]
Hg_mod$coefficients


}



