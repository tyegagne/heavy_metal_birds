library(ggplot2)
library(gtools)
library(minpack.lm) 

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

setwd('/Users/tgagne/heavy_metal_birds/data') # running from Tylers computer
TTC <- read.csv('SuedelTTC.csv')

str(TTC)

ggplot(TTC,aes(x = TL, y = ttc))+
  geom_point()+
  facet_wrap(~metal, scales = "free_y")

metals <- levels(TTC$metal)
metals <- metals[c(1,2,4,5,6,7)]
# <- "Copper"

par(mfrow = c(6,3))

#par(mfrow = c(6,3), oma = c(5,4,0,0) + 0.3, mar = c(0,0,1,1) + 1.2)

lookup_table <- NULL

for(i in 1:length(metals)){

TTC_sub <- subset(TTC, metal == metals[i])

starters <- coef(lm(logit(ttc/100) ~ TL, data = TTC_sub))
TTC_form <- ttc~phi1/(1+exp(-(phi2+phi3*TL)))
start <- list(phi1=30,
           phi2=starters[1] + .001 ,
           phi3=starters[2] + .001)

fitTypical <- nlsLM(TTC_form, data=TTC_sub, start=start, trace = T)

newdata <- data.frame(TL = seq(0,5, by = .01),ttc = predict(fitTypical, newdata = data.frame(TL = seq(0,5, by = .01))), metal = metals[i])

lookup_table <- rbind(lookup_table,newdata)

plot(ttc ~ TL, data = TTC_sub, main = metals[i], xlim = c(0,5))
lines(ttc ~ TL, data = newdata)
#abline(h = 1, lty = 'dashed')

plot(ttc ~ TL, data = newdata, type = 'l', main = paste(metals[i], 'TTC by TL'), ylab = "TTC")
abline(h = 1, lty = 'dashed')

#plot(x = newdata$TL, y = newdata$ttc/10, type = 'l', main = "TTC / 10 ppm")
#plot(x = newdata$TL, y = 10/newdata$ttc, type = 'l', main = "10 ppm / TTC")
#plot(x = newdata$TL, y = (newdata$TL*newdata$ttc)/10, type = 'l', main = "TL*TTC / 10 ppm")
#plot(x = newdata$TL, y = 10/(newdata$TL*newdata$ttc), type = 'l', main = "10 ppm / TL * TTC")
#plot(x = newdata$TL, y = log(10^(newdata$TL*newdata$ttc)), type = 'l', main = "log( 10 ppm ^ TL*TTC )")
#plot(x = newdata$TL, y = (newdata$ttc*newdata$TL)*10, type = 'l', main = "TTC * TL * 10 ppm")
plot(x = newdata$TL, y = newdata$ttc*10, type = 'l', main = "TTC * 10 ppm")

}

str(lookup_table)

# Copper is cadmium, and Iron and Mg will self represent as Zinc
Cu <- subset(lookup_table, metal == "Cadmium"); Cu$metal <- "Copper" %>% as.factor(); str(Cu)
Mn <- subset(lookup_table, metal == "Zinc"); Mn$metal <- "Manganese" %>% as.factor(); str(Mn)
Fe <- subset(lookup_table, metal == "Zinc"); Fe$metal <- "Iron" %>% as.factor(); str(Fe)

lookup_table <- rbind(lookup_table,Cu,Mn,Fe)


str(lookup_table)

a <- ggplot(lookup_table, aes(TL, ttc))+
  geom_line()+
  geom_point(data = TTC,aes(x = TL, y = ttc), size = 1)+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1 )+
  themeo

b <- ggplot(lookup_table, aes(TL, ttc*10))+
  geom_line()+
  geom_hline(yintercept = 10, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo

gridExtra::grid.arrange(a,b, ncol = 2)

levels(lookup_table$metal)
levels(joined_all$metal)

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

str(joined_all)
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

joined_all <- left_join(joined_all,lookup_table, by = c("metal","tp_med"))
corrected <- joined_all %>% mutate(corrected_metal_level = interp_levels * ttc)


ggplot(corrected )+
  geom_point(aes(x = year, y = (corrected_metal_level), color = spp))+
  #geom_line(aes(x = year, y = interp_levels, color = spp))+
  facet_wrap(~metal, scales = "free_y")+
  scale_y_continuous(limits = c(0,1))
  
  
ggplot(corrected )+
  geom_point(aes(x = year, y = (corrected_metal_level), color = metal))+
  #geom_line(aes(x = year, y = interp_levels, color = spp))+
  facet_wrap(~spp, scales = "free_y")
  
  
  ggplot(corrected,aes(x = year, y = corrected_metal_level, group = spp) )+
           geom_line()+
           facet_wrap(~metal, scales = "free_y", ncol = 1)
  
  
  
# To Do: 
# z -score filter/adjustment
# running post- 1980
# ppm global generation
# metal ensembles
# shade TP in TTC figures
  



