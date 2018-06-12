# load libraries
library(lubridate)
library(ggplot2)

##############################
###    T's ggPlot theme    ###
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


# read in csv
teeth <- read.csv('/Users/tgagne/Downloads/tooth_eruption.csv')
str(teeth)

teeth <- teeth %>% 
  mutate(Lc1 = fct_recode(Lc1, "d" = "ed")) %>% 
  mutate(Rc1 = fct_recode(Rc1, "d" = "ed")) 

mand_teeth <- filter(teeth, maxormand == "mand")
colnames(mand_teeth)[8:25] <- paste0(colnames(mand_teeth)[8:25],"_mand")

maxi_teeth <- filter(teeth, maxormand == "max")
colnames(maxi_teeth)[8:25] <- paste0(colnames(maxi_teeth)[8:25],"_max")

teeth_wide <- merge(maxi_teeth,mand_teeth, by = c("otter","date","sex","bdate","age_days","obs") )

teeth_wide$maxormand.x <- NULL
teeth_wide$maxormand.y <- NULL

str(teeth_wide)

teeth_tall <- teeth_wide %>% select(otter,sex,age_days,7:42) %>% 
  gather(key = tooth, value = measurement, 4:39) 

str(teeth_tall)

teeth_tall$tooth <- as.factor(teeth_tall$tooth)
teeth_tall$measurement <- as.factor(teeth_tall$measurement)

teeth_tall <- teeth_tall %>% 
  mutate(measurement = fct_recode(measurement, "pA" ="d/pA" )) %>% 
  mutate(measurement = fct_recode(measurement, "eA" = "d/eA")) %>% 
  mutate(measurement = fct_recode(measurement, "fA" = "d/fA")) %>% 
  mutate(measurement = fct_recode(measurement, "fA" = "fa")) %>% 
  mutate(measurement = fct_recode(measurement, "pA" = "PA")) %>% 
  mutate(measurement = fct_recode(measurement, "d" = "pd")) %>% 
  mutate(measurement = fct_recode(measurement, "nto" = "ntor")) %>% 
  mutate(measurement = fct_recode(measurement, "d" = "s")) %>% 
  mutate(measurement = fct_recode(measurement, "d" = "fd")) %>% 
  mutate(measurement = fct_recode(measurement, "nto" = "")) 

  
teeth_tall <- teeth_tall[complete.cases(teeth_tall), ]# remove rows of mismatched NA, listwise deletion #

teeth_tall$measurement <- droplevels(teeth_tall$measurement)

levels(teeth_tall$measurement)
noquote((paste("'",levels(teeth_tall$tooth),"'",",")))

teeth_tall$measurement <- factor(teeth_tall$measurement, levels = c("nto","ed","d","pe","eA","pA","fA"), ordered = T)


levels_ord <- c('Lc1_mand' ,  'Rc1_mand' , 
  'Lc1_max' ,  'Rc1_max' ,
  'Li1_mand' ,'Ri1_mand' ,
  'Li1_max' ,'Ri1_max' ,
  'Li2_mand' , 'Ri2_mand' ,
  'Li2_max' , 'Ri2_max' ,
  'Li3_mand' ,'Ri3_mand' ,
  'Li3_max' ,'Ri3_max' ,
  'Lm1_mand' ,'Rm1_mand' ,
  'Lm1_max' ,'Rm1_max' ,
  'Lm2_mand' ,'Rm2_mand' ,
  'Lm2_max' ,'Rm2_max' ,
  'Lpm2_mand' ,'Rpm2_mand' ,
  'Lpm2_max' , 'Rpm2_max' ,
  'Lpm3_mand' ,'Rpm3_mand' ,
  'Lpm3_max' , 'Rpm3_max' ,
  'Lpm4_mand' ,'Rpm4_mand' ,
  'Lpm4_max' ,'Rpm4_max' )
  
            
           
                
           


teeth_tall$tooth <- factor(teeth_tall$tooth, levels = levels_ord, ordered = T)



ggplot(teeth_tall, aes(x = age_days, y = as.numeric(measurement)))+
  geom_point(color = "gray", size = .5, show.legend = F)+
  geom_smooth(se = T, color = "black", span = .25) +
  facet_wrap(~tooth, scales = "free_y")+
  themeo

teeth_tall$age_days_cut <- as.numeric(cut_number(teeth_tall$age_days,36))

ggplot(teeth_tall, aes(x = age_days_cut, y = as.numeric(measurement)))+
  geom_point(color = "gray", size = .5, show.legend = F)+
  geom_smooth(se = T, color = "black", span = .25) +
  facet_wrap(~tooth, scales = "free_y")+
  themeo

ggplot(teeth_tall,aes( x = age_days_cut, fill = measurement)) + 
  geom_bar(position = "fill", width = 0.95)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0.01,0.01))+
  themeo

ggplot(teeth_tall,aes( x = age_days_cut, fill = measurement)) + 
  geom_bar(position = "fill", width = 0.95)+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~tooth, ncol = 2)+
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0.01,0.01))+
  themeo


# combine left and right

















