# merge corrected estimates from three sources
str(corrected)

sueddel_corrected <- data.frame(year = corrected$year,
                                spp = corrected$spp,
                                metal = corrected$metal,
                                corrected_conc = corrected$corrected_metal_level,source = "sueddel")
#write.csv(sueddel_corrected,"sueddel_merge.csv")

cambell_corrected <- data.frame(year = corrected$year,
                                spp = corrected$spp,
                                metal = corrected$metal,
                                corrected_conc = corrected$corrected_metal_level,source = "cambell")
#write.csv(cambell_corrected,"cambell_merge.csv")

gain_corrected <- data.frame(year = corrected$year,
                             spp = corrected$spp,
                             metal = corrected$metal,
                             corrected_conc = corrected$corrected_metal_level,source = "gain")

#write.csv(gain_corrected,"gain_merge.csv")

gain_corrected <- read.csv("gain_merge.csv")
sueddel_corrected <- read.csv("sueddel_merge.csv")
cambell_corrected <- read.csv("cambell_merge.csv")

str(gain_corrected)
str(sueddel_corrected)
str(cambell_corrected)

all_corrected <- rbind(gain_corrected,sueddel_corrected,cambell_corrected)

str(all_corrected)

ggplot(all_corrected,aes(x=year,y=corrected_conc,group = spp,color = spp))+
  geom_line()+
  facet_wrap(metal~source,scales = "free_y")






