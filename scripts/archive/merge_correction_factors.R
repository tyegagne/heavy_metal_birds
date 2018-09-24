# merge corrected estimates from three sources
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

all_corrected <- rbind(gain_corrected,sueddel_corrected,cambell_corrected)

ggplot(all_corrected,aes(x=year,y=corrected_conc,group = spp,color = spp))+
  geom_line()+
  facet_grid(metal~source,scales = "free_y")

all_corrected %>% 
  filter(spp == "BUPE" & metal == "Lead") %>% 
  ggplot(aes(x=year,y=corrected_conc, group = source))+
  geom_point()

all_corrected %>% 
  filter(spp == "BUPE" & metal == "Lead") %>% 
  group_by(year) %>% 
  mutate(min_ppm = min(corrected_conc),
         max_ppm = max(corrected_conc)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_ribbon(aes(x = year, ymin = min_ppm, ymax = max_ppm), alpha = .5)

all_corrected %>% 
  group_by(spp,metal,year) %>% 
  mutate(min_ppm = min(corrected_conc),
         max_ppm = max(corrected_conc)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_ribbon(aes(x = year, ymin = min_ppm, ymax = max_ppm, group = spp, fill = spp), alpha = .5) +
  #facet_grid(metal~spp,scales = "free_y")
  facet_wrap(~metal,scales = "free_y", ncol = 3)+
  scale_fill_manual(values = colorRampPalette(rev(brewer.pal(8, "Accent")))(9))+
  #scale_fill_manual(values = colorRampPalette(rev(brewer.pal(8, "Paired")))(9))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(.2,.5))+
  theme_linedraw()





