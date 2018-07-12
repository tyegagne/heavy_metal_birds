
conc_comp <- read.csv('concentration_compilation.csv', na.strings = '-') %>% 
                                      filter(reference == "Cambell_2005") %>% 
                                      drop_na(concentration)


str(conc_comp)

ggplot(conc_comp,aes(x = published.TL,y = concentration))+
  geom_point()+
  facet_wrap(~Metal, scales = "free_y")

 conc_comp
 par(mfrow = c(4,4))
 metals <- levels(droplevels(conc_comp$Metal))
 
 cambell_lookup <- NULL

for(e in 1:length(metals)){
  
  # this is the real intercept i.e. "environmental level"
  x_base = subset(conc_comp, Metal == metals[e])[,"published.TL"]
  y_base = subset(conc_comp, Metal == metals[e])[,"concentration"]
  
  df <- data.frame(x = x_base,y = y_base)
  
  plot(df$x, df$y, xlab = "trophic position", ylab = "concentration (ppm)",pch = 20, xlim = c(0,5), main = metals[e])
  
  # fit 3 degree spline to raw concentration data
  spline_mod <- smooth.spline(df$x, df$y, df = 3)
  prediction <- predict(object = spline_mod, x = seq(0,5,by = 0.01))
  lines(prediction)
  
  # obtain 0 intercept of the model
  mod_intercept <- as.data.frame(prediction)[1,2]
  mod_intercept
  
  mod_intercept <- ifelse(mod_intercept <= 0,median(df$y), mod_intercept)
  
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
  
  metal_lookup <- data.frame(TL = est_prediction$x,ttc = est_prediction$y,metal = metals[e])
  
  cambell_lookup <- rbind(metal_lookup,cambell_lookup)

}

 
str(cambell_lookup)

b <- ggplot(cambell_lookup, aes(TL, ttc*10))+ # correction shows what 10ppm at base would look like at various TPs
  geom_line()+
  geom_hline(yintercept = 10, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo


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






# plot of magnification

par(mfrow = c(5,1),mar=c(1,1,1,1))

runif(50)

plot(runif(500*5),runif(500*5))
plot(runif(500*4),runif(500*4))
plot(runif(500*3),runif(500*3))
plot(runif(500*2),runif(500*2))
plot(runif(500*1),runif(500*1))
