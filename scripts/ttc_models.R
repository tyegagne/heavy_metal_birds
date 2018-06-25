library(ggplot2)
library(gtools)
library(minpack.lm) 

setwd('/Users/tgagne/heavy_metal_birds/data') # running from Tylers computer
TTC <- read.csv('SuedelTTC.csv')

str(TTC)

ggplot(TTC,aes(x = TL, y = ttc))+
  geom_point()+
  facet_wrap(~metal, scales = "free_y")

metals <- levels(TTC$metal)

metals <- metals[c(1,2,4,5,6,7)]
metals

par(mfrow = c(6,3))

par(mfrow = c(6,7),
    oma = c(5,4,0,0) + 0.3,
    mar = c(0,0,1,1) + 1.2)

lookup_table <- NULL

for(i in 1:length(metals)){

TTC_sub <- subset(TTC, metal == metals[i])

starters <- coef(lm(logit(ttc/100) ~ TL, data = TTC_sub))
TTC_form <- ttc~phi1/(1+exp(-(phi2+phi3*TL)))
start <- list(phi1=30,
           phi2=starters[1] ,
           phi3=starters[2])

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

a <- ggplot(lookup_table, aes(TL, ttc))+
  geom_line()+
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1 )+
  themeo

b <- ggplot(lookup_table, aes(TL, ttc*10))+
  geom_line()+
  geom_hline(yintercept = 10, lty = "dashed")+
  facet_wrap(~metal, scales = "free_y", ncol = 1)+
  themeo

gridExtra::grid.arrange(a,b, ncol = 2)


















