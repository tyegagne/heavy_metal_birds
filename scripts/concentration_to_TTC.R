
library(dplyr)

x=runif(50)
y=exp(2*x)+rnorm(50)
x <- c(0, 3.2, 3.6, 2.6, 3.6, 3.0, 2.0, 3.3, 1.0, 1.0)
y <- c(1, 0.3, 0.9, 0.1, 0.9, 2.6, 0.4, 0.7, 0.5, 0.4)
df <- data.frame(x = x,y = y)
plot(df$x,df$y)
abline(h = 1, lty = "dashed")
weights <- c(100, 1, 1, 1, 1, 1, 1, 1, 1, 1)
spline_mod <- smooth.spline(x = df$x, y = df$y,w = weights, df = 5)
prediction <- predict(object = spline_mod, x = seq(0,5,length.out = 100))
lines(prediction$x,prediction$y)



#
TTC <- read.csv('SuedelTTC.csv')

str(TTC)

metals <- levels(TTC$metal)
#metals <- metals[c(1,2,4,5,6,7)]
lookup_table <- NULL
par(mfrow = c(3,3))

for(i in 1:length(metals)){
  
  # subset a single metal from the TTC table
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
  
  # dataframe of predictions of TTC accross range of TP i.e. 0 - 5
  newdata <- data.frame(TL = seq(0,5, by = .01),ttc = prediction$y, metal = metals[i])
  
  lines(newdata$TL,newdata$ttc)
  
  # build dataframe of all metal's TTCs
  lookup_table <- rbind(lookup_table,newdata)
  
}

# Iron and Mg will self represent as Zinc
Mn <- subset(lookup_table, metal == "Zinc"); Mn$metal <- "Manganese" %>% as.factor(); str(Mn)
Fe <- subset(lookup_table, metal == "Zinc"); Fe$metal <- "Iron" %>% as.factor(); str(Fe)

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




# dummy data attempt to correct concentration data in to trophic transfer coefficient data
par(mfrow = c(4,2))

n = 15
# generate a plot dummy concentration data
x_base=runif(n , min = 0, max = 5)

ya = exp(1.5*x_base)+rnorm(n , sd = 200)+real_intercept # exponential increasing
yb = -exp(1.2*x_base)+rnorm(n , sd = 100)+real_intercept # exponential degreasing
yc = (5*x_base)+rnorm(n , sd = 20)+real_intercept # linear increasing 
yd = rnorm(n , sd = 20) + real_intercept

equations <- cbind(ya,yb,yc,yd) %>%  as.data.frame()

for(e in 1:length(equations)){
  

# this is the real intercept i.e. "environmental level"
real_intercept <- 500

y_base = equations[e] %>% as.vector() 

df <- data.frame(x_base,y_base)
plot(df, xlab = "trophic position", ylab = "concentration (ppm)",pch = 20)

# fit 3 degree spline to raw concentration data
spline_mod <- smooth.spline(df$x, df$y, df = 3)
prediction <- predict(object = spline_mod, x = seq(0,5,by = 0.01))
lines(prediction)

# obtain 0 intercept of the model
mod_intercept <- as.data.frame(prediction)[1,2]
mod_intercept


# correct the concentration data given the intercepts, modeled vs real
mod_corrected_TTC <- df$y / mod_intercept
real_corrected_TTC <- df$y / real_intercept


x <- c(0,df$x)
y <- c(1,mod_corrected_TTC)
y2 <- c(1,real_corrected_TTC)

weights <- seq(1,1, length.out = length(x))
weights[1] <- 1000

est_mod <- smooth.spline(x, y, df = 3, w = weights) 
est_prediction <- predict(object = est_mod, x = seq(0,5,by = 0.01)) %>% as.data.frame()

real_mod <- smooth.spline(x, y2, df = 3)
real_prediction <- predict(object = real_mod, x = seq(0,5,by = 0.01)) %>% as.data.frame()

plot(x, y, pch = 20, ylab = "transfer coefficient", xlab = "trophic position")
points(x, y2, col = "green4", pch = 20)
lines(est_prediction)
lines(real_prediction, col = "green4")

}












