library(ggplot2)
library(dplyr)
library(gridExtra)
# simulation examples

# time sequence
time <- seq(1890,2015,1)

# constant_constant
constant_constant <- data.frame(time = time, 
                                tp = .5, metal = .75, 
                                relationship = 'constant_constant')
# constant_increasing
constant_increasing <- data.frame(time = time, 
                                  tp = .5, 
                                  metal = seq(from = .5, to = .75, length.out = length(time)), 
                                  relationship = 'constant_increasing')
# increasing_constant
increasing_constant <- data.frame(time = time, 
                                  tp = seq(from = .5, to = .75, length.out = length(time)), 
                                  metal = .75, 
                                  relationship = 'increasing_constant')
# constant_decreasing
constant_decreasing <- data.frame(time = time, 
                                  tp = .5, 
                                  metal = seq(from = .75, to = .5, length.out = length(time)), 
                                  relationship = 'constant_decreasing')
# decreasing_constant
decreasing_constant <- data.frame(time = time, 
                                  tp = seq(from = .75, to = .5, length.out = length(time)),
                                  metal = .75, 
                                  relationship = 'decreasing_constant')
# decreasing_increasing
decreasing_increasing <- data.frame(time = time, 
                                    tp = seq(from = .75, to = .5, length.out = length(time)), 
                                    metal = seq(from = .5, to = .75, length.out = length(time)), 
                                    relationship = 'decreasing_increasing')
# increasing_decreasing
increasing_decreasing <- data.frame(time = time, 
                                    tp = seq(from = .5, to = .75, length.out = length(time)), 
                                    metal = seq(from = .75, to = .5, length.out = length(time)), 
                                    relationship = 'increasing_decreasing')
# increasing_increasing_more
increasing_increasing_more <- data.frame(time = time, 
                                         tp = seq(from = .4, to = .6, length.out = length(time)),
                                         metal = seq(from = .6, to = .7, length.out = length(time)),
                                         relationship = 'increasing_increasing_more')
# decreasing_decreasing_less
decreasing_decreasing_less <- data.frame(time = time, 
                                         tp = seq(from = .6, to = .4, length.out = length(time)),
                                         metal = seq(from = .7, to = .6, length.out = length(time)),
                                         relationship = 'decreasing_decreasing_less')
# bind the scenarios together
scenarios <- rbind(constant_constant,
                   constant_increasing,
                   increasing_constant,
                   constant_decreasing,
                   decreasing_constant,
                   decreasing_increasing,
                   increasing_decreasing,
                   increasing_increasing_more,
                   decreasing_decreasing_less)

line_size = 1

stock <- ggplot(scenarios,aes(time,color = relationship))+
  geom_line(aes(y = tp), size = line_size, show.legend = F)+
  geom_line(aes(y = metal), lty = "dotted", size = line_size, show.legend = F)+
  facet_wrap(~relationship, ncol = 1) +
  scale_y_continuous(limits = c(.4,.8))+
  themeo
#stock

# example corrections/fractionation adjustments
scenarios$additive <- ( scenarios$metal - scenarios$tp  )
scenarios$multiplicative <- ( scenarios$metal / scenarios$tp )
scenarios$exponential <- ( scenarios$metal ^ scenarios$tp  )

scenarios <- gather(scenarios, key = "adjustment", value = "adjust_value", c("additive","multiplicative","exponential"))

str(scenarios)

tp <- ggplot(scenarios,aes(x = time))+
  geom_line(aes(y = adjust_value, color = adjustment))+
  facet_wrap( ~ relationship, ncol = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

adj <- ggplot(scenarios,aes(x = time))+
  geom_line(aes(y = tp))+
  geom_line(aes(y = metal), lty = "dotted")+
  facet_wrap( ~ relationship, ncol = 1)+
  theme(axis.title.y=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.ticks.y=element_blank())

grid.arrange(adj,tp , ncol = 2)



