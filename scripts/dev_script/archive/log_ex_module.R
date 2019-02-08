log(10)
log(0.1)
exp(2.302585)
exp(-2.302585)

exp(log(10))
exp(log(0.1))

log10(10)
log10(0.1)

10^log10(10)
10^log10(.1)

10^0.001
10^0.01
10^1

e^0.01
e^0.1
e^1
e^2

x <- seq(0,5,length.out = 100)
y_ln <- exp(jitter(x, factor = 10))-.5
#y_log10 <- 10^(jitter(x, factor = 10))



plot(x,y_ln, type = "l")
#lines(x,y_log10, col = "red")

plot(x, log(y_ln), type = "l")
#lines(x,log10(y_log10), col = "red")
#lines(x, log10(y_ln))
#lines(x, log(y_log10))


ln_df <- data.frame(x = x, y_ln = y_ln)
str(ln_df)

ggplot(ln_df, aes(x,y_ln))+
  geom_point()

ggplot(ln_df,aes(x, log(y_ln)))+
  geom_point()

library(scales)
ggplot(ln_df,aes(x, log(y_ln)))+
  geom_point()+
#  scale_y_continuous(breaks = trans_breaks("log", function(x) exp(x)),
#                     labels = trans_format("log", math_format(e^.x))) +

scale_y_continuous(breaks = trans_breaks("log10", function(x) 10 ^ x),
                   labels = trans_format("log10", math_format(10^.x)))




x <- seq(0,100, length.out = 100)
y <- seq(0,100, length.out = 100)


plot(x,y) 

plot(log(x),log(y) )
plot(x^10,y^10)
plot(exp(x),exp(y))
plot(log(x^10),log(y^10))

plot(x,log(y))
plot(x,y^10)
plot(x,log(y^10))
plot(x,exp(y))

                    