install.packages("MASS")
library(ggplot2)
library(MASS)

# log normal distribution
curve(dlnorm(x, meanlog=2.48, sdlog=0.916), from=0, to=30)
abline(v=18, col="red")
plnorm(18, meanlog=2.48, sdlog=0.916)


# normal distribution

x <- seq(0, 30, length=1000)

y <- dnorm(x, mean=12, sd=2.5)



plot(x, y, type="l")

plot(x,y, type = "l",  xlim = c(0, 30))
abline(v=18, col="red")

pnorm(18, mean=12, sd=2.5)
