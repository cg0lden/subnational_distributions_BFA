# Running SPADE: Zambia data
# File created on 11/9/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "zambia_wom"))


###########################################################
# 1. RUN SPADE FOR B12

# Variance is too low: work with the exceedingly high entries


# Let's have a look at the highest b12 intakes

summary(zambia_wom)

# number of intakes per person:
table(table(zambia_wom$id))
#  1   2 
# 60 324   This means 60 women with one observation and 324 with 2 observations

# Let's have a look at the highest b12 intakes
sort(zambia_wom$b12, decreasing = T)[1:10]
#  [1] 150.06  62.14  58.79  38.48  38.21  36.60  33.76  31.64  31.60  27.72

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
zambia_wom_pos <- zambia_wom[zambia_wom$b12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(zambia_wom_pos$b12,  zambia_wom_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(zambia_wom_pos[zambia_wom_pos$id == idid,])

summary(tapply(zambia_wom_pos$b12,  zambia_wom_pos$id, var))

# Remove first 5 
zambia_wom_2 <- zambia_wom
for( idid in names(res)[1:5])
  zambia_wom_2 <- zambia_wom_2[zambia_wom_2$id != idid, ]

zambia_wom_2 <- zambia_wom_2[zambia_wom_2$b12 <100,]

zambia_b12 <- f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), 
        data=zambia_wom_2, seed=123,  backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,  
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_b12",
        spade.output.path = "output/b12_SPADE_zambia/")

#Using fitdist code from Alon
q <- quantile(tmpageclassHI, c(0.05,0.1,0.25,0.50,0.75,0.9,0.95))
p <- c(0.05,0.1,0.25,0.50,0.75,0.9,0.95)

#It says that Weibull, lognormal, and gamma all worked

fit.results <- rriskFitdist.perc(p,q, show.output = TRUE)
plotDiagnostics.perc(fit.results)

# Using code from Arnold to plot the back transofrmation (will have to do this for each age group)
head(zambia_b12)
tmpageclassHI <- zambia_b12[zambia_b12$age >= 18 & zambia_b12$age <= 67, "HI"]
d <- density(tmpageclassHI)
plot(d)
plot(d, frame = FALSE, col = "steelblue", 
     main = "Density") 
plot2 <- abline(v = mean(tmpageclassHI), col = "red", lwd = 2)

##################################################################

# 2. RUN SPADE FOR IRON

summary(zambia_wom)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_iron",
        spade.output.path = "output/iron_SPADE_zambia/")

##################################################################

# 3. RUN SPADE FOR ZINC

summary(zambia_wom)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_zinc",
        spade.output.path = "output/zinc_SPADE_zambia/")

##################################################################

# 4. RUN SPADE FOR VIT A

f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_vita",
        spade.output.path = "output/vita_SPADE_zambia/")

##################################################################

# 5. RUN SPADE FOR CALCIUM

f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_calc",
        spade.output.path = "output/calc_SPADE_zambia/")

##################################################################

# 6. RUN SPADE FOR RED MEAT

# Have to use two part model because so many zeroes
f.spade(frml.ia=red_meat~fp(age),  frml.if=red_meat~cs(age), 
        data=zambia_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_calc",
        spade.output.path = "output/red_meat_SPADE_zambia/")

