# Running SPADE: Mexico data
# File created on 11/23/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "mexico"))


###########################################################
# Make separate datasets for men and women
mexico_wom <- mexico_spade %>% 
  filter(sex==2) 

mexico_men <- mexico_spade %>% 
  filter(sex==1) 


###########################################################
# 1. RUN SPADE FOR B12

# Variance is too low: work with the exceedingly high entries

# Let's have a look at the highest b12 intakes
summary(mexico_wom)


mexico_b12 <- f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), 
                      data=mexico_wom_2, seed=123,  backtrans.nr = 3,
                      min.age=18,max.age=67,sex="female", dgts.distr = 2,  
                      age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
                      output.name = "mexico_wom_b12",
                      spade.output.path = "output/b12_SPADE_mexico/")

#Using fitdist code from Alon
q <- quantile(tmpageclassHI, c(0.05,0.1,0.25,0.50,0.75,0.9,0.95))
p <- c(0.05,0.1,0.25,0.50,0.75,0.9,0.95)

#It says that Weibull, lognormal, and gamma all worked

fit.results <- rriskFitdist.perc(p,q, show.output = TRUE)
plotDiagnostics.perc(fit.results)

# Using code from Arnold to plot the back transofrmation (will have to do this for each age group)
head(mexico_b12)
tmpageclassHI <- mexico_b12[mexico_b12$age >= 18 & mexico_b12$age <= 67, "HI"]
d <- density(tmpageclassHI)
plot(d)
plot(d, frame = FALSE, col = "steelblue", 
     main = "Density") 
plot2 <- abline(v = mean(tmpageclassHI), col = "red", lwd = 2)

##################################################################

# 2. RUN SPADE FOR IRON

summary(mexico_wom)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
        data=mexico_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "mexico_wom_iron",
        spade.output.path = "output/iron_SPADE_mexico/")

##################################################################

# 3. RUN SPADE FOR ZINC

summary(mexico_wom)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=mexico_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "mexico_wom_zinc",
        spade.output.path = "output/zinc_SPADE_mexico/")

##################################################################

# 4. RUN SPADE FOR VIT A

f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=mexico_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "mexico_wom_vita",
        spade.output.path = "output/vita_SPADE_mexico/")

##################################################################

# 5. RUN SPADE FOR CALCIUM

f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
        data=mexico_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "mexico_wom_calc",
        spade.output.path = "output/calc_SPADE_mexico/")

##################################################################

# 6. RUN SPADE FOR RED MEAT

# Have to use two part model because so many zeroes
f.spade(frml.ia=red_meat~fp(age),  frml.if=red_meat~cs(age), 
        data=mexico_wom, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "mexico_wom_calc",
        spade.output.path = "output/red_meat_SPADE_mexico/")