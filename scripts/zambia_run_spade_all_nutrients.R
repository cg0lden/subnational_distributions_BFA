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
        spade.output.path = "output/zambia/")

zambia_b12 <- subset(zambia_b12, select = c(age, HI))
zambia_b12 <- zambia_b12[order(zambia_b12$age),]

write.csv(zambia_b12, "all_intakes/zambia_w_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON

summary(zambia_wom)

# only positive intake values for iron so use 1 part model
zambia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_iron",
        spade.output.path = "output/zambia/")

zambia_iron <- subset(zambia_iron, select = c(age, HI))
zambia_iron <- zambia_iron[order(zambia_iron$age),]

write.csv(zambia_iron, "all_intakes/zambia_w_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC

summary(zambia_wom)

# only positive intake values for iron so use 1 part model
zambia_zinc <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_zinc",
        spade.output.path = "output/zambia/")

zambia_zinc <- subset(zambia_zinc, select = c(age, HI))
zambia_zinc <- zambia_zinc[order(zambia_zinc$age),]

write.csv(zambia_zinc, "all_intakes/zambia_w_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

zambia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_vita",
        spade.output.path = "output/zambia/")

zambia_vita <- subset(zambia_vita, select = c(age, HI))
zambia_vita <- zambia_vita[order(zambia_vita$age),]

write.csv(zambia_vita, "all_intakes/zambia_w_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

zambia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_calc",
        spade.output.path = "output/zambia/")

zambia_calc <- subset(zambia_calc, select = c(age, HI))
zambia_calc <- zambia_calc[order(zambia_calc$age),]

write.csv(zambia_calc, "all_intakes/zambia_w_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

# Have to use two part model because so many zeroes
zambia_red_meat <- f.spade(frml.ia=red_meat~fp(age),  frml.if=red_meat~cs(age), 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_red_meat",
        spade.output.path = "output/zambia/")

zambia_red_meat <- subset(zambia_red_meat, select = c(age, HI))
zambia_red_meat <- zambia_red_meat[order(zambia_red_meat$age),]


write.csv(zambia_red_meat, "all_intakes/zambia_w_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

# there aren't enough observations for processed meat to estimate a distribution

# Have to use two part model because so many zeroes
zambia_processed_meat <- f.spade(frml.ia=processed_meat~fp(age),  frml.if=processed_meat~cs(age), 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
        output.name = "Zambia_wom_processed_meat",
        spade.output.path = "output/zambia/")

zambia_processed_meat <- subset(zambia_processed_meat, select = c(age, HI))
zambia_processed_meat <- zambia_processed_meat[order(zambia_processed_meat$age),]


write.csv(zambia_processed_meat, "all_intakes/zambia_w_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA-3

# there aren't enough observations for omega_3 to estimate a distribution

# Have to use two part model because so many zeroes
zambia_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age), 
                                 data=zambia_wom, seed=123, backtrans.nr = 3,
                                 min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                                 age.classes=c(18, 20, 25, 29, 34, 39, 44, 49, 54, 59, 64),
                                 output.name = "Zambia_wom_omega_3",
                                 spade.output.path = "output/zambia/")

zambia_omega_3 <- subset(zambia_omega_3, select = c(age, HI))
zambia_omega_3 <- zambia_omega_3[order(zambia_omega_3$age),]


write.csv(zambia_omega_3, "all_intakes/zambia_w_omega_3.csv")