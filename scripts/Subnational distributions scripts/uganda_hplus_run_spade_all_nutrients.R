# Running Spade
# Running SPADE: Uganda Harvest Plus data
# File created on 11/10/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc

# Load packages
library(SPADE.RIVMNwCore)
library(here)

SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Uganda"))


# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "uganda"))

summary(uganda_h)

###########################################################
# 1. RUN SPADE FOR B12

# number of intakes per person:
table(table(uganda_h$id))
#  1   2 
# 60 324   This means 60 women with one observation and 324 with 2 observations

# Let's have a look at the highest b12 intakes
sort(uganda_h$b12, decreasing = T)[1:10]

uganda_h$sex[uganda_h$sex==1] <- 2

table(uganda_h$sex)
# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_h_pos <- uganda_h[uganda_h$vitb12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_h_pos$vitb12,  uganda_h_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_h_pos[uganda_h_pos$id == idid,])

summary(tapply(uganda_h_pos$vitb12,  uganda_h_pos$id, var))
# 
# # Remove first 1  
 uganda_h_2 <- uganda_h
 for( idid in names(res)[1:1])
   uganda_h_2 <- uganda_h_2[uganda_h_2$id != idid, ]
 
 # Seems to be 2 observations listed as men
 

range(uganda_h$age)
range(uganda_h$vitb12)

summary(uganda_h_2)
summary(uganda_h)
table(uganda_h$sex)
# 1. RUN FOR B12
uganda_h_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if=vitb12~cs(age), 
        data=uganda_h_2, seed=123, backtrans.nr = 3,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        output.name = "uganda_wom_vitb12")

uganda_vitb12 <- subset(uganda_h_vitb12, select = c(age, HI))
uganda_vitb12 <- uganda_h_vitb12[order(uganda_h_vitb12$age),]

write.csv(uganda_h_vitb12, "all_intakes/uganda_w_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON

summary(uganda_h)

# Within to between ratio is still high
# Let's have a look at the highest b12 intakes
sort(uganda_h$iron, decreasing = T)[1:10]

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_h_pos <- uganda_h[uganda_h$iron > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_h_pos$iron,  uganda_h_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_h_pos[uganda_h_pos$id == idid,])

summary(tapply(uganda_h_pos$iron,  uganda_h_pos$id, var))
# 

# # # Remove first 2  
uganda_h_2_iron <- uganda_h
 for( idid in names(res)[1:2])
   uganda_h_2_iron <- uganda_h_2_iron[uganda_h_2_iron$id != idid, ]

# There are some extremely high iron intakes

uganda_h_2_iron <- subset(uganda_h_2_iron, iron<60)

# RUN FOR WOMEN
uganda_h_iron <- f.spade(frml.ia=iron~fp(age),  frml.if="no.if", 
        data=uganda_h_2_iron, seed=123, backtrans.nr = 3,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        output.name = "uganda_wom_iron")

# within to between ratio is still high (5.77) but might be because of 
# small number of second day recalls
uganda_h_iron <- subset(uganda_h_iron, select = c(age, HI))
uganda_h_iron <- uganda_h_iron[order(uganda_h_iron$age),]


write.csv(uganda_h_iron, "all_intakes/uganda_w_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC

summary(uganda_h)

uganda_h_zinc <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=uganda_h, seed=123, backtrans.nr = 3,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "uganda_wom_zinc")

uganda_h_zinc <- subset(uganda_h_zinc, select = c(age, HI))
uganda_h_zinc <- uganda_h_zinc[order(uganda_h_zinc$age),]

write.csv(uganda_h_zinc, "all_intakes/uganda_w_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

# Some extremely high intakes
uganda_h_vita_1 <- subset(uganda_h, vita<9000)

uganda_h_vita <-f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=uganda_h_vita_1, seed=123, backtrans.nr = 3,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "uganda_w_vita")

uganda_h_vita <- subset(uganda_h_vita, select = c(age, HI))
uganda_h_vita <- uganda_h_vita[order(uganda_h_vita$age),]

write.csv(uganda_h_vita, "all_intakes/uganda_w_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

uganda_h_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
        data=uganda_h, seed=123, backtrans.nr = 3,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "uganda_wom_calc")

uganda_h_calc <- subset(uganda_h_calc, select = c(age, HI))
uganda_h_calc <- uganda_h_calc[order(uganda_h_calc$age),]

write.csv(uganda_h_calc, "all_intakes/uganda_w_calc.csv")

##################################################################

# 6. RUN SPADE FOR OMEGA-3

# Not enough observations to run this for Uganda
range(uganda_h$omega_3)

# Have to use 2-part model 
uganda_h_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age), 
                                   data=uganda_h, seed=123, backtrans.nr = 3,
                                   min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                                   output.name = "uganda_wom_omega_3")

uganda_h_omega_3 <- subset(uganda_h_omega_3, select = c(age, HI))
uganda_h_omega_3 <- uganda_h_omega_3[order(uganda_h_omega_3$age),]

write.csv(uganda_h_omega_3, "all_intakes/uganda_w_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR FIBER

uganda_h_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                         data=uganda_h, seed=123, backtrans.nr = 3,
                         min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                         output.name = "uganda_wom_fiber")

uganda_h_fiber <- subset(uganda_h_fiber, select = c(age, HI))
uganda_h_fiber <- uganda_h_fiber[order(uganda_h_fiber$age),]

write.csv(uganda_h_fiber, "all_intakes/uganda_w_fiber.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

uganda_h_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                           data=uganda_h, seed=123, backtrans.nr = 3,
                           min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                           output.name = "uganda_wom_protein")

uganda_h_protein <- subset(uganda_h_protein, select = c(age, HI))
uganda_h_protein <- uganda_h_protein[order(uganda_h_protein$age),]

write.csv(uganda_h_protein, "all_intakes/uganda_w_protein.csv")

##################################################################

# 9. RUN SPADE FOR FAT

uganda_h_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                           data=uganda_h, seed=123, backtrans.nr = 3,
                           min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                           output.name = "uganda_wom_fat")

uganda_h_fat <- subset(uganda_h_fat, select = c(age, HI))
uganda_h_fat <- uganda_h_fat[order(uganda_h_fat$age),]

write.csv(uganda_h_fat, "all_intakes/uganda_w_fat.csv")

##################################################################

# 9. RUN SPADE FOR CARBOHYDRATE

uganda_h_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                        data=uganda_h, seed=123, backtrans.nr = 3,
                        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                        output.name = "uganda_wom_carb")

uganda_h_carb <- subset(uganda_h_carb, select = c(age, HI))
uganda_h_carb <- uganda_h_carb[order(uganda_h_carb$age),]

write.csv(uganda_h_carb, "all_intakes/uganda_w_carb.csv")

##################################################################

# 10. RUN SPADE FOR CARBOHYDRATE

uganda_h_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                         data=uganda_h, seed=123, backtrans.nr = 3,
                         min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                         output.name = "uganda_wom_carb")

uganda_h_carb <- subset(uganda_h_carb, select = c(age, HI))
uganda_h_carb <- uganda_h_carb[order(uganda_h_carb$age),]

write.csv(uganda_h_carb, "all_intakes/uganda_w_carb.csv")

##################################################################

# 11. RUN SPADE FOR ENERGY

uganda_h_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                          data=uganda_h, seed=123, backtrans.nr = 3,
                          min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "uganda_wom_energy")

uganda_h_energy <- subset(uganda_h_energy, select = c(age, HI))
uganda_h_energy <- uganda_h_energy[order(uganda_h_energy$age),]

write.csv(uganda_h_energy, "all_intakes/uganda_w_energy.csv")

##################################################################

# 12. RUN SPADE FOR VITAMIN C

uganda_h_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                           data=uganda_h, seed=123, backtrans.nr = 3,
                           min.age=20,max.age=67,sex="female", dgts.distr = 2, 
                           output.name = "uganda_wom_vitc")

uganda_h_vitc <- subset(uganda_h_vitc, select = c(age, HI))
uganda_h_vitc <- uganda_h_vitc[order(uganda_h_vitc$age),]

write.csv(uganda_h_vitc, "all_intakes/uganda_w_vitc.csv")

