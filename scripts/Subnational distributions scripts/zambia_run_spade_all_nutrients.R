# Running SPADE: Zambia data
# File created on 11/9/20 by Simone Passarelli
# Updated 3/15/21 to reflect 

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "zambia_wom"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Zambia"))


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
sort(zambia_wom$vitb12, decreasing = T)[1:10]
#  [1] 150.06  62.14  58.79  38.48  38.21  36.60  33.76  31.64  31.60  27.72

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
zambia_wom_pos <- zambia_wom[zambia_wom$vitb12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(zambia_wom_pos$vitb12,  zambia_wom_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(zambia_wom_pos[zambia_wom_pos$id == idid,])

summary(tapply(zambia_wom_pos$vitb12,  zambia_wom_pos$id, var))
summary(zambia_wom_2)
##################################################################
# 1. Run SPADE for Vitamin B12
# Remove first 5 

zambia_wom_2 <- zambia_wom
for( idid in names(res)[1:5])
  zambia_wom_2 <- zambia_wom_2[zambia_wom_2$id != idid, ]

zambia_wom_2 <- zambia_wom_2[zambia_wom_2$vitb12 <100,]

zambia_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if=vitb12~cs(age), 
        data=zambia_wom_2, seed=123,  backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,  
        output.name = "Zambia_wom_b12")

zambia_vitb12 <- subset(zambia_vitb12, select = c(age, HI))
zambia_vitb12 <- zambia_vitb12[order(zambia_vitb12$age),]

write.csv(zambia_vitb12, "all_intakes/zambia_w_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON

summary(zambia_wom)

# only positive intake values for iron so use 1 part model
zambia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2,
        output.name = "Zambia_wom_iron")

zambia_iron <- subset(zambia_iron, select = c(age, HI))
zambia_iron <- zambia_iron[order(zambia_iron$age),]

write.csv(zambia_iron, "all_intakes/zambia_w_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC

# only positive intake values for iron so use 1 part model
zambia_zinc <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "Zambia_wom_zinc")

zambia_zinc <- subset(zambia_zinc, select = c(age, HI))
zambia_zinc <- zambia_zinc[order(zambia_zinc$age),]

write.csv(zambia_zinc, "all_intakes/zambia_w_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

zambia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "Zambia_wom_vita")

zambia_vita <- subset(zambia_vita, select = c(age, HI))
zambia_vita <- zambia_vita[order(zambia_vita$age),]

write.csv(zambia_vita, "all_intakes/zambia_w_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

zambia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
        data=zambia_wom, seed=123, backtrans.nr = 3,
        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
        output.name = "Zambia_wom_calc")

zambia_calc <- subset(zambia_calc, select = c(age, HI))
zambia_calc <- zambia_calc[order(zambia_calc$age),]

write.csv(zambia_calc, "all_intakes/zambia_w_calc.csv")
##################################################################

# 6. RUN SPADE FOR OMEGA-3

# there aren't enough observations for omega_3 to estimate a distribution

# Have to use two part model because so many zeroes
zambia_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age), 
                                 data=zambia_wom, seed=123, backtrans.nr = 3,
                                 min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                                 output.name = "Zambia_wom_omega_3")

zambia_omega_3 <- subset(zambia_omega_3, select = c(age, HI))
zambia_omega_3 <- zambia_omega_3[order(zambia_omega_3$age),]


write.csv(zambia_omega_3, "all_intakes/zambia_w_omega_3.csv")
##################################################################
# 7. RUN SPADE FOR VITAMIN C

zambia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_vitc")

zambia_vitc <- subset(zambia_vitc, select = c(age, HI))
zambia_vitc <- zambia_vitc[order(zambia_vitc$age),]

write.csv(zambia_vitc, "all_intakes/zambia_w_vitc.csv")
##################################################################
#8. RUN SPADE FOR THIAMIN

zambia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_thia")

zambia_thia <- subset(zambia_thia, select = c(age, HI))
zambia_thia <- zambia_thia[order(zambia_thia$age),]

write.csv(zambia_thia, "all_intakes/zambia_w_thia.csv")

##################################################################
#9. RUN SPADE FOR RIBOFLAVIN

zambia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_ribo")

zambia_ribo <- subset(zambia_ribo, select = c(age, HI))
zambia_ribo <- zambia_ribo[order(zambia_ribo$age),]

write.csv(zambia_ribo, "all_intakes/zambia_w_ribo.csv")

##################################################################
#10. RUN SPADE FOR NIACIN

zambia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_niac")

zambia_niac <- subset(zambia_niac, select = c(age, HI))
zambia_niac <- zambia_niac[order(zambia_niac$age),]

write.csv(zambia_niac, "all_intakes/zambia_w_niac.csv")

##################################################################
#11. RUN SPADE FOR VITAMIN B6

zambia_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_vitb6")

zambia_vitb6 <- subset(zambia_vitb6, select = c(age, HI))
zambia_vitb6 <- zambia_vitb6[order(zambia_vitb6$age),]

write.csv(zambia_vitb6, "all_intakes/zambia_w_vitb6.csv")

##################################################################
#12. RUN SPADE FOR FOLATE

zambia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                        data=zambia_wom, seed=123, backtrans.nr = 3,
                        min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                        output.name = "Zambia_wom_fola")

zambia_fola <- subset(zambia_fola, select = c(age, HI))
zambia_fola <- zambia_fola[order(zambia_fola$age),]

write.csv(zambia_fola, "all_intakes/zambia_w_fola.csv")

##################################################################
#13. RUN SPADE FOR RETINOL

zambia_retinol <- f.spade(frml.ia=retinol~fp(age), frml.if=retinol~cs(age), 
                       data=zambia_wom, seed=123, backtrans.nr = 3,
                       min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                       output.name = "Zambia_wom_retinol")

zambia_retinol <- subset(zambia_retinol, select = c(age, HI))
zambia_retinol <- zambia_retinol[order(zambia_retinol$age),]

write.csv(zambia_retinol, "all_intakes/zambia_w_retinol.csv")

##################################################################
#14. RUN SPADE FOR ALPHACAROT

zambia_alphacarot <- f.spade(frml.ia=alphacarot~fp(age), frml.if="no.if", 
                          data=zambia_wom, seed=123, backtrans.nr = 3,
                          min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "Zambia_wom_alphacarot")

zambia_alphacarot <- subset(zambia_alphacarot, select = c(age, HI))
zambia_alphacarot <- zambia_alphacarot[order(zambia_alphacarot$age),]

write.csv(zambia_alphacarot, "all_intakes/zambia_w_alphacarot.csv")

##################################################################
#15. RUN SPADE FOR BETACAROT

zambia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                          data=zambia_wom, seed=123, backtrans.nr = 3,
                          min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "Zambia_wom_betacarot")

zambia_betacarot <- subset(zambia_betacarot, select = c(age, HI))
zambia_betacarot <- zambia_betacarot[order(zambia_betacarot$age),]

write.csv(zambia_betacarot, "all_intakes/zambia_w_betacarot.csv")

##################################################################
#16. RUN SPADE FOR BETACRYPT

zambia_betacrypt <- f.spade(frml.ia=betacrypt~fp(age), frml.if="no.if", 
                            data=zambia_wom, seed=123, backtrans.nr = 3,
                            min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                            output.name = "Zambia_wom_betacrypt")

zambia_betacrypt <- subset(zambia_betacrypt, select = c(age, HI))
zambia_betacrypt <- zambia_betacrypt[order(zambia_betacrypt$age),]

write.csv(zambia_betacrypt, "all_intakes/zambia_w_betacrypt.csv")

##################################################################
#17. RUN SPADE FOR ENERGY

zambia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                            data=zambia_wom, seed=123, backtrans.nr = 3,
                            min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                            output.name = "Zambia_wom_energy")

zambia_energy <- subset(zambia_energy, select = c(age, HI))
zambia_energy <- zambia_energy[order(zambia_energy$age),]

write.csv(zambia_energy, "all_intakes/zambia_w_energy.csv")

##################################################################
#15. RUN SPADE FOR PROTEIN

zambia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                            data=zambia_wom, seed=123, backtrans.nr = 3,
                            min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                            output.name = "Zambia_wom_protein")

zambia_protein <- subset(zambia_protein, select = c(age, HI))
zambia_protein <- zambia_protein[order(zambia_protein$age),]

write.csv(zambia_protein, "all_intakes/zambia_w_protein.csv")

##################################################################
#16. RUN SPADE FOR FAT

zambia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                          data=zambia_wom, seed=123, backtrans.nr = 3,
                          min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "Zambia_wom_fat")

zambia_fat <- subset(zambia_fat, select = c(age, HI))
zambia_fat <- zambia_fat[order(zambia_fat$age),]

write.csv(zambia_fat, "all_intakes/zambia_w_fat.csv")

##################################################################
#17. RUN SPADE FOR CARBOHYDRATE

zambia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                          data=zambia_wom, seed=123, backtrans.nr = 3,
                          min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "Zambia_wom_carb")

zambia_carb <- subset(zambia_carb, select = c(age, HI))
zambia_carb <- zambia_carb[order(zambia_carb$age),]

write.csv(zambia_carb, "all_intakes/zambia_w_carb.csv")

##################################################################
#18. RUN SPADE FOR FIBER

zambia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                          data=zambia_wom, seed=123, backtrans.nr = 3,
                          min.age=18,max.age=67,sex="female", dgts.distr = 2, 
                          output.name = "Zambia_wom_fiber")

zambia_fiber <- subset(zambia_fiber, select = c(age, HI))
zambia_fiber <- zambia_fiber[order(zambia_fiber$age),]

write.csv(zambia_fiber, "all_intakes/zambia_w_fiber.csv")