# Running SPADE: kenya data
# File created on 11/24/20 by Simone Passarelli
# Updated by Simone Passarelli on March 22 2021

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "kenya"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "kenya"))
###########################################################

# Remove missing obs
summary(kenya_spade)
names(kenya_spade)

#remove obs with weights=0
kenya_spade <- subset(kenya_spade, weight !=0)
# Remove instance where there are 


# number of intakes per person:
table(kenya_spade$mday)

# Make separate datasets for men and women
kenya_wom <- subset(kenya_spade, sex==2)
kenya_men <- subset(kenya_spade, sex==1)

# Only women and children

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(kenya_wom$age)
range(kenya_men$age)

# Women
kenya_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if=vitb12 ~cs(age),
                         data=kenya_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=49,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "kenya_wom_vitb12")

kenya_vitb12 <- subset(kenya_vitb12, select = c(age, HI))
kenya_vitb12 <- kenya_vitb12[order(kenya_vitb12$age),]

write.csv(kenya_vitb12, "all_intakes/kenya_w_vitb12.csv")

# Men
kenya_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if=vitb12 ~cs(age), 
                         data=kenya_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=4,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "kenya_men_vitb12")

kenya_vitb12 <- subset(kenya_vitb12, select = c(age, HI))
kenya_vitb12 <- kenya_vitb12[order(kenya_vitb12$age),]

write.csv(kenya_vitb12, "all_intakes/kenya_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
kenya_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_iron")

kenya_iron <- subset(kenya_iron, select = c(age, HI))
kenya_iron <- kenya_iron[order(kenya_iron$age),]

write.csv(kenya_iron, "all_intakes/kenya_w_iron.csv")

# Men
kenya_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_iron")

kenya_iron <- subset(kenya_iron, select = c(age, HI))
kenya_iron <- kenya_iron[order(kenya_iron$age),]

write.csv(kenya_iron, "all_intakes/kenya_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
kenya_zinc <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=kenya_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=49,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "kenya_wom_zinc")

kenya_zinc <- subset(kenya_zinc, select = c(age, HI))
kenya_zinc <- kenya_zinc[order(kenya_zinc$age),]

write.csv(kenya_zinc, "all_intakes/kenya_w_zinc.csv")

# Men
kenya_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=kenya_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=4,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "kenya_men_zinc")

kenya_zinc <- subset(kenya_zinc, select = c(age, HI))
kenya_zinc <- kenya_zinc[order(kenya_zinc$age),]

write.csv(kenya_zinc, "all_intakes/kenya_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

kenya_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_vita")

kenya_vita <- subset(kenya_vita, select = c(age, HI))
kenya_vita <- kenya_vita[order(kenya_vita$age),]

write.csv(kenya_vita, "all_intakes/kenya_w_vita.csv")

# Men
kenya_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_vita")

kenya_vita <- subset(kenya_vita, select = c(age, HI))
kenya_vita <- kenya_vita[order(kenya_vita$age),]

write.csv(kenya_vita, "all_intakes/kenya_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

kenya_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_calc")

kenya_calc <- subset(kenya_calc, select = c(age, HI))
kenya_calc <- kenya_calc[order(kenya_calc$age),]

write.csv(kenya_calc, "all_intakes/kenya_w_calc.csv")

# Men
kenya_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_calc")

kenya_calc <- subset(kenya_calc, select = c(age, HI))
kenya_calc <- kenya_calc[order(kenya_calc$age),]

write.csv(kenya_calc, "all_intakes/kenya_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

# kenya_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
#                           data=kenya_wom, seed=123,  backtrans.nr = 3,
#                           dgts.distr = 2, min.age=1, max.age=49,
#                           sex.lab="women",
#                           weights.name = "weight",
#                           output.name = "kenya_wom_omega_3")
# 
# kenya_omega_3 <- subset(kenya_omega_3, select = c(age, HI))
# kenya_omega_3 <- kenya_omega_3[order(kenya_omega_3$age),]
# 
# write.csv(kenya_omega_3, "all_intakes/kenya_w_omega_3.csv")
# 
# # Men
# kenya_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
#                           data=kenya_men, seed=123,  backtrans.nr = 3,
#                           dgts.distr = 2, min.age=1, max.age=4,
#                           sex.lab="men",
#                           weights.name = "weight",
#                           output.name = "kenya_men_omega_3")
# 
# kenya_omega_3 <- subset(kenya_omega_3, select = c(age, HI))
# kenya_omega_3 <- kenya_omega_3[order(kenya_omega_3$age),]
# 
# write.csv(kenya_omega_3, "all_intakes/kenya_m_omega_3.csv")

##################################################################

# 6. RUN SPADE FOR ENERGY

kenya_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=kenya_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=49,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "kenya_wom_energy")

kenya_energy <- subset(kenya_energy, select = c(age, HI))
kenya_energy <- kenya_energy[order(kenya_energy$age),]

write.csv(kenya_energy, "all_intakes/kenya_w_energy.csv")

# Men
kenya_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=kenya_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=4,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "kenya_men_energy")

kenya_energy <- subset(kenya_energy, select = c(age, HI))
kenya_energy <- kenya_energy[order(kenya_energy$age),]

write.csv(kenya_energy, "all_intakes/kenya_m_energy.csv")

##################################################################

# 7. RUN SPADE FOR PROTEIN

kenya_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=kenya_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=49,
                          sex.lab="women",
                          weights.name = "weight",
                          output.name = "kenya_wom_protein")

kenya_protein <- subset(kenya_protein, select = c(age, HI))
kenya_protein <- kenya_protein[order(kenya_protein$age),]

write.csv(kenya_protein, "all_intakes/kenya_w_protein.csv")

# Men
kenya_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=kenya_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="men",
                          weights.name = "weight",
                          output.name = "kenya_men_protein")

kenya_protein <- subset(kenya_protein, select = c(age, HI))
kenya_protein <- kenya_protein[order(kenya_protein$age),]

write.csv(kenya_protein, "all_intakes/kenya_m_protein.csv")

##################################################################

# 8. RUN SPADE FOR CARB

kenya_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_carb")

kenya_carb <- subset(kenya_carb, select = c(age, HI))
kenya_carb <- kenya_carb[order(kenya_carb$age),]

write.csv(kenya_carb, "all_intakes/kenya_w_carb.csv")

# Men
kenya_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_carb")

kenya_carb <- subset(kenya_carb, select = c(age, HI))
kenya_carb <- kenya_carb[order(kenya_carb$age),]

write.csv(kenya_carb, "all_intakes/kenya_m_carb.csv")


##################################################################

# 9. RUN SPADE FOR FIBER

kenya_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=kenya_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=49,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "kenya_wom_fiber")

kenya_fiber <- subset(kenya_fiber, select = c(age, HI))
kenya_fiber <- kenya_fiber[order(kenya_fiber$age),]

write.csv(kenya_fiber, "all_intakes/kenya_w_fiber.csv")

# Men
kenya_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=kenya_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "kenya_men_fiber")

kenya_fiber <- subset(kenya_fiber, select = c(age, HI))
kenya_fiber <- kenya_fiber[order(kenya_fiber$age),]

write.csv(kenya_fiber, "all_intakes/kenya_m_fiber.csv")

##################################################################

# 10. RUN SPADE FOR FAT

kenya_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=kenya_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=49,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "kenya_wom_fat")

kenya_fat <- subset(kenya_fat, select = c(age, HI))
kenya_fat <- kenya_fat[order(kenya_fat$age),]

write.csv(kenya_fat, "all_intakes/kenya_w_fat.csv")

# Men
kenya_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=kenya_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "kenya_men_fat")

kenya_fat <- subset(kenya_fat, select = c(age, HI))
kenya_fat <- kenya_fat[order(kenya_fat$age),]

write.csv(kenya_fat, "all_intakes/kenya_m_fat.csv")

##################################################################

# 11. RUN SPADE FOR THIAMIN

kenya_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_thia")

kenya_thia <- subset(kenya_thia, select = c(age, HI))
kenya_thia <- kenya_thia[order(kenya_thia$age),]

write.csv(kenya_thia, "all_intakes/kenya_w_thia.csv")

# Men
kenya_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_thia")

kenya_thia <- subset(kenya_thia, select = c(age, HI))
kenya_thia <- kenya_thia[order(kenya_thia$age),]

write.csv(kenya_thia, "all_intakes/kenya_m_thia.csv")

##################################################################

# 12. RUN SPADE FOR RIBOFLAVIN

kenya_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_ribo")

kenya_ribo <- subset(kenya_ribo, select = c(age, HI))
kenya_ribo <- kenya_ribo[order(kenya_ribo$age),]

write.csv(kenya_ribo, "all_intakes/kenya_w_ribo.csv")

# Men
kenya_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_ribo")

kenya_ribo <- subset(kenya_ribo, select = c(age, HI))
kenya_ribo <- kenya_ribo[order(kenya_ribo$age),]

write.csv(kenya_ribo, "all_intakes/kenya_m_ribo.csv")

##################################################################

# 13. RUN SPADE FOR NIACIN

kenya_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_niac")

kenya_niac <- subset(kenya_niac, select = c(age, HI))
kenya_niac <- kenya_niac[order(kenya_niac$age),]

write.csv(kenya_niac, "all_intakes/kenya_w_niac.csv")

# Men
kenya_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_niac")

kenya_niac <- subset(kenya_niac, select = c(age, HI))
kenya_niac <- kenya_niac[order(kenya_niac$age),]

write.csv(kenya_niac, "all_intakes/kenya_m_niac.csv")

##################################################################

# 14. RUN SPADE FOR VITAMIN B6

kenya_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=kenya_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=49,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "kenya_wom_vitb6")

kenya_vitb6 <- subset(kenya_vitb6, select = c(age, HI))
kenya_vitb6 <- kenya_vitb6[order(kenya_vitb6$age),]

write.csv(kenya_vitb6, "all_intakes/kenya_w_vitb6.csv")

# Men
kenya_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=kenya_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "kenya_men_vitb6")

kenya_vitb6 <- subset(kenya_vitb6, select = c(age, HI))
kenya_vitb6 <- kenya_vitb6[order(kenya_vitb6$age),]

write.csv(kenya_vitb6, "all_intakes/kenya_m_vitb6.csv")

##################################################################

# 15. RUN SPADE FOR FOLATE

kenya_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_fola")

kenya_fola <- subset(kenya_fola, select = c(age, HI))
kenya_fola <- kenya_fola[order(kenya_fola$age),]

write.csv(kenya_fola, "all_intakes/kenya_w_fola.csv")

# Men
kenya_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_fola")

kenya_fola <- subset(kenya_fola, select = c(age, HI))
kenya_fola <- kenya_fola[order(kenya_fola$age),]

write.csv(kenya_fola, "all_intakes/kenya_m_fola.csv")

##################################################################

# 16. RUN SPADE FOR VITAMIN C

kenya_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=kenya_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=49,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "kenya_wom_vitc")

kenya_vitc <- subset(kenya_vitc, select = c(age, HI))
kenya_vitc <- kenya_vitc[order(kenya_vitc$age),]

write.csv(kenya_vitc, "all_intakes/kenya_w_vitc.csv")

# Men
kenya_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=kenya_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "kenya_men_vitc")

kenya_vitc <- subset(kenya_vitc, select = c(age, HI))
kenya_vitc <- kenya_vitc[order(kenya_vitc$age),]

write.csv(kenya_vitc, "all_intakes/kenya_m_vitc.csv")

##################################################################

# 17. RUN SPADE FOR BETA CAROTENE

kenya_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                              data=kenya_wom, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=18, max.age=45,
                              sex.lab="women",
                           weights.name = "weight",
                              output.name = "kenya_wom_betacarot")

kenya_betacarot <- subset(kenya_betacarot, select = c(age, HI))
kenya_betacarot <- kenya_betacarot[order(kenya_betacarot$age),]

write.csv(kenya_betacarot, "all_intakes/kenya_w_betacarot.csv")
