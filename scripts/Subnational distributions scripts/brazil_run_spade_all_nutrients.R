# Running SPADE: brazil data
# File created on 11/24/20 by Simone Passarelli
# Updated by Simone Passarelli on March 22 2021


# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "brazil"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "brazil"))
###########################################################
# Remove missing obs
summary(brazil_spade)
brazil_spade <- na.omit(brazil_spade)
summary(brazil_spade)
names(brazil_spade)

#remove obs with weights=0
brazil_spade <- subset(brazil_spade, weight !=0)
# Remove instance where there are 


# number of intakes per person:
table(brazil_spade$mday)

# Make separate datasets for men and women
brazil_wom <- subset(brazil_spade, sex==2)
brazil_men <- subset(brazil_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(brazil_wom$age)
range(brazil_men$age)

# Women
brazil_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=brazil_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=105,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "brazil_wom_vitb12")

brazil_vitb12 <- subset(brazil_vitb12, select = c(age, HI))
brazil_vitb12 <- brazil_vitb12[order(brazil_vitb12$age),]

write.csv(brazil_vitb12, "all_intakes/brazil_w_vitb12.csv")

# Men
brazil_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=brazil_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=103,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "brazil_men_vitb12")

brazil_vitb12 <- subset(brazil_vitb12, select = c(age, HI))
brazil_vitb12 <- brazil_vitb12[order(brazil_vitb12$age),]

write.csv(brazil_vitb12, "all_intakes/brazil_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
brazil_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_iron")

brazil_iron <- subset(brazil_iron, select = c(age, HI))
brazil_iron <- brazil_iron[order(brazil_iron$age),]

write.csv(brazil_iron, "all_intakes/brazil_w_iron.csv")

# Men
brazil_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_iron")

brazil_iron <- subset(brazil_iron, select = c(age, HI))
brazil_iron <- brazil_iron[order(brazil_iron$age),]

write.csv(brazil_iron, "all_intakes/brazil_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
brazil_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=brazil_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=105,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "brazil_wom_zinc")

brazil_zinc_w <- subset(brazil_zinc_w, select = c(age, HI))
brazil_zinc_w <- brazil_zinc_w[order(brazil_zinc_w$age),]

write.csv(brazil_zinc_w, "all_intakes/brazil_w_zinc.csv")

# Men
brazil_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=brazil_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=103,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "brazil_men_zinc")

brazil_zinc_m <- subset(brazil_zinc_m, select = c(age, HI))
brazil_zinc_m <- brazil_zinc_m[order(brazil_zinc_m$age),]

write.csv(brazil_zinc_m, "all_intakes/brazil_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

brazil_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_vita")

brazil_vita <- subset(brazil_vita, select = c(age, HI))
brazil_vita <- brazil_vita[order(brazil_vita$age),]

write.csv(brazil_vita, "all_intakes/brazil_w_vita.csv")

# Men
brazil_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_vita")

brazil_vita <- subset(brazil_vita, select = c(age, HI))
brazil_vita <- brazil_vita[order(brazil_vita$age),]

write.csv(brazil_vita, "all_intakes/brazil_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

brazil_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_calc")

brazil_calc <- subset(brazil_calc, select = c(age, HI))
brazil_calc <- brazil_calc[order(brazil_calc$age),]

write.csv(brazil_calc, "all_intakes/brazil_w_calc.csv")

# Men
brazil_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_calc")

brazil_calc <- subset(brazil_calc, select = c(age, HI))
brazil_calc <- brazil_calc[order(brazil_calc$age),]

write.csv(brazil_calc, "all_intakes/brazil_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

brazil_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=brazil_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=10, max.age=105,
                          sex.lab="women",
                          weights.name = "weight",
                          output.name = "brazil_wom_omega_3")

brazil_omega_3 <- subset(brazil_omega_3, select = c(age, HI))
brazil_omega_3 <- brazil_omega_3[order(brazil_omega_3$age),]

write.csv(brazil_omega_3, "all_intakes/brazil_w_omega_3.csv")

# Men
brazil_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=brazil_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=10, max.age=103,
                          sex.lab="men",
                          weights.name = "weight",
                          output.name = "brazil_men_omega_3")

brazil_omega_3 <- subset(brazil_omega_3, select = c(age, HI))
brazil_omega_3 <- brazil_omega_3[order(brazil_omega_3$age),]

write.csv(brazil_omega_3, "all_intakes/brazil_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

brazil_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=brazil_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=105,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "brazil_wom_energy")

brazil_energy <- subset(brazil_energy, select = c(age, HI))
brazil_energy <- brazil_energy[order(brazil_energy$age),]

write.csv(brazil_energy, "all_intakes/brazil_w_energy.csv")

# Men
brazil_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=brazil_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=103,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "brazil_men_energy")

brazil_energy <- subset(brazil_energy, select = c(age, HI))
brazil_energy <- brazil_energy[order(brazil_energy$age),]

write.csv(brazil_energy, "all_intakes/brazil_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

brazil_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=brazil_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=10, max.age=105,
                          sex.lab="women",
                          weights.name = "weight",
                          output.name = "brazil_wom_protein")

brazil_protein <- subset(brazil_protein, select = c(age, HI))
brazil_protein <- brazil_protein[order(brazil_protein$age),]

write.csv(brazil_protein, "all_intakes/brazil_w_protein.csv")

# Men
brazil_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=brazil_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=10, max.age=103,
                          sex.lab="men",
                          weights.name = "weight",
                          output.name = "brazil_men_protein")

brazil_protein <- subset(brazil_protein, select = c(age, HI))
brazil_protein <- brazil_protein[order(brazil_protein$age),]

write.csv(brazil_protein, "all_intakes/brazil_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

brazil_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_carb")

brazil_carb <- subset(brazil_carb, select = c(age, HI))
brazil_carb <- brazil_carb[order(brazil_carb$age),]

write.csv(brazil_carb, "all_intakes/brazil_w_carb.csv")

# Men
brazil_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_carb")

brazil_carb <- subset(brazil_carb, select = c(age, HI))
brazil_carb <- brazil_carb[order(brazil_carb$age),]

write.csv(brazil_carb, "all_intakes/brazil_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR SUGAR

brazil_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                        data=brazil_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=105,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "brazil_wom_sugar")

brazil_sugar <- subset(brazil_sugar, select = c(age, HI))
brazil_sugar <- brazil_sugar[order(brazil_sugar$age),]

write.csv(brazil_sugar, "all_intakes/brazil_w_sugar.csv")

# Men
brazil_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                        data=brazil_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=103,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "brazil_men_sugar")

brazil_sugar <- subset(brazil_sugar, select = c(age, HI))
brazil_sugar <- brazil_sugar[order(brazil_sugar$age),]

write.csv(brazil_sugar, "all_intakes/brazil_m_sugar.csv")

##################################################################

# 12. RUN SPADE FOR FIBER

brazil_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=brazil_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=105,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "brazil_wom_fiber")

brazil_fiber <- subset(brazil_fiber, select = c(age, HI))
brazil_fiber <- brazil_fiber[order(brazil_fiber$age),]

write.csv(brazil_fiber, "all_intakes/brazil_w_fiber.csv")

# Men
brazil_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=brazil_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=103,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "brazil_men_fiber")

brazil_fiber <- subset(brazil_fiber, select = c(age, HI))
brazil_fiber <- brazil_fiber[order(brazil_fiber$age),]

write.csv(brazil_fiber, "all_intakes/brazil_m_fiber.csv")

##################################################################

# 13. RUN SPADE FOR FAT

brazil_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=brazil_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=10, max.age=105,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "brazil_wom_fat")

brazil_fat <- subset(brazil_fat, select = c(age, HI))
brazil_fat <- brazil_fat[order(brazil_fat$age),]

write.csv(brazil_fat, "all_intakes/brazil_w_fat.csv")

# Men
brazil_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=brazil_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=10, max.age=103,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "brazil_men_fat")

brazil_fat <- subset(brazil_fat, select = c(age, HI))
brazil_fat <- brazil_fat[order(brazil_fat$age),]

write.csv(brazil_fat, "all_intakes/brazil_m_fat.csv")

##################################################################

# 14. RUN SPADE FOR SATFAT

brazil_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=brazil_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=105,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "brazil_wom_satfat")

brazil_satfat <- subset(brazil_satfat, select = c(age, HI))
brazil_satfat <- brazil_satfat[order(brazil_satfat$age),]

write.csv(brazil_satfat, "all_intakes/brazil_w_satfat.csv")

# Men
brazil_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=brazil_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=10, max.age=103,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "brazil_men_satfat")

brazil_satfat <- subset(brazil_satfat, select = c(age, HI))
brazil_satfat <- brazil_satfat[order(brazil_satfat$age),]

write.csv(brazil_satfat, "all_intakes/brazil_m_satfat.csv")

##################################################################

# 15. RUN SPADE FOR MUFA

brazil_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_mufa")

brazil_mufa <- subset(brazil_mufa, select = c(age, HI))
brazil_mufa <- brazil_mufa[order(brazil_mufa$age),]

write.csv(brazil_mufa, "all_intakes/brazil_w_mufa.csv")

# Men
brazil_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_mufa")

brazil_mufa <- subset(brazil_mufa, select = c(age, HI))
brazil_mufa <- brazil_mufa[order(brazil_mufa$age),]

write.csv(brazil_mufa, "all_intakes/brazil_m_mufa.csv")

##################################################################

# 16. RUN SPADE FOR PUFA

brazil_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_pufa")

brazil_pufa <- subset(brazil_pufa, select = c(age, HI))
brazil_pufa <- brazil_pufa[order(brazil_pufa$age),]

write.csv(brazil_pufa, "all_intakes/brazil_w_pufa.csv")

# Men
brazil_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_pufa")

brazil_pufa <- subset(brazil_pufa, select = c(age, HI))
brazil_pufa <- brazil_pufa[order(brazil_pufa$age),]

write.csv(brazil_pufa, "all_intakes/brazil_m_pufa.csv")

##################################################################

# 17. RUN SPADE FOR THIAMIN

brazil_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_thia")

brazil_thia <- subset(brazil_thia, select = c(age, HI))
brazil_thia <- brazil_thia[order(brazil_thia$age),]

write.csv(brazil_thia, "all_intakes/brazil_w_thia.csv")

# Men
brazil_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_thia")

brazil_thia <- subset(brazil_thia, select = c(age, HI))
brazil_thia <- brazil_thia[order(brazil_thia$age),]

write.csv(brazil_thia, "all_intakes/brazil_m_thia.csv")

##################################################################

# 18. RUN SPADE FOR RIBOFLAVIN

brazil_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_ribo")

brazil_ribo <- subset(brazil_ribo, select = c(age, HI))
brazil_ribo <- brazil_ribo[order(brazil_ribo$age),]

write.csv(brazil_ribo, "all_intakes/brazil_w_ribo.csv")

# Men
brazil_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_ribo")

brazil_ribo <- subset(brazil_ribo, select = c(age, HI))
brazil_ribo <- brazil_ribo[order(brazil_ribo$age),]

write.csv(brazil_ribo, "all_intakes/brazil_m_ribo.csv")

##################################################################

# 19. RUN SPADE FOR NIACIN

brazil_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_niac")

brazil_niac <- subset(brazil_niac, select = c(age, HI))
brazil_niac <- brazil_niac[order(brazil_niac$age),]

write.csv(brazil_niac, "all_intakes/brazil_w_niac.csv")

# Men
brazil_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_niac")

brazil_niac <- subset(brazil_niac, select = c(age, HI))
brazil_niac <- brazil_niac[order(brazil_niac$age),]

write.csv(brazil_niac, "all_intakes/brazil_m_niac.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN B6

brazil_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=brazil_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=105,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "brazil_wom_vitb6")

brazil_vitb6 <- subset(brazil_vitb6, select = c(age, HI))
brazil_vitb6 <- brazil_vitb6[order(brazil_vitb6$age),]

write.csv(brazil_vitb6, "all_intakes/brazil_w_vitb6.csv")

# Men
brazil_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=brazil_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=10, max.age=103,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "brazil_men_vitb6")

brazil_vitb6 <- subset(brazil_vitb6, select = c(age, HI))
brazil_vitb6 <- brazil_vitb6[order(brazil_vitb6$age),]

write.csv(brazil_vitb6, "all_intakes/brazil_m_vitb6.csv")

##################################################################

# 21. RUN SPADE FOR FOLATE

brazil_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_fola")

brazil_fola <- subset(brazil_fola, select = c(age, HI))
brazil_fola <- brazil_fola[order(brazil_fola$age),]

write.csv(brazil_fola, "all_intakes/brazil_w_fola.csv")

# Men
brazil_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_fola")

brazil_fola <- subset(brazil_fola, select = c(age, HI))
brazil_fola <- brazil_fola[order(brazil_fola$age),]

write.csv(brazil_fola, "all_intakes/brazil_m_fola.csv")

##################################################################

# 22. RUN SPADE FOR VITAMIN D

brazil_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_vitd")

brazil_vitd <- subset(brazil_vitd, select = c(age, HI))
brazil_vitd <- brazil_vitd[order(brazil_vitd$age),]

write.csv(brazil_vitd, "all_intakes/brazil_w_vitd.csv")

# Men
brazil_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_vitd")

brazil_vitd <- subset(brazil_vitd, select = c(age, HI))
brazil_vitd <- brazil_vitd[order(brazil_vitd$age),]

write.csv(brazil_vitd, "all_intakes/brazil_m_vitd.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN C

brazil_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_vitc")

brazil_vitc <- subset(brazil_vitc, select = c(age, HI))
brazil_vitc <- brazil_vitc[order(brazil_vitc$age),]

write.csv(brazil_vitc, "all_intakes/brazil_w_vitc.csv")

# Men
brazil_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_vitc")

brazil_vitc <- subset(brazil_vitc, select = c(age, HI))
brazil_vitc <- brazil_vitc[order(brazil_vitc$age),]

write.csv(brazil_vitc, "all_intakes/brazil_m_vitc.csv")

##################################################################

# 24. RUN SPADE FOR PHOSPHORUS

brazil_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_phos")

brazil_phos <- subset(brazil_phos, select = c(age, HI))
brazil_phos <- brazil_phos[order(brazil_phos$age),]

write.csv(brazil_phos, "all_intakes/brazil_w_phos.csv")

# Men
brazil_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_phos")

brazil_phos <- subset(brazil_phos, select = c(age, HI))
brazil_phos <- brazil_phos[order(brazil_phos$age),]

write.csv(brazil_phos, "all_intakes/brazil_m_phos.csv")

##################################################################

# 25. RUN SPADE FOR MG

brazil_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=brazil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=10, max.age=105,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "brazil_wom_mg")

brazil_mg <- subset(brazil_mg, select = c(age, HI))
brazil_mg <- brazil_mg[order(brazil_mg$age),]

write.csv(brazil_mg, "all_intakes/brazil_w_mg.csv")

# Men
brazil_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=brazil_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=10, max.age=103,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "brazil_men_mg")

brazil_mg <- subset(brazil_mg, select = c(age, HI))
brazil_mg <- brazil_mg[order(brazil_mg$age),]

write.csv(brazil_mg, "all_intakes/brazil_m_mg.csv")

##################################################################

# 27. RUN SPADE FOR SODIUM

brazil_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=brazil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=10, max.age=105,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "brazil_wom_na")

brazil_na <- subset(brazil_na, select = c(age, HI))
brazil_na <- brazil_na[order(brazil_na$age),]

write.csv(brazil_na, "all_intakes/brazil_w_na.csv")

# Men
brazil_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=brazil_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=10, max.age=103,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "brazil_men_na")

brazil_na <- subset(brazil_na, select = c(age, HI))
brazil_na <- brazil_na[order(brazil_na$age),]

write.csv(brazil_na, "all_intakes/brazil_m_na.csv")

##################################################################

# 28. RUN SPADE FOR POTASSIUM

brazil_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=brazil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=105,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "brazil_wom_pota")

brazil_pota <- subset(brazil_pota, select = c(age, HI))
brazil_pota <- brazil_pota[order(brazil_pota$age),]

write.csv(brazil_pota, "all_intakes/brazil_w_pota.csv")

# Men
brazil_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=brazil_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=10, max.age=103,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "brazil_men_pota")

brazil_pota <- subset(brazil_pota, select = c(age, HI))
brazil_pota <- brazil_pota[order(brazil_pota$age),]

write.csv(brazil_pota, "all_intakes/brazil_m_pota.csv")
