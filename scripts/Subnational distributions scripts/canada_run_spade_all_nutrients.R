# Running SPADE: canada data
# File created on 11/24/20 by Simone Passarelli
# Updated by Simone Passarelli on March 17 2021


# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "canada"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Canada"))
###########################################################
# Remove missing obs
summary(canada_spade)
canada_spade <- na.omit(canada_spade)
summary(canada_spade)
names(canada_spade)

#remove obs with weights=0
canada_spade <- subset(canada_spade, weight !=0)
# Remove instance where there are 


# number of intakes per person:
table(canada_spade$mday)

# Make separate datasets for men and women
canada_wom <- subset(canada_spade, sex==2)
canada_men <- subset(canada_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(canada_wom$age)
range(canada_men$age)

# Women
canada_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=canada_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "canada_wom_vitb12")

canada_vitb12 <- subset(canada_vitb12, select = c(age, HI))
canada_vitb12 <- canada_vitb12[order(canada_vitb12$age),]

write.csv(canada_vitb12, "all_intakes/canada_w_vitb12.csv")

# Men
canada_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=canada_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "canada_men_vitb12")

canada_vitb12 <- subset(canada_vitb12, select = c(age, HI))
canada_vitb12 <- canada_vitb12[order(canada_vitb12$age),]

write.csv(canada_vitb12, "all_intakes/canada_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
canada_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_iron")

canada_iron <- subset(canada_iron, select = c(age, HI))
canada_iron <- canada_iron[order(canada_iron$age),]

write.csv(canada_iron, "all_intakes/canada_w_iron.csv")

# Men
canada_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_iron")

canada_iron <- subset(canada_iron, select = c(age, HI))
canada_iron <- canada_iron[order(canada_iron$age),]

write.csv(canada_iron, "all_intakes/canada_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
canada_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=canada_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "canada_wom_zinc")

canada_zinc_w <- subset(canada_zinc_w, select = c(age, HI))
canada_zinc_w <- canada_zinc_w[order(canada_zinc_w$age),]

write.csv(canada_zinc_w, "all_intakes/canada_w_zinc.csv")

# Men
canada_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=canada_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "canada_men_zinc")

canada_zinc_m <- subset(canada_zinc_m, select = c(age, HI))
canada_zinc_m <- canada_zinc_m[order(canada_zinc_m$age),]

write.csv(canada_zinc_m, "all_intakes/canada_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

canada_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_vita")

canada_vita <- subset(canada_vita, select = c(age, HI))
canada_vita <- canada_vita[order(canada_vita$age),]

write.csv(canada_vita, "all_intakes/canada_w_vita.csv")

# Men
canada_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_vita")

canada_vita <- subset(canada_vita, select = c(age, HI))
canada_vita <- canada_vita[order(canada_vita$age),]

write.csv(canada_vita, "all_intakes/canada_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

canada_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_calc")

canada_calc <- subset(canada_calc, select = c(age, HI))
canada_calc <- canada_calc[order(canada_calc$age),]

write.csv(canada_calc, "all_intakes/canada_w_calc.csv")

# Men
canada_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_calc")

canada_calc <- subset(canada_calc, select = c(age, HI))
canada_calc <- canada_calc[order(canada_calc$age),]

write.csv(canada_calc, "all_intakes/canada_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

canada_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                       data=canada_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=95,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "canada_wom_omega_3")

canada_omega_3 <- subset(canada_omega_3, select = c(age, HI))
canada_omega_3 <- canada_omega_3[order(canada_omega_3$age),]

write.csv(canada_omega_3, "all_intakes/canada_w_omega_3.csv")

# Men
canada_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                       data=canada_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=95,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "canada_men_omega_3")

canada_omega_3 <- subset(canada_omega_3, select = c(age, HI))
canada_omega_3 <- canada_omega_3[order(canada_omega_3$age),]

write.csv(canada_omega_3, "all_intakes/canada_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

canada_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                      data=canada_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "canada_wom_energy")

canada_energy <- subset(canada_energy, select = c(age, HI))
canada_energy <- canada_energy[order(canada_energy$age),]

write.csv(canada_energy, "all_intakes/canada_w_energy.csv")

# Men
canada_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                      data=canada_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "canada_men_energy")

canada_energy <- subset(canada_energy, select = c(age, HI))
canada_energy <- canada_energy[order(canada_energy$age),]

write.csv(canada_energy, "all_intakes/canada_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

canada_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                       data=canada_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=95,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "canada_wom_protein")

canada_protein <- subset(canada_protein, select = c(age, HI))
canada_protein <- canada_protein[order(canada_protein$age),]

write.csv(canada_protein, "all_intakes/canada_w_protein.csv")

# Men
canada_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                       data=canada_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=95,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "canada_men_protein")

canada_protein <- subset(canada_protein, select = c(age, HI))
canada_protein <- canada_protein[order(canada_protein$age),]

write.csv(canada_protein, "all_intakes/canada_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

canada_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_carb")

canada_carb <- subset(canada_carb, select = c(age, HI))
canada_carb <- canada_carb[order(canada_carb$age),]

write.csv(canada_carb, "all_intakes/canada_w_carb.csv")

# Men
canada_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_carb")

canada_carb <- subset(canada_carb, select = c(age, HI))
canada_carb <- canada_carb[order(canada_carb$age),]

write.csv(canada_carb, "all_intakes/canada_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR SUGAR

canada_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                     data=canada_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "canada_wom_sugar")

canada_sugar <- subset(canada_sugar, select = c(age, HI))
canada_sugar <- canada_sugar[order(canada_sugar$age),]

write.csv(canada_sugar, "all_intakes/canada_w_sugar.csv")

# Men
canada_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                     data=canada_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "canada_men_sugar")

canada_sugar <- subset(canada_sugar, select = c(age, HI))
canada_sugar <- canada_sugar[order(canada_sugar$age),]

write.csv(canada_sugar, "all_intakes/canada_m_sugar.csv")

##################################################################

# 12. RUN SPADE FOR FIBER

canada_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                     data=canada_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "canada_wom_fiber")

canada_fiber <- subset(canada_fiber, select = c(age, HI))
canada_fiber <- canada_fiber[order(canada_fiber$age),]

write.csv(canada_fiber, "all_intakes/canada_w_fiber.csv")

# Men
canada_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                     data=canada_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "canada_men_fiber")

canada_fiber <- subset(canada_fiber, select = c(age, HI))
canada_fiber <- canada_fiber[order(canada_fiber$age),]

write.csv(canada_fiber, "all_intakes/canada_m_fiber.csv")

##################################################################

# 13. RUN SPADE FOR FAT

canada_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                   data=canada_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=95,
                   sex.lab="women",
                   weights.name = "weight",
                   output.name = "canada_wom_fat")

canada_fat <- subset(canada_fat, select = c(age, HI))
canada_fat <- canada_fat[order(canada_fat$age),]

write.csv(canada_fat, "all_intakes/canada_w_fat.csv")

# Men
canada_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                   data=canada_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=95,
                   sex.lab="men",
                   weights.name = "weight",
                   output.name = "canada_men_fat")

canada_fat <- subset(canada_fat, select = c(age, HI))
canada_fat <- canada_fat[order(canada_fat$age),]

write.csv(canada_fat, "all_intakes/canada_m_fat.csv")

##################################################################

# 14. RUN SPADE FOR SATFAT

canada_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                      data=canada_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "canada_wom_satfat")

canada_satfat <- subset(canada_satfat, select = c(age, HI))
canada_satfat <- canada_satfat[order(canada_satfat$age),]

write.csv(canada_satfat, "all_intakes/canada_w_satfat.csv")

# Men
canada_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                      data=canada_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=95,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "canada_men_satfat")

canada_satfat <- subset(canada_satfat, select = c(age, HI))
canada_satfat <- canada_satfat[order(canada_satfat$age),]

write.csv(canada_satfat, "all_intakes/canada_m_satfat.csv")

##################################################################

# 15. RUN SPADE FOR MUFA

canada_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_mufa")

canada_mufa <- subset(canada_mufa, select = c(age, HI))
canada_mufa <- canada_mufa[order(canada_mufa$age),]

write.csv(canada_mufa, "all_intakes/canada_w_mufa.csv")

# Men
canada_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_mufa")

canada_mufa <- subset(canada_mufa, select = c(age, HI))
canada_mufa <- canada_mufa[order(canada_mufa$age),]

write.csv(canada_mufa, "all_intakes/canada_m_mufa.csv")

##################################################################

# 16. RUN SPADE FOR PUFA

canada_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_pufa")

canada_pufa <- subset(canada_pufa, select = c(age, HI))
canada_pufa <- canada_pufa[order(canada_pufa$age),]

write.csv(canada_pufa, "all_intakes/canada_w_pufa.csv")

# Men
canada_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_pufa")

canada_pufa <- subset(canada_pufa, select = c(age, HI))
canada_pufa <- canada_pufa[order(canada_pufa$age),]

write.csv(canada_pufa, "all_intakes/canada_m_pufa.csv")

##################################################################

# 17. RUN SPADE FOR THIAMIN

canada_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_thia")

canada_thia <- subset(canada_thia, select = c(age, HI))
canada_thia <- canada_thia[order(canada_thia$age),]

write.csv(canada_thia, "all_intakes/canada_w_thia.csv")

# Men
canada_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_thia")

canada_thia <- subset(canada_thia, select = c(age, HI))
canada_thia <- canada_thia[order(canada_thia$age),]

write.csv(canada_thia, "all_intakes/canada_m_thia.csv")

##################################################################

# 18. RUN SPADE FOR RIBOFLAVIN

canada_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_ribo")

canada_ribo <- subset(canada_ribo, select = c(age, HI))
canada_ribo <- canada_ribo[order(canada_ribo$age),]

write.csv(canada_ribo, "all_intakes/canada_w_ribo.csv")

# Men
canada_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_ribo")

canada_ribo <- subset(canada_ribo, select = c(age, HI))
canada_ribo <- canada_ribo[order(canada_ribo$age),]

write.csv(canada_ribo, "all_intakes/canada_m_ribo.csv")

##################################################################

# 19. RUN SPADE FOR NIACIN

canada_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_niac")

canada_niac <- subset(canada_niac, select = c(age, HI))
canada_niac <- canada_niac[order(canada_niac$age),]

write.csv(canada_niac, "all_intakes/canada_w_niac.csv")

# Men
canada_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_niac")

canada_niac <- subset(canada_niac, select = c(age, HI))
canada_niac <- canada_niac[order(canada_niac$age),]

write.csv(canada_niac, "all_intakes/canada_m_niac.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN B6

canada_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                     data=canada_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "canada_wom_vitb6")

canada_vitb6 <- subset(canada_vitb6, select = c(age, HI))
canada_vitb6 <- canada_vitb6[order(canada_vitb6$age),]

write.csv(canada_vitb6, "all_intakes/canada_w_vitb6.csv")

# Men
canada_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                     data=canada_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=95,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "canada_men_vitb6")

canada_vitb6 <- subset(canada_vitb6, select = c(age, HI))
canada_vitb6 <- canada_vitb6[order(canada_vitb6$age),]

write.csv(canada_vitb6, "all_intakes/canada_m_vitb6.csv")

##################################################################

# 21. RUN SPADE FOR FOLATE

canada_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_fola")

canada_fola <- subset(canada_fola, select = c(age, HI))
canada_fola <- canada_fola[order(canada_fola$age),]

write.csv(canada_fola, "all_intakes/canada_w_fola.csv")

# Men
canada_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_fola")

canada_fola <- subset(canada_fola, select = c(age, HI))
canada_fola <- canada_fola[order(canada_fola$age),]

write.csv(canada_fola, "all_intakes/canada_m_fola.csv")

##################################################################

# 22. RUN SPADE FOR VITAMIN D

canada_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_vitd")

canada_vitd <- subset(canada_vitd, select = c(age, HI))
canada_vitd <- canada_vitd[order(canada_vitd$age),]

write.csv(canada_vitd, "all_intakes/canada_w_vitd.csv")

# Men
canada_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_vitd")

canada_vitd <- subset(canada_vitd, select = c(age, HI))
canada_vitd <- canada_vitd[order(canada_vitd$age),]

write.csv(canada_vitd, "all_intakes/canada_m_vitd.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN C

canada_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_vitc")

canada_vitc <- subset(canada_vitc, select = c(age, HI))
canada_vitc <- canada_vitc[order(canada_vitc$age),]

write.csv(canada_vitc, "all_intakes/canada_w_vitc.csv")

# Men
canada_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_vitc")

canada_vitc <- subset(canada_vitc, select = c(age, HI))
canada_vitc <- canada_vitc[order(canada_vitc$age),]

write.csv(canada_vitc, "all_intakes/canada_m_vitc.csv")

##################################################################

# 24. RUN SPADE FOR PHOSPHORUS

canada_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_phos")

canada_phos <- subset(canada_phos, select = c(age, HI))
canada_phos <- canada_phos[order(canada_phos$age),]

write.csv(canada_phos, "all_intakes/canada_w_phos.csv")

# Men
canada_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_phos")

canada_phos <- subset(canada_phos, select = c(age, HI))
canada_phos <- canada_phos[order(canada_phos$age),]

write.csv(canada_phos, "all_intakes/canada_m_phos.csv")

##################################################################

# 25. RUN SPADE FOR MG

canada_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                  data=canada_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=1, max.age=95,
                  sex.lab="women",
                  weights.name = "weight",
                  output.name = "canada_wom_mg")

canada_mg <- subset(canada_mg, select = c(age, HI))
canada_mg <- canada_mg[order(canada_mg$age),]

write.csv(canada_mg, "all_intakes/canada_w_mg.csv")

# Men
canada_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                  data=canada_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=1, max.age=95,
                  sex.lab="men",
                  weights.name = "weight",
                  output.name = "canada_men_mg")

canada_mg <- subset(canada_mg, select = c(age, HI))
canada_mg <- canada_mg[order(canada_mg$age),]

write.csv(canada_mg, "all_intakes/canada_m_mg.csv")

##################################################################

# 27. RUN SPADE FOR SODIUM

canada_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                  data=canada_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=1, max.age=95,
                  sex.lab="women",
                  weights.name = "weight",
                  output.name = "canada_wom_na")

canada_na <- subset(canada_na, select = c(age, HI))
canada_na <- canada_na[order(canada_na$age),]

write.csv(canada_na, "all_intakes/canada_w_na.csv")

# Men
canada_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                  data=canada_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=1, max.age=95,
                  sex.lab="men",
                  weights.name = "weight",
                  output.name = "canada_men_na")

canada_na <- subset(canada_na, select = c(age, HI))
canada_na <- canada_na[order(canada_na$age),]

write.csv(canada_na, "all_intakes/canada_m_na.csv")

##################################################################

# 28. RUN SPADE FOR POTASSIUM

canada_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=canada_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "canada_wom_pota")

canada_pota <- subset(canada_pota, select = c(age, HI))
canada_pota <- canada_pota[order(canada_pota$age),]

write.csv(canada_pota, "all_intakes/canada_w_pota.csv")

# Men
canada_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=canada_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=95,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "canada_men_pota")

canada_pota <- subset(canada_pota, select = c(age, HI))
canada_pota <- canada_pota[order(canada_pota$age),]

write.csv(canada_pota, "all_intakes/canada_m_pota.csv")
