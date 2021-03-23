# Running SPADE: portugal data
# Created by Simone Passarelli on March 18 2021


# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "portugal"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Portugal"))
###########################################################
# Remove missing obs
summary(portugal_spade)
portugal_spade <- na.omit(portugal_spade)
summary(portugal_spade)
names(portugal_spade)

#remove obs with weights=0
portugal_spade <- subset(portugal_spade, weight !=0)
# Remove instance where there are 


# number of intakes per person:
table(portugal_spade$mday)

# Make separate datasets for men and women
portugal_wom <- subset(portugal_spade, sex==2)
portugal_men <- subset(portugal_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(portugal_wom$age)
range(portugal_men$age)


# Some outliers that require cleaning
portugal_wom_vitb12 <- subset(portugal_wom, vitb12<1000)

# Women
portugal_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=portugal_wom_vitb12, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "portugal_wom_vitb12")

portugal_vitb12 <- subset(portugal_vitb12, select = c(age, HI))
portugal_vitb12 <- portugal_vitb12[order(portugal_vitb12$age),]

write.csv(portugal_vitb12, "all_intakes/portugal_w_vitb12.csv")

# Men
# Some outliers that require cleaning
portugal_men_vitb12 <- subset(portugal_men, vitb12<1000)

portugal_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=portugal_men_vitb12, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "portugal_men_vitb12")

portugal_vitb12 <- subset(portugal_vitb12, select = c(age, HI))
portugal_vitb12 <- portugal_vitb12[order(portugal_vitb12$age),]

write.csv(portugal_vitb12, "all_intakes/portugal_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women

portugal_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_iron")

portugal_iron <- subset(portugal_iron, select = c(age, HI))
portugal_iron <- portugal_iron[order(portugal_iron$age),]

write.csv(portugal_iron, "all_intakes/portugal_w_iron.csv")

# Men
# CLean some outliers
portugal_men_iron <- subset(portugal_men, iron<1000)

portugal_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=portugal_men_iron, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_iron")

portugal_iron <- subset(portugal_iron, select = c(age, HI))
portugal_iron <- portugal_iron[order(portugal_iron$age),]

write.csv(portugal_iron, "all_intakes/portugal_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
portugal_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=portugal_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "portugal_wom_zinc")

portugal_zinc_w <- subset(portugal_zinc_w, select = c(age, HI))
portugal_zinc_w <- portugal_zinc_w[order(portugal_zinc_w$age),]

write.csv(portugal_zinc_w, "all_intakes/portugal_w_zinc.csv")

# Men
portugal_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=portugal_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "portugal_men_zinc")

portugal_zinc_m <- subset(portugal_zinc_m, select = c(age, HI))
portugal_zinc_m <- portugal_zinc_m[order(portugal_zinc_m$age),]

write.csv(portugal_zinc_m, "all_intakes/portugal_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

portugal_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_vita")

portugal_vita <- subset(portugal_vita, select = c(age, HI))
portugal_vita <- portugal_vita[order(portugal_vita$age),]

write.csv(portugal_vita, "all_intakes/portugal_w_vita.csv")

# Men
portugal_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_vita")

portugal_vita <- subset(portugal_vita, select = c(age, HI))
portugal_vita <- portugal_vita[order(portugal_vita$age),]

write.csv(portugal_vita, "all_intakes/portugal_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

portugal_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_calc")

portugal_calc <- subset(portugal_calc, select = c(age, HI))
portugal_calc <- portugal_calc[order(portugal_calc$age),]

write.csv(portugal_calc, "all_intakes/portugal_w_calc.csv")

# Men
portugal_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_calc")

portugal_calc <- subset(portugal_calc, select = c(age, HI))
portugal_calc <- portugal_calc[order(portugal_calc$age),]

write.csv(portugal_calc, "all_intakes/portugal_m_calc.csv")


##################################################################
# 6. RUN SPADE FOR OMEGA 3 

portugal_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                       data=portugal_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=84,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "portugal_wom_omega_3")

portugal_omega_3 <- subset(portugal_omega_3, select = c(age, HI))
portugal_omega_3 <- portugal_omega_3[order(portugal_omega_3$age),]

write.csv(portugal_omega_3, "all_intakes/portugal_w_omega_3.csv")

# Men
portugal_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                       data=portugal_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=84,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "portugal_men_omega_3")

portugal_omega_3 <- subset(portugal_omega_3, select = c(age, HI))
portugal_omega_3 <- portugal_omega_3[order(portugal_omega_3$age),]

write.csv(portugal_omega_3, "all_intakes/portugal_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITAMIN E

portugal_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_vite")

portugal_vite <- subset(portugal_vite, select = c(age, HI))
portugal_vite <- portugal_vite[order(portugal_vite$age),]

write.csv(portugal_vite, "all_intakes/portugal_w_vite.csv")

# Men
portugal_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_vite")

portugal_vite <- subset(portugal_vite, select = c(age, HI))
portugal_vite <- portugal_vite[order(portugal_vite$age),]

write.csv(portugal_vite, "all_intakes/portugal_m_vite.csv")

##################################################################

# 8. RUN SPADE FOR ENERGY

portugal_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                      data=portugal_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "portugal_wom_energy")

portugal_energy <- subset(portugal_energy, select = c(age, HI))
portugal_energy <- portugal_energy[order(portugal_energy$age),]

write.csv(portugal_energy, "all_intakes/portugal_w_energy.csv")

# Men
portugal_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                      data=portugal_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "portugal_men_energy")

portugal_energy <- subset(portugal_energy, select = c(age, HI))
portugal_energy <- portugal_energy[order(portugal_energy$age),]

write.csv(portugal_energy, "all_intakes/portugal_m_energy.csv")

##################################################################

# 9. RUN SPADE FOR PROTEIN

portugal_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                       data=portugal_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=84,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "portugal_wom_protein")

portugal_protein <- subset(portugal_protein, select = c(age, HI))
portugal_protein <- portugal_protein[order(portugal_protein$age),]

write.csv(portugal_protein, "all_intakes/portugal_w_protein.csv")

# Men
portugal_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                       data=portugal_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=84,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "portugal_men_protein")

portugal_protein <- subset(portugal_protein, select = c(age, HI))
portugal_protein <- portugal_protein[order(portugal_protein$age),]

write.csv(portugal_protein, "all_intakes/portugal_m_protein.csv")

##################################################################

# 10. RUN SPADE FOR CARB

portugal_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_carb")

portugal_carb <- subset(portugal_carb, select = c(age, HI))
portugal_carb <- portugal_carb[order(portugal_carb$age),]

write.csv(portugal_carb, "all_intakes/portugal_w_carb.csv")

# Men
portugal_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_carb")

portugal_carb <- subset(portugal_carb, select = c(age, HI))
portugal_carb <- portugal_carb[order(portugal_carb$age),]

write.csv(portugal_carb, "all_intakes/portugal_m_carb.csv")

##################################################################

# 11. RUN SPADE FOR TRANS FAT

portugal_tfat <- f.spade(frml.ia=tfat~fp(age), frml.if="no.if", 
                     data=portugal_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "portugal_wom_tfat")

portugal_tfat <- subset(portugal_tfat, select = c(age, HI))
portugal_tfat <- portugal_tfat[order(portugal_tfat$age),]

write.csv(portugal_tfat, "all_intakes/portugal_w_tfat.csv")

# Men
portugal_tfat <- f.spade(frml.ia=tfat~fp(age), frml.if="no.if", 
                     data=portugal_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "portugal_men_tfat")

portugal_tfat <- subset(portugal_tfat, select = c(age, HI))
portugal_tfat <- portugal_tfat[order(portugal_tfat$age),]

write.csv(portugal_tfat, "all_intakes/portugal_m_tfat.csv")

##################################################################

# 12. RUN SPADE FOR FIBER

portugal_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                     data=portugal_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "portugal_wom_fiber")

portugal_fiber <- subset(portugal_fiber, select = c(age, HI))
portugal_fiber <- portugal_fiber[order(portugal_fiber$age),]

write.csv(portugal_fiber, "all_intakes/portugal_w_fiber.csv")

# Men
portugal_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                     data=portugal_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "portugal_men_fiber")

portugal_fiber <- subset(portugal_fiber, select = c(age, HI))
portugal_fiber <- portugal_fiber[order(portugal_fiber$age),]

write.csv(portugal_fiber, "all_intakes/portugal_m_fiber.csv")

##################################################################

# 13. RUN SPADE FOR FAT

portugal_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                   data=portugal_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=84,
                   sex.lab="women",
                   weights.name = "weight",
                   output.name = "portugal_wom_fat")

portugal_fat <- subset(portugal_fat, select = c(age, HI))
portugal_fat <- portugal_fat[order(portugal_fat$age),]

write.csv(portugal_fat, "all_intakes/portugal_w_fat.csv")

# Men
portugal_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                   data=portugal_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=84,
                   sex.lab="men",
                   weights.name = "weight",
                   output.name = "portugal_men_fat")

portugal_fat <- subset(portugal_fat, select = c(age, HI))
portugal_fat <- portugal_fat[order(portugal_fat$age),]

write.csv(portugal_fat, "all_intakes/portugal_m_fat.csv")

##################################################################

# 14. RUN SPADE FOR SATFAT

portugal_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                      data=portugal_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "portugal_wom_satfat")

portugal_satfat <- subset(portugal_satfat, select = c(age, HI))
portugal_satfat <- portugal_satfat[order(portugal_satfat$age),]

write.csv(portugal_satfat, "all_intakes/portugal_w_satfat.csv")

# Men
portugal_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                      data=portugal_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=84,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "portugal_men_satfat")

portugal_satfat <- subset(portugal_satfat, select = c(age, HI))
portugal_satfat <- portugal_satfat[order(portugal_satfat$age),]

write.csv(portugal_satfat, "all_intakes/portugal_m_satfat.csv")

##################################################################

# 15. RUN SPADE FOR MUFA

portugal_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_mufa")

portugal_mufa <- subset(portugal_mufa, select = c(age, HI))
portugal_mufa <- portugal_mufa[order(portugal_mufa$age),]

write.csv(portugal_mufa, "all_intakes/portugal_w_mufa.csv")

# Men
portugal_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_mufa")

portugal_mufa <- subset(portugal_mufa, select = c(age, HI))
portugal_mufa <- portugal_mufa[order(portugal_mufa$age),]

write.csv(portugal_mufa, "all_intakes/portugal_m_mufa.csv")

##################################################################

# 16. RUN SPADE FOR PUFA

portugal_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_pufa")

portugal_pufa <- subset(portugal_pufa, select = c(age, HI))
portugal_pufa <- portugal_pufa[order(portugal_pufa$age),]

write.csv(portugal_pufa, "all_intakes/portugal_w_pufa.csv")

# Men
portugal_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_pufa")

portugal_pufa <- subset(portugal_pufa, select = c(age, HI))
portugal_pufa <- portugal_pufa[order(portugal_pufa$age),]

write.csv(portugal_pufa, "all_intakes/portugal_m_pufa.csv")

##################################################################

# 17. RUN SPADE FOR THIAMIN

portugal_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_thia")

portugal_thia <- subset(portugal_thia, select = c(age, HI))
portugal_thia <- portugal_thia[order(portugal_thia$age),]

write.csv(portugal_thia, "all_intakes/portugal_w_thia.csv")

# Men
portugal_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_thia")

portugal_thia <- subset(portugal_thia, select = c(age, HI))
portugal_thia <- portugal_thia[order(portugal_thia$age),]

write.csv(portugal_thia, "all_intakes/portugal_m_thia.csv")

##################################################################

# 18. RUN SPADE FOR RIBOFLAVIN

portugal_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_ribo")

portugal_ribo <- subset(portugal_ribo, select = c(age, HI))
portugal_ribo <- portugal_ribo[order(portugal_ribo$age),]

write.csv(portugal_ribo, "all_intakes/portugal_w_ribo.csv")

# Men
portugal_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_ribo")

portugal_ribo <- subset(portugal_ribo, select = c(age, HI))
portugal_ribo <- portugal_ribo[order(portugal_ribo$age),]

write.csv(portugal_ribo, "all_intakes/portugal_m_ribo.csv")

##################################################################

# 19. RUN SPADE FOR NIACIN

portugal_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_niac")

portugal_niac <- subset(portugal_niac, select = c(age, HI))
portugal_niac <- portugal_niac[order(portugal_niac$age),]

write.csv(portugal_niac, "all_intakes/portugal_w_niac.csv")

# Men
portugal_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_niac")

portugal_niac <- subset(portugal_niac, select = c(age, HI))
portugal_niac <- portugal_niac[order(portugal_niac$age),]

write.csv(portugal_niac, "all_intakes/portugal_m_niac.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN B6

# Remove some outliers
portugal_wom_b6 <- subset(portugal_wom, vitb6<500)

portugal_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                     data=portugal_wom_b6, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "portugal_wom_vitb6")

portugal_vitb6 <- subset(portugal_vitb6, select = c(age, HI))
portugal_vitb6 <- portugal_vitb6[order(portugal_vitb6$age),]

write.csv(portugal_vitb6, "all_intakes/portugal_w_vitb6.csv")

# Men
portugal_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                     data=portugal_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=0, max.age=84,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "portugal_men_vitb6")

portugal_vitb6 <- subset(portugal_vitb6, select = c(age, HI))
portugal_vitb6 <- portugal_vitb6[order(portugal_vitb6$age),]

write.csv(portugal_vitb6, "all_intakes/portugal_m_vitb6.csv")

##################################################################

# 21. RUN SPADE FOR FOLATE

portugal_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_fola")

portugal_fola <- subset(portugal_fola, select = c(age, HI))
portugal_fola <- portugal_fola[order(portugal_fola$age),]

write.csv(portugal_fola, "all_intakes/portugal_w_fola.csv")

# Men
portugal_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_fola")

portugal_fola <- subset(portugal_fola, select = c(age, HI))
portugal_fola <- portugal_fola[order(portugal_fola$age),]

write.csv(portugal_fola, "all_intakes/portugal_m_fola.csv")

##################################################################

# 22. RUN SPADE FOR VITAMIN C

portugal_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_vitc")

portugal_vitc <- subset(portugal_vitc, select = c(age, HI))
portugal_vitc <- portugal_vitc[order(portugal_vitc$age),]

write.csv(portugal_vitc, "all_intakes/portugal_w_vitc.csv")

# Men
portugal_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_vitc")

portugal_vitc <- subset(portugal_vitc, select = c(age, HI))
portugal_vitc <- portugal_vitc[order(portugal_vitc$age),]

write.csv(portugal_vitc, "all_intakes/portugal_m_vitc.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN D

portugal_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_vitd")

portugal_vitd <- subset(portugal_vitd, select = c(age, HI))
portugal_vitd <- portugal_vitd[order(portugal_vitd$age),]

write.csv(portugal_vitd, "all_intakes/portugal_w_vitd.csv")

# Men
portugal_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_vitd")

portugal_vitd <- subset(portugal_vitd, select = c(age, HI))
portugal_vitd <- portugal_vitd[order(portugal_vitd$age),]

write.csv(portugal_vitd, "all_intakes/portugal_m_vitd.csv")

##################################################################

# 24. RUN SPADE FOR PHOSPHORUS

portugal_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_phos")

portugal_phos <- subset(portugal_phos, select = c(age, HI))
portugal_phos <- portugal_phos[order(portugal_phos$age),]

write.csv(portugal_phos, "all_intakes/portugal_w_phos.csv")

# Men
portugal_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_phos")

portugal_phos <- subset(portugal_phos, select = c(age, HI))
portugal_phos <- portugal_phos[order(portugal_phos$age),]

write.csv(portugal_phos, "all_intakes/portugal_m_phos.csv")

##################################################################

# 25. RUN SPADE FOR MG

portugal_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                  data=portugal_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=84,
                  sex.lab="women",
                  weights.name = "weight",
                  output.name = "portugal_wom_mg")

portugal_mg <- subset(portugal_mg, select = c(age, HI))
portugal_mg <- portugal_mg[order(portugal_mg$age),]

write.csv(portugal_mg, "all_intakes/portugal_w_mg.csv")

# Men
portugal_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                  data=portugal_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=84,
                  sex.lab="men",
                  weights.name = "weight",
                  output.name = "portugal_men_mg")

portugal_mg <- subset(portugal_mg, select = c(age, HI))
portugal_mg <- portugal_mg[order(portugal_mg$age),]

write.csv(portugal_mg, "all_intakes/portugal_m_mg.csv")


##################################################################

# 26. RUN SPADE FOR SODIUM

portugal_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                  data=portugal_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=84,
                  sex.lab="women",
                  weights.name = "weight",
                  output.name = "portugal_wom_na")

portugal_na <- subset(portugal_na, select = c(age, HI))
portugal_na <- portugal_na[order(portugal_na$age),]

write.csv(portugal_na, "all_intakes/portugal_w_na.csv")

# Men
portugal_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                  data=portugal_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=84,
                  sex.lab="men",
                  weights.name = "weight",
                  output.name = "portugal_men_na")

portugal_na <- subset(portugal_na, select = c(age, HI))
portugal_na <- portugal_na[order(portugal_na$age),]

write.csv(portugal_na, "all_intakes/portugal_m_na.csv")

##################################################################

# 27. RUN SPADE FOR POTASSIUM

portugal_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=portugal_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "portugal_wom_pota")

portugal_pota <- subset(portugal_pota, select = c(age, HI))
portugal_pota <- portugal_pota[order(portugal_pota$age),]

write.csv(portugal_pota, "all_intakes/portugal_w_pota.csv")

# Men
portugal_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=portugal_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=84,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "portugal_men_pota")

portugal_pota <- subset(portugal_pota, select = c(age, HI))
portugal_pota <- portugal_pota[order(portugal_pota$age),]

write.csv(portugal_pota, "all_intakes/portugal_m_pota.csv")
