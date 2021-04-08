# Running SPADE: sweden data
# File created on 3/29/21 by Simone Passarelli

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "sweden"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "sweden"))
###########################################################
# Remove missing obs
summary(sweden_spade)
names(sweden_spade)

# number of intakes per person:
table(sweden_spade$mday)

# Make separate datasets for men and women
sweden_wom <- subset(sweden_spade, sex==2)
sweden_men <- subset(sweden_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(sweden_wom$age)
range(sweden_men$age)

# Women
sweden_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=sweden_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="women",
                         output.name = "sweden_wom_vitb12")

sweden_vitb12 <- subset(sweden_vitb12, select = c(age, HI))
sweden_vitb12 <- sweden_vitb12[order(sweden_vitb12$age),]

write.csv(sweden_vitb12, "all_intakes/sweden_w_vitb12.csv")

# Men
sweden_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=sweden_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="men",
                         output.name = "sweden_men_vitb12")

sweden_vitb12 <- subset(sweden_vitb12, select = c(age, HI))
sweden_vitb12 <- sweden_vitb12[order(sweden_vitb12$age),]

write.csv(sweden_vitb12, "all_intakes/sweden_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
sweden_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       output.name = "sweden_wom_iron")

sweden_iron <- subset(sweden_iron, select = c(age, HI))
sweden_iron <- sweden_iron[order(sweden_iron$age),]

write.csv(sweden_iron, "all_intakes/sweden_w_iron.csv")

# Men
sweden_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       output.name = "sweden_men_iron")

sweden_iron <- subset(sweden_iron, select = c(age, HI))
sweden_iron <- sweden_iron[order(sweden_iron$age),]

write.csv(sweden_iron, "all_intakes/sweden_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
sweden_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=sweden_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="women",
                         output.name = "sweden_wom_zinc")

sweden_zinc_w <- subset(sweden_zinc_w, select = c(age, HI))
sweden_zinc_w <- sweden_zinc_w[order(sweden_zinc_w$age),]

write.csv(sweden_zinc_w, "all_intakes/sweden_w_zinc.csv")

# Men
sweden_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=sweden_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="men",
                         output.name = "sweden_men_zinc")

sweden_zinc_m <- subset(sweden_zinc_m, select = c(age, HI))
sweden_zinc_m <- sweden_zinc_m[order(sweden_zinc_m$age),]

write.csv(sweden_zinc_m, "all_intakes/sweden_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

sweden_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       output.name = "sweden_wom_vita")

sweden_vita <- subset(sweden_vita, select = c(age, HI))
sweden_vita <- sweden_vita[order(sweden_vita$age),]

write.csv(sweden_vita, "all_intakes/sweden_w_vita.csv")

# Men
sweden_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       output.name = "sweden_men_vita")

sweden_vita <- subset(sweden_vita, select = c(age, HI))
sweden_vita <- sweden_vita[order(sweden_vita$age),]

write.csv(sweden_vita, "all_intakes/sweden_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

sweden_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       output.name = "sweden_wom_calc")

sweden_calc <- subset(sweden_calc, select = c(age, HI))
sweden_calc <- sweden_calc[order(sweden_calc$age),]

write.csv(sweden_calc, "all_intakes/sweden_w_calc.csv")

# Men
sweden_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       output.name = "sweden_men_calc")

sweden_calc <- subset(sweden_calc, select = c(age, HI))
sweden_calc <- sweden_calc[order(sweden_calc$age),]

write.csv(sweden_calc, "all_intakes/sweden_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

sweden_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=sweden_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=80,
                          sex.lab="women",
                          output.name = "sweden_wom_omega_3")

sweden_omega_3 <- subset(sweden_omega_3, select = c(age, HI))
sweden_omega_3 <- sweden_omega_3[order(sweden_omega_3$age),]

write.csv(sweden_omega_3, "all_intakes/sweden_w_omega_3.csv")

# Men
sweden_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=sweden_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=80,
                          sex.lab="men",
                          output.name = "sweden_men_omega_3")

sweden_omega_3 <- subset(sweden_omega_3, select = c(age, HI))
sweden_omega_3 <- sweden_omega_3[order(sweden_omega_3$age),]

write.csv(sweden_omega_3, "all_intakes/sweden_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

sweden_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=sweden_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="women",
                         output.name = "sweden_wom_energy")

sweden_energy <- subset(sweden_energy, select = c(age, HI))
sweden_energy <- sweden_energy[order(sweden_energy$age),]

write.csv(sweden_energy, "all_intakes/sweden_w_energy.csv")

# Men
sweden_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=sweden_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="men",
                         output.name = "sweden_men_energy")

sweden_energy <- subset(sweden_energy, select = c(age, HI))
sweden_energy <- sweden_energy[order(sweden_energy$age),]

write.csv(sweden_energy, "all_intakes/sweden_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

sweden_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=sweden_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=80,
                          sex.lab="women",
                          output.name = "sweden_wom_protein")

sweden_protein <- subset(sweden_protein, select = c(age, HI))
sweden_protein <- sweden_protein[order(sweden_protein$age),]

write.csv(sweden_protein, "all_intakes/sweden_w_protein.csv")

# Men
sweden_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=sweden_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=80,
                          sex.lab="men",
                          output.name = "sweden_men_protein")

sweden_protein <- subset(sweden_protein, select = c(age, HI))
sweden_protein <- sweden_protein[order(sweden_protein$age),]

write.csv(sweden_protein, "all_intakes/sweden_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

sweden_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       output.name = "sweden_wom_carb")

sweden_carb <- subset(sweden_carb, select = c(age, HI))
sweden_carb <- sweden_carb[order(sweden_carb$age),]

write.csv(sweden_carb, "all_intakes/sweden_w_carb.csv")

# Men
sweden_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       output.name = "sweden_men_carb")

sweden_carb <- subset(sweden_carb, select = c(age, HI))
sweden_carb <- sweden_carb[order(sweden_carb$age),]

write.csv(sweden_carb, "all_intakes/sweden_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR sucrose

sweden_sucrose <- f.spade(frml.ia=sucrose~fp(age), frml.if="no.if", 
                        data=sweden_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="women",
                        output.name = "sweden_wom_sucrose")

sweden_sucrose <- subset(sweden_sucrose, select = c(age, HI))
sweden_sucrose <- sweden_sucrose[order(sweden_sucrose$age),]

write.csv(sweden_sucrose, "all_intakes/sweden_w_sucrose.csv")

# Men
sweden_sucrose <- f.spade(frml.ia=sucrose~fp(age), frml.if="no.if", 
                        data=sweden_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="men",
                        output.name = "sweden_men_sucrose")

sweden_sucrose <- subset(sweden_sucrose, select = c(age, HI))
sweden_sucrose <- sweden_sucrose[order(sweden_sucrose$age),]

write.csv(sweden_sucrose, "all_intakes/sweden_m_sucrose.csv")

##################################################################

# 11. RUN SPADE FOR FIBER

sweden_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=sweden_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="women",
                        output.name = "sweden_wom_fiber")

sweden_fiber <- subset(sweden_fiber, select = c(age, HI))
sweden_fiber <- sweden_fiber[order(sweden_fiber$age),]

write.csv(sweden_fiber, "all_intakes/sweden_w_fiber.csv")

# Men
sweden_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=sweden_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="men",
                        output.name = "sweden_men_fiber")

sweden_fiber <- subset(sweden_fiber, select = c(age, HI))
sweden_fiber <- sweden_fiber[order(sweden_fiber$age),]

write.csv(sweden_fiber, "all_intakes/sweden_m_fiber.csv")

##################################################################

# 12. RUN SPADE FOR FAT

sweden_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=sweden_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=3, max.age=80,
                      sex.lab="women",
                      output.name = "sweden_wom_fat")

sweden_fat <- subset(sweden_fat, select = c(age, HI))
sweden_fat <- sweden_fat[order(sweden_fat$age),]

write.csv(sweden_fat, "all_intakes/sweden_w_fat.csv")

# Men
sweden_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=sweden_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=3, max.age=80,
                      sex.lab="men",
                      output.name = "sweden_men_fat")

sweden_fat <- subset(sweden_fat, select = c(age, HI))
sweden_fat <- sweden_fat[order(sweden_fat$age),]

write.csv(sweden_fat, "all_intakes/sweden_m_fat.csv")

##################################################################

# 13. RUN SPADE FOR SATFAT

sweden_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=sweden_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="women",
                         output.name = "sweden_wom_satfat")

sweden_satfat <- subset(sweden_satfat, select = c(age, HI))
sweden_satfat <- sweden_satfat[order(sweden_satfat$age),]

write.csv(sweden_satfat, "all_intakes/sweden_w_satfat.csv")

# Men
sweden_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=sweden_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=80,
                         sex.lab="men",
                         output.name = "sweden_men_satfat")

sweden_satfat <- subset(sweden_satfat, select = c(age, HI))
sweden_satfat <- sweden_satfat[order(sweden_satfat$age),]

write.csv(sweden_satfat, "all_intakes/sweden_m_satfat.csv")

##################################################################

# 14. RUN SPADE FOR MUFA

sweden_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_mufa")

sweden_mufa <- subset(sweden_mufa, select = c(age, HI))
sweden_mufa <- sweden_mufa[order(sweden_mufa$age),]

write.csv(sweden_mufa, "all_intakes/sweden_w_mufa.csv")

# Men
sweden_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_mufa")

sweden_mufa <- subset(sweden_mufa, select = c(age, HI))
sweden_mufa <- sweden_mufa[order(sweden_mufa$age),]

write.csv(sweden_mufa, "all_intakes/sweden_m_mufa.csv")

##################################################################

# 15. RUN SPADE FOR PUFA

sweden_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_pufa")

sweden_pufa <- subset(sweden_pufa, select = c(age, HI))
sweden_pufa <- sweden_pufa[order(sweden_pufa$age),]

write.csv(sweden_pufa, "all_intakes/sweden_w_pufa.csv")

# Men
sweden_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_pufa")

sweden_pufa <- subset(sweden_pufa, select = c(age, HI))
sweden_pufa <- sweden_pufa[order(sweden_pufa$age),]

write.csv(sweden_pufa, "all_intakes/sweden_m_pufa.csv")

##################################################################

# 16. RUN SPADE FOR THIAMIN

sweden_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_thia")

sweden_thia <- subset(sweden_thia, select = c(age, HI))
sweden_thia <- sweden_thia[order(sweden_thia$age),]

write.csv(sweden_thia, "all_intakes/sweden_w_thia.csv")

# Men
sweden_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_thia")

sweden_thia <- subset(sweden_thia, select = c(age, HI))
sweden_thia <- sweden_thia[order(sweden_thia$age),]

write.csv(sweden_thia, "all_intakes/sweden_m_thia.csv")

##################################################################

# 17. RUN SPADE FOR RIBOFLAVIN

sweden_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_ribo")

sweden_ribo <- subset(sweden_ribo, select = c(age, HI))
sweden_ribo <- sweden_ribo[order(sweden_ribo$age),]

write.csv(sweden_ribo, "all_intakes/sweden_w_ribo.csv")

# Men
sweden_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_ribo")

sweden_ribo <- subset(sweden_ribo, select = c(age, HI))
sweden_ribo <- sweden_ribo[order(sweden_ribo$age),]

write.csv(sweden_ribo, "all_intakes/sweden_m_ribo.csv")

##################################################################

# 18. RUN SPADE FOR NIACIN

sweden_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_niac")

sweden_niac <- subset(sweden_niac, select = c(age, HI))
sweden_niac <- sweden_niac[order(sweden_niac$age),]

write.csv(sweden_niac, "all_intakes/sweden_w_niac.csv")

# Men
sweden_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_niac")

sweden_niac <- subset(sweden_niac, select = c(age, HI))
sweden_niac <- sweden_niac[order(sweden_niac$age),]

write.csv(sweden_niac, "all_intakes/sweden_m_niac.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN B6

sweden_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=sweden_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="women",
                        
                        output.name = "sweden_wom_vitb6")

sweden_vitb6 <- subset(sweden_vitb6, select = c(age, HI))
sweden_vitb6 <- sweden_vitb6[order(sweden_vitb6$age),]

write.csv(sweden_vitb6, "all_intakes/sweden_w_vitb6.csv")

# Men
sweden_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=sweden_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=80,
                        sex.lab="men",
                        
                        output.name = "sweden_men_vitb6")

sweden_vitb6 <- subset(sweden_vitb6, select = c(age, HI))
sweden_vitb6 <- sweden_vitb6[order(sweden_vitb6$age),]

write.csv(sweden_vitb6, "all_intakes/sweden_m_vitb6.csv")

##################################################################

# 20. RUN SPADE FOR FOLATE

sweden_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_fola")

sweden_fola <- subset(sweden_fola, select = c(age, HI))
sweden_fola <- sweden_fola[order(sweden_fola$age),]

write.csv(sweden_fola, "all_intakes/sweden_w_fola.csv")

# Men
sweden_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_fola")

sweden_fola <- subset(sweden_fola, select = c(age, HI))
sweden_fola <- sweden_fola[order(sweden_fola$age),]

write.csv(sweden_fola, "all_intakes/sweden_m_fola.csv")

##################################################################

# 21. RUN SPADE FOR VITAMIN D

sweden_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_vitd")

sweden_vitd <- subset(sweden_vitd, select = c(age, HI))
sweden_vitd <- sweden_vitd[order(sweden_vitd$age),]

write.csv(sweden_vitd, "all_intakes/sweden_w_vitd.csv")

# Men
sweden_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_vitd")

sweden_vitd <- subset(sweden_vitd, select = c(age, HI))
sweden_vitd <- sweden_vitd[order(sweden_vitd$age),]

write.csv(sweden_vitd, "all_intakes/sweden_m_vitd.csv")

##################################################################

# 22. RUN SPADE FOR VITAMIN C

sweden_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_vitc")

sweden_vitc <- subset(sweden_vitc, select = c(age, HI))
sweden_vitc <- sweden_vitc[order(sweden_vitc$age),]

write.csv(sweden_vitc, "all_intakes/sweden_w_vitc.csv")

# Men
sweden_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_vitc")

sweden_vitc <- subset(sweden_vitc, select = c(age, HI))
sweden_vitc <- sweden_vitc[order(sweden_vitc$age),]

write.csv(sweden_vitc, "all_intakes/sweden_m_vitc.csv")

##################################################################

# 23. RUN SPADE FOR PHOSPHORUS

sweden_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_phos")

sweden_phos <- subset(sweden_phos, select = c(age, HI))
sweden_phos <- sweden_phos[order(sweden_phos$age),]

write.csv(sweden_phos, "all_intakes/sweden_w_phos.csv")

# Men
sweden_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_phos")

sweden_phos <- subset(sweden_phos, select = c(age, HI))
sweden_phos <- sweden_phos[order(sweden_phos$age),]

write.csv(sweden_phos, "all_intakes/sweden_m_phos.csv")

##################################################################

# 24. RUN SPADE FOR MG

sweden_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=sweden_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=80,
                     sex.lab="women",
                     
                     output.name = "sweden_wom_mg")

sweden_mg <- subset(sweden_mg, select = c(age, HI))
sweden_mg <- sweden_mg[order(sweden_mg$age),]

write.csv(sweden_mg, "all_intakes/sweden_w_mg.csv")

# Men
sweden_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=sweden_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=80,
                     sex.lab="men",
                     
                     output.name = "sweden_men_mg")

sweden_mg <- subset(sweden_mg, select = c(age, HI))
sweden_mg <- sweden_mg[order(sweden_mg$age),]

write.csv(sweden_mg, "all_intakes/sweden_m_mg.csv")

##################################################################

# 25. RUN SPADE FOR SODIUM

sweden_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=sweden_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=80,
                     sex.lab="women",
                     
                     output.name = "sweden_wom_na")

sweden_na <- subset(sweden_na, select = c(age, HI))
sweden_na <- sweden_na[order(sweden_na$age),]

write.csv(sweden_na, "all_intakes/sweden_w_na.csv")

# Men
sweden_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=sweden_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=80,
                     sex.lab="men",
                     
                     output.name = "sweden_men_na")

sweden_na <- subset(sweden_na, select = c(age, HI))
sweden_na <- sweden_na[order(sweden_na$age),]

write.csv(sweden_na, "all_intakes/sweden_m_na.csv")

##################################################################

# 26. RUN SPADE FOR POTASSIUM

sweden_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_pota")

sweden_pota <- subset(sweden_pota, select = c(age, HI))
sweden_pota <- sweden_pota[order(sweden_pota$age),]

write.csv(sweden_pota, "all_intakes/sweden_w_pota.csv")

# Men
sweden_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_pota")

sweden_pota <- subset(sweden_pota, select = c(age, HI))
sweden_pota <- sweden_pota[order(sweden_pota$age),]

write.csv(sweden_pota, "all_intakes/sweden_m_pota.csv")

##################################################################

# 27. RUN SPADE FOR VITAMIN E

sweden_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       output.name = "sweden_wom_vite")

sweden_vite <- subset(sweden_vite, select = c(age, HI))
sweden_vite <- sweden_vite[order(sweden_vite$age),]

write.csv(sweden_vite, "all_intakes/sweden_w_vite.csv")

# Men
sweden_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       output.name = "sweden_men_vite")

sweden_vite <- subset(sweden_vite, select = c(age, HI))
sweden_vite <- sweden_vite[order(sweden_vite$age),]

write.csv(sweden_vite, "all_intakes/sweden_m_vite.csv")


##################################################################

# 28. RUN SPADE FOR ALCOHOL

sweden_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_alcohol")

sweden_alcohol <- subset(sweden_alcohol, select = c(age, HI))
sweden_alcohol <- sweden_alcohol[order(sweden_alcohol$age),]

write.csv(sweden_alcohol, "all_intakes/sweden_w_alcohol.csv")

# Men
sweden_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_alcohol")

sweden_alcohol <- subset(sweden_alcohol, select = c(age, HI))
sweden_alcohol <- sweden_alcohol[order(sweden_alcohol$age),]

write.csv(sweden_alcohol, "all_intakes/sweden_m_alcohol.csv")


##################################################################

# 29. RUN SPADE FOR SELENIUM

sweden_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_se")

sweden_se <- subset(sweden_se, select = c(age, HI))
sweden_se <- sweden_se[order(sweden_se$age),]

write.csv(sweden_se, "all_intakes/sweden_w_se.csv")

# Men
sweden_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_se")

sweden_se <- subset(sweden_se, select = c(age, HI))
sweden_se <- sweden_se[order(sweden_se$age),]

write.csv(sweden_se, "all_intakes/sweden_m_se.csv")


##################################################################

# 30. RUN SPADE FOR CHOLINE

sweden_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_chol")

sweden_chol <- subset(sweden_chol, select = c(age, HI))
sweden_chol <- sweden_chol[order(sweden_chol$age),]

write.csv(sweden_chol, "all_intakes/sweden_w_chol.csv")

# Men
sweden_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_chol")

sweden_chol <- subset(sweden_chol, select = c(age, HI))
sweden_chol <- sweden_chol[order(sweden_chol$age),]

write.csv(sweden_chol, "all_intakes/sweden_m_chol.csv")


##################################################################

# 31. RUN SPADE FOR BETA CAROTENE

sweden_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_betacarot")

sweden_betacarot <- subset(sweden_betacarot, select = c(age, HI))
sweden_betacarot <- sweden_betacarot[order(sweden_betacarot$age),]

write.csv(sweden_betacarot, "all_intakes/sweden_w_betacarot.csv")

# Men
sweden_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_betacarot")

sweden_betacarot <- subset(sweden_betacarot, select = c(age, HI))
sweden_betacarot <- sweden_betacarot[order(sweden_betacarot$age),]

write.csv(sweden_betacarot, "all_intakes/sweden_m_betacarot.csv")


##################################################################

# 32. RUN SPADE FOR RETINOL

sweden_retinol <- f.spade(frml.ia=retinol~fp(age), frml.if="no.if", 
                       data=sweden_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="women",
                       
                       output.name = "sweden_wom_retinol")

sweden_retinol <- subset(sweden_retinol, select = c(age, HI))
sweden_retinol <- sweden_retinol[order(sweden_retinol$age),]

write.csv(sweden_retinol, "all_intakes/sweden_w_retinol.csv")

# Men
sweden_retinol <- f.spade(frml.ia=retinol~fp(age), frml.if="no.if", 
                       data=sweden_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=80,
                       sex.lab="men",
                       
                       output.name = "sweden_men_retinol")

sweden_retinol <- subset(sweden_retinol, select = c(age, HI))
sweden_retinol <- sweden_retinol[order(sweden_retinol$age),]

write.csv(sweden_retinol, "all_intakes/sweden_m_retinol.csv")

