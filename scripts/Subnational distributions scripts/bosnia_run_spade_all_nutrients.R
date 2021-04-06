# Running SPADE: bosnia data
# File created on 4/6/21 by Simone Passarelli

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "bosnia"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "bosnia"))
###########################################################
# Remove missing obs
summary(bosnia_spade)
names(bosnia_spade)

# number of intakes per person:
table(bosnia_spade$mday)

# Make separate datasets for men and women
bosnia_wom <- subset(bosnia_spade, sex==2)
bosnia_men <- subset(bosnia_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(bosnia_wom$age)
range(bosnia_men$age)
is.integer(bosnia_wom$id)
# Women
bosnia_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=bosnia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=18, max.age=90,
                         sex.lab="women",
                         output.name = "bosnia_wom_vitb12")

bosnia_vitb12 <- subset(bosnia_vitb12, select = c(age, HI))
bosnia_vitb12 <- bosnia_vitb12[order(bosnia_vitb12$age),]

write.csv(bosnia_vitb12, "all_intakes/bosnia_w_vitb12.csv")

# Men
bosnia_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=bosnia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         sex.lab="men",
                         output.name = "bosnia_men_vitb12")

bosnia_vitb12 <- subset(bosnia_vitb12, select = c(age, HI))
bosnia_vitb12 <- bosnia_vitb12[order(bosnia_vitb12$age),]

write.csv(bosnia_vitb12, "all_intakes/bosnia_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
bosnia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       output.name = "bosnia_wom_iron")

bosnia_iron <- subset(bosnia_iron, select = c(age, HI))
bosnia_iron <- bosnia_iron[order(bosnia_iron$age),]

write.csv(bosnia_iron, "all_intakes/bosnia_w_iron.csv")

# Men
bosnia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "bosnia_men_iron")

bosnia_iron <- subset(bosnia_iron, select = c(age, HI))
bosnia_iron <- bosnia_iron[order(bosnia_iron$age),]

write.csv(bosnia_iron, "all_intakes/bosnia_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
bosnia_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=bosnia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=18, max.age=90,
                         sex.lab="women",
                         output.name = "bosnia_wom_zinc")

bosnia_zinc_w <- subset(bosnia_zinc_w, select = c(age, HI))
bosnia_zinc_w <- bosnia_zinc_w[order(bosnia_zinc_w$age),]

write.csv(bosnia_zinc_w, "all_intakes/bosnia_w_zinc.csv")

# Men
bosnia_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=bosnia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         sex.lab="men",
                         output.name = "bosnia_men_zinc")

bosnia_zinc_m <- subset(bosnia_zinc_m, select = c(age, HI))
bosnia_zinc_m <- bosnia_zinc_m[order(bosnia_zinc_m$age),]

write.csv(bosnia_zinc_m, "all_intakes/bosnia_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

bosnia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       output.name = "bosnia_wom_vita")

bosnia_vita <- subset(bosnia_vita, select = c(age, HI))
bosnia_vita <- bosnia_vita[order(bosnia_vita$age),]

write.csv(bosnia_vita, "all_intakes/bosnia_w_vita.csv")

# Men
bosnia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "bosnia_men_vita")

bosnia_vita <- subset(bosnia_vita, select = c(age, HI))
bosnia_vita <- bosnia_vita[order(bosnia_vita$age),]

write.csv(bosnia_vita, "all_intakes/bosnia_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

bosnia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       output.name = "bosnia_wom_calc")

bosnia_calc <- subset(bosnia_calc, select = c(age, HI))
bosnia_calc <- bosnia_calc[order(bosnia_calc$age),]

write.csv(bosnia_calc, "all_intakes/bosnia_w_calc.csv")

# Men
bosnia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "bosnia_men_calc")

bosnia_calc <- subset(bosnia_calc, select = c(age, HI))
bosnia_calc <- bosnia_calc[order(bosnia_calc$age),]

write.csv(bosnia_calc, "all_intakes/bosnia_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

bosnia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=bosnia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=90,
                          sex.lab="women",
                          output.name = "bosnia_wom_omega_3")

bosnia_omega_3 <- subset(bosnia_omega_3, select = c(age, HI))
bosnia_omega_3 <- bosnia_omega_3[order(bosnia_omega_3$age),]

write.csv(bosnia_omega_3, "all_intakes/bosnia_w_omega_3.csv")

# Men
bosnia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=bosnia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=88,
                          sex.lab="men",
                          output.name = "bosnia_men_omega_3")

bosnia_omega_3 <- subset(bosnia_omega_3, select = c(age, HI))
bosnia_omega_3 <- bosnia_omega_3[order(bosnia_omega_3$age),]

write.csv(bosnia_omega_3, "all_intakes/bosnia_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

bosnia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=bosnia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=18, max.age=90,
                         sex.lab="women",
                         output.name = "bosnia_wom_energy")

bosnia_energy <- subset(bosnia_energy, select = c(age, HI))
bosnia_energy <- bosnia_energy[order(bosnia_energy$age),]

write.csv(bosnia_energy, "all_intakes/bosnia_w_energy.csv")

# Men
bosnia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=bosnia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         sex.lab="men",
                         output.name = "bosnia_men_energy")

bosnia_energy <- subset(bosnia_energy, select = c(age, HI))
bosnia_energy <- bosnia_energy[order(bosnia_energy$age),]

write.csv(bosnia_energy, "all_intakes/bosnia_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

bosnia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=bosnia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=90,
                          sex.lab="women",
                          output.name = "bosnia_wom_protein")

bosnia_protein <- subset(bosnia_protein, select = c(age, HI))
bosnia_protein <- bosnia_protein[order(bosnia_protein$age),]

write.csv(bosnia_protein, "all_intakes/bosnia_w_protein.csv")

# Men
bosnia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=bosnia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=88,
                          sex.lab="men",
                          output.name = "bosnia_men_protein")

bosnia_protein <- subset(bosnia_protein, select = c(age, HI))
bosnia_protein <- bosnia_protein[order(bosnia_protein$age),]

write.csv(bosnia_protein, "all_intakes/bosnia_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

bosnia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       output.name = "bosnia_wom_carb")

bosnia_carb <- subset(bosnia_carb, select = c(age, HI))
bosnia_carb <- bosnia_carb[order(bosnia_carb$age),]

write.csv(bosnia_carb, "all_intakes/bosnia_w_carb.csv")

# Men
bosnia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "bosnia_men_carb")

bosnia_carb <- subset(bosnia_carb, select = c(age, HI))
bosnia_carb <- bosnia_carb[order(bosnia_carb$age),]

write.csv(bosnia_carb, "all_intakes/bosnia_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR FIBER

bosnia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=bosnia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=90,
                        sex.lab="women",
                        output.name = "bosnia_wom_fiber")

bosnia_fiber <- subset(bosnia_fiber, select = c(age, HI))
bosnia_fiber <- bosnia_fiber[order(bosnia_fiber$age),]

write.csv(bosnia_fiber, "all_intakes/bosnia_w_fiber.csv")

# Men
bosnia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=bosnia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=19, max.age=88,
                        sex.lab="men",
                        output.name = "bosnia_men_fiber")

bosnia_fiber <- subset(bosnia_fiber, select = c(age, HI))
bosnia_fiber <- bosnia_fiber[order(bosnia_fiber$age),]

write.csv(bosnia_fiber, "all_intakes/bosnia_m_fiber.csv")

##################################################################

# 11. RUN SPADE FOR FAT

bosnia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=bosnia_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=18, max.age=90,
                      sex.lab="women",
                      output.name = "bosnia_wom_fat")

bosnia_fat <- subset(bosnia_fat, select = c(age, HI))
bosnia_fat <- bosnia_fat[order(bosnia_fat$age),]

write.csv(bosnia_fat, "all_intakes/bosnia_w_fat.csv")

# Men
bosnia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=bosnia_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "bosnia_men_fat")

bosnia_fat <- subset(bosnia_fat, select = c(age, HI))
bosnia_fat <- bosnia_fat[order(bosnia_fat$age),]

write.csv(bosnia_fat, "all_intakes/bosnia_m_fat.csv")

##################################################################

# 12. RUN SPADE FOR SATFAT

bosnia_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=bosnia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=18, max.age=90,
                         sex.lab="women",
                         output.name = "bosnia_wom_satfat")

bosnia_satfat <- subset(bosnia_satfat, select = c(age, HI))
bosnia_satfat <- bosnia_satfat[order(bosnia_satfat$age),]

write.csv(bosnia_satfat, "all_intakes/bosnia_w_satfat.csv")

# Men
bosnia_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                         data=bosnia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         sex.lab="men",
                         output.name = "bosnia_men_satfat")

bosnia_satfat <- subset(bosnia_satfat, select = c(age, HI))
bosnia_satfat <- bosnia_satfat[order(bosnia_satfat$age),]

write.csv(bosnia_satfat, "all_intakes/bosnia_m_satfat.csv")

##################################################################

# 13. RUN SPADE FOR MUFA

bosnia_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_mufa")

bosnia_mufa <- subset(bosnia_mufa, select = c(age, HI))
bosnia_mufa <- bosnia_mufa[order(bosnia_mufa$age),]

write.csv(bosnia_mufa, "all_intakes/bosnia_w_mufa.csv")

# Men
bosnia_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_mufa")

bosnia_mufa <- subset(bosnia_mufa, select = c(age, HI))
bosnia_mufa <- bosnia_mufa[order(bosnia_mufa$age),]

write.csv(bosnia_mufa, "all_intakes/bosnia_m_mufa.csv")

##################################################################

# 14. RUN SPADE FOR PUFA

bosnia_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_pufa")

bosnia_pufa <- subset(bosnia_pufa, select = c(age, HI))
bosnia_pufa <- bosnia_pufa[order(bosnia_pufa$age),]

write.csv(bosnia_pufa, "all_intakes/bosnia_w_pufa.csv")

# Men
bosnia_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_pufa")

bosnia_pufa <- subset(bosnia_pufa, select = c(age, HI))
bosnia_pufa <- bosnia_pufa[order(bosnia_pufa$age),]

write.csv(bosnia_pufa, "all_intakes/bosnia_m_pufa.csv")

##################################################################

# 15. RUN SPADE FOR THIAMIN

bosnia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_thia")

bosnia_thia <- subset(bosnia_thia, select = c(age, HI))
bosnia_thia <- bosnia_thia[order(bosnia_thia$age),]

write.csv(bosnia_thia, "all_intakes/bosnia_w_thia.csv")

# Men
bosnia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_thia")

bosnia_thia <- subset(bosnia_thia, select = c(age, HI))
bosnia_thia <- bosnia_thia[order(bosnia_thia$age),]

write.csv(bosnia_thia, "all_intakes/bosnia_m_thia.csv")

##################################################################

# 16. RUN SPADE FOR RIBOFLAVIN

bosnia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_ribo")

bosnia_ribo <- subset(bosnia_ribo, select = c(age, HI))
bosnia_ribo <- bosnia_ribo[order(bosnia_ribo$age),]

write.csv(bosnia_ribo, "all_intakes/bosnia_w_ribo.csv")

# Men
bosnia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_ribo")

bosnia_ribo <- subset(bosnia_ribo, select = c(age, HI))
bosnia_ribo <- bosnia_ribo[order(bosnia_ribo$age),]

write.csv(bosnia_ribo, "all_intakes/bosnia_m_ribo.csv")

##################################################################

# 17. RUN SPADE FOR NIACIN

bosnia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_niac")

bosnia_niac <- subset(bosnia_niac, select = c(age, HI))
bosnia_niac <- bosnia_niac[order(bosnia_niac$age),]

write.csv(bosnia_niac, "all_intakes/bosnia_w_niac.csv")

# Men
bosnia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_niac")

bosnia_niac <- subset(bosnia_niac, select = c(age, HI))
bosnia_niac <- bosnia_niac[order(bosnia_niac$age),]

write.csv(bosnia_niac, "all_intakes/bosnia_m_niac.csv")

##################################################################

# 18. RUN SPADE FOR VITAMIN B6

bosnia_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=bosnia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=90,
                        sex.lab="women",
                        
                        output.name = "bosnia_wom_vitb6")

bosnia_vitb6 <- subset(bosnia_vitb6, select = c(age, HI))
bosnia_vitb6 <- bosnia_vitb6[order(bosnia_vitb6$age),]

write.csv(bosnia_vitb6, "all_intakes/bosnia_w_vitb6.csv")

# Men
bosnia_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=bosnia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=19, max.age=88,
                        sex.lab="men",
                        
                        output.name = "bosnia_men_vitb6")

bosnia_vitb6 <- subset(bosnia_vitb6, select = c(age, HI))
bosnia_vitb6 <- bosnia_vitb6[order(bosnia_vitb6$age),]

write.csv(bosnia_vitb6, "all_intakes/bosnia_m_vitb6.csv")

##################################################################

# 19. RUN SPADE FOR FOLATE

bosnia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_fola")

bosnia_fola <- subset(bosnia_fola, select = c(age, HI))
bosnia_fola <- bosnia_fola[order(bosnia_fola$age),]

write.csv(bosnia_fola, "all_intakes/bosnia_w_fola.csv")

# Men
bosnia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_fola")

bosnia_fola <- subset(bosnia_fola, select = c(age, HI))
bosnia_fola <- bosnia_fola[order(bosnia_fola$age),]

write.csv(bosnia_fola, "all_intakes/bosnia_m_fola.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN D

bosnia_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_vitd")

bosnia_vitd <- subset(bosnia_vitd, select = c(age, HI))
bosnia_vitd <- bosnia_vitd[order(bosnia_vitd$age),]

write.csv(bosnia_vitd, "all_intakes/bosnia_w_vitd.csv")

# Men
bosnia_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_vitd")

bosnia_vitd <- subset(bosnia_vitd, select = c(age, HI))
bosnia_vitd <- bosnia_vitd[order(bosnia_vitd$age),]

write.csv(bosnia_vitd, "all_intakes/bosnia_m_vitd.csv")

##################################################################

# 21. RUN SPADE FOR VITAMIN C

bosnia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_vitc")

bosnia_vitc <- subset(bosnia_vitc, select = c(age, HI))
bosnia_vitc <- bosnia_vitc[order(bosnia_vitc$age),]

write.csv(bosnia_vitc, "all_intakes/bosnia_w_vitc.csv")

# Men
bosnia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_vitc")

bosnia_vitc <- subset(bosnia_vitc, select = c(age, HI))
bosnia_vitc <- bosnia_vitc[order(bosnia_vitc$age),]

write.csv(bosnia_vitc, "all_intakes/bosnia_m_vitc.csv")

##################################################################

# 22. RUN SPADE FOR PHOSPHORUS

bosnia_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_phos")

bosnia_phos <- subset(bosnia_phos, select = c(age, HI))
bosnia_phos <- bosnia_phos[order(bosnia_phos$age),]

write.csv(bosnia_phos, "all_intakes/bosnia_w_phos.csv")

# Men
bosnia_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_phos")

bosnia_phos <- subset(bosnia_phos, select = c(age, HI))
bosnia_phos <- bosnia_phos[order(bosnia_phos$age),]

write.csv(bosnia_phos, "all_intakes/bosnia_m_phos.csv")

##################################################################

# 23. RUN SPADE FOR MG

bosnia_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=bosnia_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=18, max.age=90,
                     sex.lab="women",
                     
                     output.name = "bosnia_wom_mg")

bosnia_mg <- subset(bosnia_mg, select = c(age, HI))
bosnia_mg <- bosnia_mg[order(bosnia_mg$age),]

write.csv(bosnia_mg, "all_intakes/bosnia_w_mg.csv")

# Men
bosnia_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=bosnia_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=88,
                     sex.lab="men",
                     
                     output.name = "bosnia_men_mg")

bosnia_mg <- subset(bosnia_mg, select = c(age, HI))
bosnia_mg <- bosnia_mg[order(bosnia_mg$age),]

write.csv(bosnia_mg, "all_intakes/bosnia_m_mg.csv")

##################################################################

# 24. RUN SPADE FOR SODIUM

bosnia_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=bosnia_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=18, max.age=90,
                     sex.lab="women",
                     
                     output.name = "bosnia_wom_na")

bosnia_na <- subset(bosnia_na, select = c(age, HI))
bosnia_na <- bosnia_na[order(bosnia_na$age),]

write.csv(bosnia_na, "all_intakes/bosnia_w_na.csv")

# Men
bosnia_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=bosnia_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=88,
                     sex.lab="men",
                     
                     output.name = "bosnia_men_na")

bosnia_na <- subset(bosnia_na, select = c(age, HI))
bosnia_na <- bosnia_na[order(bosnia_na$age),]

write.csv(bosnia_na, "all_intakes/bosnia_m_na.csv")

##################################################################

# 25. RUN SPADE FOR POTASSIUM

bosnia_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_pota")

bosnia_pota <- subset(bosnia_pota, select = c(age, HI))
bosnia_pota <- bosnia_pota[order(bosnia_pota$age),]

write.csv(bosnia_pota, "all_intakes/bosnia_w_pota.csv")

# Men
bosnia_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_pota")

bosnia_pota <- subset(bosnia_pota, select = c(age, HI))
bosnia_pota <- bosnia_pota[order(bosnia_pota$age),]

write.csv(bosnia_pota, "all_intakes/bosnia_m_pota.csv")

##################################################################

# 26. RUN SPADE FOR VITAMIN E

bosnia_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       output.name = "bosnia_wom_vite")

bosnia_vite <- subset(bosnia_vite, select = c(age, HI))
bosnia_vite <- bosnia_vite[order(bosnia_vite$age),]

write.csv(bosnia_vite, "all_intakes/bosnia_w_vite.csv")

# Men
bosnia_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "bosnia_men_vite")

bosnia_vite <- subset(bosnia_vite, select = c(age, HI))
bosnia_vite <- bosnia_vite[order(bosnia_vite$age),]

write.csv(bosnia_vite, "all_intakes/bosnia_m_vite.csv")


##################################################################

# 27. RUN SPADE FOR ALCOHOL

bosnia_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if=alcohol ~cs(age),
                          data=bosnia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=90,
                          sex.lab="women",
                          
                          output.name = "bosnia_wom_alcohol")

bosnia_alcohol <- subset(bosnia_alcohol, select = c(age, HI))
bosnia_alcohol <- bosnia_alcohol[order(bosnia_alcohol$age),]

write.csv(bosnia_alcohol, "all_intakes/bosnia_w_alcohol.csv")

# Men
bosnia_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if=alcohol ~cs(age), 
                          data=bosnia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=88,
                          sex.lab="men",
                          
                          output.name = "bosnia_men_alcohol")

bosnia_alcohol <- subset(bosnia_alcohol, select = c(age, HI))
bosnia_alcohol <- bosnia_alcohol[order(bosnia_alcohol$age),]

write.csv(bosnia_alcohol, "all_intakes/bosnia_m_alcohol.csv")


##################################################################

# 28. RUN SPADE FOR SELENIUM

bosnia_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=bosnia_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=18, max.age=90,
                     sex.lab="women",
                     
                     output.name = "bosnia_wom_se")

bosnia_se <- subset(bosnia_se, select = c(age, HI))
bosnia_se <- bosnia_se[order(bosnia_se$age),]

write.csv(bosnia_se, "all_intakes/bosnia_w_se.csv")

# Men
bosnia_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=bosnia_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=88,
                     sex.lab="men",
                     
                     output.name = "bosnia_men_se")

bosnia_se <- subset(bosnia_se, select = c(age, HI))
bosnia_se <- bosnia_se[order(bosnia_se$age),]

write.csv(bosnia_se, "all_intakes/bosnia_m_se.csv")


##################################################################

# 29. RUN SPADE FOR CHOLESTEROL

bosnia_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_cholest")

bosnia_cholest <- subset(bosnia_cholest, select = c(age, HI))
bosnia_cholest <- bosnia_cholest[order(bosnia_cholest$age),]

write.csv(bosnia_cholest, "all_intakes/bosnia_w_cholest.csv")

# Men
bosnia_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_cholest")

bosnia_cholest <- subset(bosnia_cholest, select = c(age, HI))
bosnia_cholest <- bosnia_cholest[order(bosnia_cholest$age),]

write.csv(bosnia_cholest, "all_intakes/bosnia_m_cholest.csv")


##################################################################

# 31. RUN SPADE FOR BETA CAROTENE

bosnia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=bosnia_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=18, max.age=90,
                            sex.lab="women",
                            
                            output.name = "bosnia_wom_betacarot")

bosnia_betacarot <- subset(bosnia_betacarot, select = c(age, HI))
bosnia_betacarot <- bosnia_betacarot[order(bosnia_betacarot$age),]

write.csv(bosnia_betacarot, "all_intakes/bosnia_w_betacarot.csv")

# Men
bosnia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=bosnia_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=19, max.age=88,
                            sex.lab="men",
                            
                            output.name = "bosnia_men_betacarot")

bosnia_betacarot <- subset(bosnia_betacarot, select = c(age, HI))
bosnia_betacarot <- bosnia_betacarot[order(bosnia_betacarot$age),]

write.csv(bosnia_betacarot, "all_intakes/bosnia_m_betacarot.csv")


##################################################################

# 32. RUN SPADE FOR OMEGA 6 

bosnia_omega_6 <- f.spade(frml.ia=omega_6~fp(age), frml.if="no.if", 
                          data=bosnia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=90,
                          sex.lab="women",
                          output.name = "bosnia_wom_omega_6")

bosnia_omega_6 <- subset(bosnia_omega_6, select = c(age, HI))
bosnia_omega_6 <- bosnia_omega_6[order(bosnia_omega_6$age),]

write.csv(bosnia_omega_6, "all_intakes/bosnia_w_omega_6.csv")

# Men
bosnia_omega_6 <- f.spade(frml.ia=omega_6~fp(age),frml.if="no.if", 
                          data=bosnia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=88,
                          sex.lab="men",
                          output.name = "bosnia_men_omega_6")

bosnia_omega_6 <- subset(bosnia_omega_6, select = c(age, HI))
bosnia_omega_6 <- bosnia_omega_6[order(bosnia_omega_6$age),]

write.csv(bosnia_omega_6, "all_intakes/bosnia_m_omega_6.csv")

##################################################################

# 32. RUN SPADE FOR SUGAR

bosnia_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                            data=bosnia_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=18, max.age=90,
                            sex.lab="women",
                            
                            output.name = "bosnia_wom_sugar")

bosnia_sugar <- subset(bosnia_sugar, select = c(age, HI))
bosnia_sugar <- bosnia_sugar[order(bosnia_sugar$age),]

write.csv(bosnia_sugar, "all_intakes/bosnia_w_sugar.csv")

# Men
bosnia_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                            data=bosnia_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=19, max.age=88,
                            sex.lab="men",
                            
                            output.name = "bosnia_men_sugar")

bosnia_sugar <- subset(bosnia_sugar, select = c(age, HI))
bosnia_sugar <- bosnia_sugar[order(bosnia_sugar$age),]

write.csv(bosnia_sugar, "all_intakes/bosnia_m_sugar.csv")


##################################################################

# 33. RUN SPADE FOR VITAMIN K

bosnia_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                            data=bosnia_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=18, max.age=90,
                            sex.lab="women",
                            
                            output.name = "bosnia_wom_vitk")

bosnia_vitk <- subset(bosnia_vitk, select = c(age, HI))
bosnia_vitk <- bosnia_vitk[order(bosnia_vitk$age),]

write.csv(bosnia_vitk, "all_intakes/bosnia_w_vitk.csv")

# Men
bosnia_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                            data=bosnia_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=19, max.age=88,
                            sex.lab="men",
                            
                            output.name = "bosnia_men_vitk")

bosnia_vitk <- subset(bosnia_vitk, select = c(age, HI))
bosnia_vitk <- bosnia_vitk[order(bosnia_vitk$age),]

write.csv(bosnia_vitk, "all_intakes/bosnia_m_vitk.csv")

##################################################################

# 34. RUN SPADE FOR IODINE

bosnia_iodine <- f.spade(frml.ia=iodine~fp(age), frml.if="no.if", 
                       data=bosnia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=90,
                       sex.lab="women",
                       
                       output.name = "bosnia_wom_iodine")

bosnia_iodine <- subset(bosnia_iodine, select = c(age, HI))
bosnia_iodine <- bosnia_iodine[order(bosnia_iodine$age),]

write.csv(bosnia_iodine, "all_intakes/bosnia_w_iodine.csv")

# Men
bosnia_iodine <- f.spade(frml.ia=iodine~fp(age), frml.if="no.if", 
                       data=bosnia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       
                       output.name = "bosnia_men_iodine")

bosnia_iodine <- subset(bosnia_iodine, select = c(age, HI))
bosnia_iodine <- bosnia_iodine[order(bosnia_iodine$age),]

write.csv(bosnia_iodine, "all_intakes/bosnia_m_iodine.csv")