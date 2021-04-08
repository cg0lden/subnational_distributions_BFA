# Running SPADE: estonia data
# Created by Simone Passarelli on March 18 2021
# Updated on March 25 to remove supplements

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "estonia"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "estonia"))
###########################################################
# Remove missing obs
summary(estonia_spade)
names(estonia_spade)

# number of intakes per person:
table(estonia_spade$mday)

# Make separate datasets for men and women
estonia_wom <- subset(estonia_spade, sex==2)
estonia_men <- subset(estonia_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(estonia_wom$age)
range(estonia_men$age)


# Women
estonia_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                           data=estonia_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=74,
                           sex.lab="women",
                           output.name = "estonia_wom_vitb12")

estonia_vitb12 <- subset(estonia_vitb12, select = c(age, HI))
estonia_vitb12 <- estonia_vitb12[order(estonia_vitb12$age),]

write.csv(estonia_vitb12, "all_intakes/estonia_w_vitb12.csv")

# Men

estonia_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                           data=estonia_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=75,
                           sex.lab="men",
                          
                           output.name = "estonia_men_vitb12")

estonia_vitb12 <- subset(estonia_vitb12, select = c(age, HI))
estonia_vitb12 <- estonia_vitb12[order(estonia_vitb12$age),]

write.csv(estonia_vitb12, "all_intakes/estonia_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women


estonia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_iron")

estonia_iron <- subset(estonia_iron, select = c(age, HI))
estonia_iron <- estonia_iron[order(estonia_iron$age),]

write.csv(estonia_iron, "all_intakes/estonia_w_iron.csv")

# Men

estonia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_iron")

estonia_iron <- subset(estonia_iron, select = c(age, HI))
estonia_iron <- estonia_iron[order(estonia_iron$age),]

write.csv(estonia_iron, "all_intakes/estonia_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
estonia_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                           data=estonia_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=74,
                           sex.lab="women",
                           output.name = "estonia_wom_zinc")

estonia_zinc_w <- subset(estonia_zinc_w, select = c(age, HI))
estonia_zinc_w <- estonia_zinc_w[order(estonia_zinc_w$age),]

write.csv(estonia_zinc_w, "all_intakes/estonia_w_zinc.csv")

# Men
estonia_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                           data=estonia_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=75,
                           sex.lab="men",
                          
                           output.name = "estonia_men_zinc")

estonia_zinc_m <- subset(estonia_zinc_m, select = c(age, HI))
estonia_zinc_m <- estonia_zinc_m[order(estonia_zinc_m$age),]

write.csv(estonia_zinc_m, "all_intakes/estonia_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

estonia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_vita")

estonia_vita <- subset(estonia_vita, select = c(age, HI))
estonia_vita <- estonia_vita[order(estonia_vita$age),]

write.csv(estonia_vita, "all_intakes/estonia_w_vita.csv")

# Men
estonia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_vita")

estonia_vita <- subset(estonia_vita, select = c(age, HI))
estonia_vita <- estonia_vita[order(estonia_vita$age),]

write.csv(estonia_vita, "all_intakes/estonia_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

estonia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_calc")

estonia_calc <- subset(estonia_calc, select = c(age, HI))
estonia_calc <- estonia_calc[order(estonia_calc$age),]

write.csv(estonia_calc, "all_intakes/estonia_w_calc.csv")

# Men
estonia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_calc")

estonia_calc <- subset(estonia_calc, select = c(age, HI))
estonia_calc <- estonia_calc[order(estonia_calc$age),]

write.csv(estonia_calc, "all_intakes/estonia_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

estonia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                            data=estonia_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=11, max.age=74,
                            sex.lab="women",
                           
                            output.name = "estonia_wom_omega_3")

estonia_omega_3 <- subset(estonia_omega_3, select = c(age, HI))
estonia_omega_3 <- estonia_omega_3[order(estonia_omega_3$age),]

write.csv(estonia_omega_3, "all_intakes/estonia_w_omega_3.csv")

# Men
estonia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                            data=estonia_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=11, max.age=75,
                            sex.lab="men",
                           
                            output.name = "estonia_men_omega_3")

estonia_omega_3 <- subset(estonia_omega_3, select = c(age, HI))
estonia_omega_3 <- estonia_omega_3[order(estonia_omega_3$age),]

write.csv(estonia_omega_3, "all_intakes/estonia_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITAMIN E

estonia_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_vite")

estonia_vite <- subset(estonia_vite, select = c(age, HI))
estonia_vite <- estonia_vite[order(estonia_vite$age),]

write.csv(estonia_vite, "all_intakes/estonia_w_vite.csv")

# Men
estonia_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_vite")

estonia_vite <- subset(estonia_vite, select = c(age, HI))
estonia_vite <- estonia_vite[order(estonia_vite$age),]

write.csv(estonia_vite, "all_intakes/estonia_m_vite.csv")

##################################################################

# 8. RUN SPADE FOR ENERGY

estonia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                           data=estonia_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=74,
                           sex.lab="women",
                          
                           output.name = "estonia_wom_energy")

estonia_energy <- subset(estonia_energy, select = c(age, HI))
estonia_energy <- estonia_energy[order(estonia_energy$age),]

write.csv(estonia_energy, "all_intakes/estonia_w_energy.csv")

# Men
estonia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                           data=estonia_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=75,
                           sex.lab="men",
                          
                           output.name = "estonia_men_energy")

estonia_energy <- subset(estonia_energy, select = c(age, HI))
estonia_energy <- estonia_energy[order(estonia_energy$age),]

write.csv(estonia_energy, "all_intakes/estonia_m_energy.csv")

##################################################################

# 9. RUN SPADE FOR PROTEIN

estonia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                            data=estonia_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=11, max.age=74,
                            sex.lab="women",
                           
                            output.name = "estonia_wom_protein")

estonia_protein <- subset(estonia_protein, select = c(age, HI))
estonia_protein <- estonia_protein[order(estonia_protein$age),]

write.csv(estonia_protein, "all_intakes/estonia_w_protein.csv")

# Men
estonia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                            data=estonia_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=11, max.age=75,
                            sex.lab="men",
                           
                            output.name = "estonia_men_protein")

estonia_protein <- subset(estonia_protein, select = c(age, HI))
estonia_protein <- estonia_protein[order(estonia_protein$age),]

write.csv(estonia_protein, "all_intakes/estonia_m_protein.csv")

##################################################################

# 10. RUN SPADE FOR CARB

estonia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_carb")

estonia_carb <- subset(estonia_carb, select = c(age, HI))
estonia_carb <- estonia_carb[order(estonia_carb$age),]

write.csv(estonia_carb, "all_intakes/estonia_w_carb.csv")

# Men
estonia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_carb")

estonia_carb <- subset(estonia_carb, select = c(age, HI))
estonia_carb <- estonia_carb[order(estonia_carb$age),]

write.csv(estonia_carb, "all_intakes/estonia_m_carb.csv")


##################################################################

# 11. RUN SPADE FOR FIBER

estonia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                          data=estonia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=11, max.age=74,
                          sex.lab="women",
                         
                          output.name = "estonia_wom_fiber")

estonia_fiber <- subset(estonia_fiber, select = c(age, HI))
estonia_fiber <- estonia_fiber[order(estonia_fiber$age),]

write.csv(estonia_fiber, "all_intakes/estonia_w_fiber.csv")

# Men
estonia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                          data=estonia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=11, max.age=75,
                          sex.lab="men",
                         
                          output.name = "estonia_men_fiber")

estonia_fiber <- subset(estonia_fiber, select = c(age, HI))
estonia_fiber <- estonia_fiber[order(estonia_fiber$age),]

write.csv(estonia_fiber, "all_intakes/estonia_m_fiber.csv")

##################################################################

# 12. RUN SPADE FOR FAT

estonia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                        data=estonia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=74,
                        sex.lab="women",
                       
                        output.name = "estonia_wom_fat")

estonia_fat <- subset(estonia_fat, select = c(age, HI))
estonia_fat <- estonia_fat[order(estonia_fat$age),]

write.csv(estonia_fat, "all_intakes/estonia_w_fat.csv")

# Men
estonia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                        data=estonia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=75,
                        sex.lab="men",
                       
                        output.name = "estonia_men_fat")

estonia_fat <- subset(estonia_fat, select = c(age, HI))
estonia_fat <- estonia_fat[order(estonia_fat$age),]

write.csv(estonia_fat, "all_intakes/estonia_m_fat.csv")

##################################################################

# 13. RUN SPADE FOR SATFAT

estonia_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                           data=estonia_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=74,
                           sex.lab="women",
                          
                           output.name = "estonia_wom_satfat")

estonia_satfat <- subset(estonia_satfat, select = c(age, HI))
estonia_satfat <- estonia_satfat[order(estonia_satfat$age),]

write.csv(estonia_satfat, "all_intakes/estonia_w_satfat.csv")

# Men
estonia_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                           data=estonia_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=11, max.age=75,
                           sex.lab="men",
                          
                           output.name = "estonia_men_satfat")

estonia_satfat <- subset(estonia_satfat, select = c(age, HI))
estonia_satfat <- estonia_satfat[order(estonia_satfat$age),]

write.csv(estonia_satfat, "all_intakes/estonia_m_satfat.csv")

##################################################################

# 14. RUN SPADE FOR MUFA

estonia_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_mufa")

estonia_mufa <- subset(estonia_mufa, select = c(age, HI))
estonia_mufa <- estonia_mufa[order(estonia_mufa$age),]

write.csv(estonia_mufa, "all_intakes/estonia_w_mufa.csv")

# Men
estonia_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_mufa")

estonia_mufa <- subset(estonia_mufa, select = c(age, HI))
estonia_mufa <- estonia_mufa[order(estonia_mufa$age),]

write.csv(estonia_mufa, "all_intakes/estonia_m_mufa.csv")

##################################################################

# 16. RUN SPADE FOR PUFA

estonia_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_pufa")

estonia_pufa <- subset(estonia_pufa, select = c(age, HI))
estonia_pufa <- estonia_pufa[order(estonia_pufa$age),]

write.csv(estonia_pufa, "all_intakes/estonia_w_pufa.csv")

# Men
estonia_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_pufa")

estonia_pufa <- subset(estonia_pufa, select = c(age, HI))
estonia_pufa <- estonia_pufa[order(estonia_pufa$age),]

write.csv(estonia_pufa, "all_intakes/estonia_m_pufa.csv")

##################################################################

# 17. RUN SPADE FOR THIAMIN

estonia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_thia")

estonia_thia <- subset(estonia_thia, select = c(age, HI))
estonia_thia <- estonia_thia[order(estonia_thia$age),]

write.csv(estonia_thia, "all_intakes/estonia_w_thia.csv")

# Men
estonia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_thia")

estonia_thia <- subset(estonia_thia, select = c(age, HI))
estonia_thia <- estonia_thia[order(estonia_thia$age),]

write.csv(estonia_thia, "all_intakes/estonia_m_thia.csv")

##################################################################

# 18. RUN SPADE FOR RIBOFLAVIN

estonia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_ribo")

estonia_ribo <- subset(estonia_ribo, select = c(age, HI))
estonia_ribo <- estonia_ribo[order(estonia_ribo$age),]

write.csv(estonia_ribo, "all_intakes/estonia_w_ribo.csv")

# Men
estonia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_ribo")

estonia_ribo <- subset(estonia_ribo, select = c(age, HI))
estonia_ribo <- estonia_ribo[order(estonia_ribo$age),]

write.csv(estonia_ribo, "all_intakes/estonia_m_ribo.csv")

##################################################################

# 19. RUN SPADE FOR NIACIN

estonia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_niac")

estonia_niac <- subset(estonia_niac, select = c(age, HI))
estonia_niac <- estonia_niac[order(estonia_niac$age),]

write.csv(estonia_niac, "all_intakes/estonia_w_niac.csv")

# Men
estonia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_niac")

estonia_niac <- subset(estonia_niac, select = c(age, HI))
estonia_niac <- estonia_niac[order(estonia_niac$age),]

write.csv(estonia_niac, "all_intakes/estonia_m_niac.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN B6


estonia_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                          data=estonia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=11, max.age=74,
                          sex.lab="women",
                         
                          output.name = "estonia_wom_vitb6")

estonia_vitb6 <- subset(estonia_vitb6, select = c(age, HI))
estonia_vitb6 <- estonia_vitb6[order(estonia_vitb6$age),]

write.csv(estonia_vitb6, "all_intakes/estonia_w_vitb6.csv")

# Men
estonia_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                          data=estonia_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=11, max.age=75,
                          sex.lab="men",
                         
                          output.name = "estonia_men_vitb6")

estonia_vitb6 <- subset(estonia_vitb6, select = c(age, HI))
estonia_vitb6 <- estonia_vitb6[order(estonia_vitb6$age),]

write.csv(estonia_vitb6, "all_intakes/estonia_m_vitb6.csv")

##################################################################

# 21. RUN SPADE FOR FOLATE

estonia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_fola")

estonia_fola <- subset(estonia_fola, select = c(age, HI))
estonia_fola <- estonia_fola[order(estonia_fola$age),]

write.csv(estonia_fola, "all_intakes/estonia_w_fola.csv")

# Men
estonia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_fola")

estonia_fola <- subset(estonia_fola, select = c(age, HI))
estonia_fola <- estonia_fola[order(estonia_fola$age),]

write.csv(estonia_fola, "all_intakes/estonia_m_fola.csv")

##################################################################

# 22. RUN SPADE FOR VITAMIN C

estonia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_vitc")

estonia_vitc <- subset(estonia_vitc, select = c(age, HI))
estonia_vitc <- estonia_vitc[order(estonia_vitc$age),]

write.csv(estonia_vitc, "all_intakes/estonia_w_vitc.csv")

# Men
estonia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_vitc")

estonia_vitc <- subset(estonia_vitc, select = c(age, HI))
estonia_vitc <- estonia_vitc[order(estonia_vitc$age),]

write.csv(estonia_vitc, "all_intakes/estonia_m_vitc.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN D

estonia_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_vitd")

estonia_vitd <- subset(estonia_vitd, select = c(age, HI))
estonia_vitd <- estonia_vitd[order(estonia_vitd$age),]

write.csv(estonia_vitd, "all_intakes/estonia_w_vitd.csv")

# Men
estonia_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_vitd")

estonia_vitd <- subset(estonia_vitd, select = c(age, HI))
estonia_vitd <- estonia_vitd[order(estonia_vitd$age),]

write.csv(estonia_vitd, "all_intakes/estonia_m_vitd.csv")

##################################################################

# 24. RUN SPADE FOR PHOSPHORUS

estonia_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_phos")

estonia_phos <- subset(estonia_phos, select = c(age, HI))
estonia_phos <- estonia_phos[order(estonia_phos$age),]

write.csv(estonia_phos, "all_intakes/estonia_w_phos.csv")

# Men
estonia_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_phos")

estonia_phos <- subset(estonia_phos, select = c(age, HI))
estonia_phos <- estonia_phos[order(estonia_phos$age),]

write.csv(estonia_phos, "all_intakes/estonia_m_phos.csv")

##################################################################

# 25. RUN SPADE FOR MG

estonia_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                       data=estonia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=74,
                       sex.lab="women",
                      
                       output.name = "estonia_wom_mg")

estonia_mg <- subset(estonia_mg, select = c(age, HI))
estonia_mg <- estonia_mg[order(estonia_mg$age),]

write.csv(estonia_mg, "all_intakes/estonia_w_mg.csv")

# Men
estonia_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                       data=estonia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=75,
                       sex.lab="men",
                      
                       output.name = "estonia_men_mg")

estonia_mg <- subset(estonia_mg, select = c(age, HI))
estonia_mg <- estonia_mg[order(estonia_mg$age),]

write.csv(estonia_mg, "all_intakes/estonia_m_mg.csv")


##################################################################

# 26. RUN SPADE FOR SODIUM

estonia_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                       data=estonia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=74,
                       sex.lab="women",
                      
                       output.name = "estonia_wom_na")

estonia_na <- subset(estonia_na, select = c(age, HI))
estonia_na <- estonia_na[order(estonia_na$age),]

write.csv(estonia_na, "all_intakes/estonia_w_na.csv")

# Men
estonia_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                       data=estonia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=75,
                       sex.lab="men",
                      
                       output.name = "estonia_men_na")

estonia_na <- subset(estonia_na, select = c(age, HI))
estonia_na <- estonia_na[order(estonia_na$age),]

write.csv(estonia_na, "all_intakes/estonia_m_na.csv")

##################################################################

# 27. RUN SPADE FOR POTASSIUM

estonia_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                         data=estonia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=74,
                         sex.lab="women",
                        
                         output.name = "estonia_wom_pota")

estonia_pota <- subset(estonia_pota, select = c(age, HI))
estonia_pota <- estonia_pota[order(estonia_pota$age),]

write.csv(estonia_pota, "all_intakes/estonia_w_pota.csv")

# Men
estonia_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                         data=estonia_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=11, max.age=75,
                         sex.lab="men",
                        
                         output.name = "estonia_men_pota")

estonia_pota <- subset(estonia_pota, select = c(age, HI))
estonia_pota <- estonia_pota[order(estonia_pota$age),]

write.csv(estonia_pota, "all_intakes/estonia_m_pota.csv")

##################################################################

# 28. RUN SPADE FOR IODINE

estonia_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                        data=estonia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=74,
                        sex.lab="women",
                        
                        output.name = "estonia_wom_iod")

estonia_iod <- subset(estonia_iod, select = c(age, HI))
estonia_iod <- estonia_iod[order(estonia_iod$age),]

write.csv(estonia_iod, "all_intakes/estonia_w_iod.csv")

# Men
estonia_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                        data=estonia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=75,
                        sex.lab="men",
                        
                        output.name = "estonia_men_iod")

estonia_iod <- subset(estonia_iod, select = c(age, HI))
estonia_iod <- estonia_iod[order(estonia_iod$age),]

write.csv(estonia_iod, "all_intakes/estonia_m_iod.csv")

##################################################################

# 29. RUN SPADE FOR SELENIUM

estonia_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                        data=estonia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=74,
                        sex.lab="women",
                        
                        output.name = "estonia_wom_se")

estonia_se <- subset(estonia_se, select = c(age, HI))
estonia_se <- estonia_se[order(estonia_se$age),]

write.csv(estonia_se, "all_intakes/estonia_w_se.csv")

# Men
estonia_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                        data=estonia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=11, max.age=75,
                        sex.lab="men",
                        
                        output.name = "estonia_men_se")

estonia_se <- subset(estonia_se, select = c(age, HI))
estonia_se <- estonia_se[order(estonia_se$age),]

write.csv(estonia_se, "all_intakes/estonia_m_se.csv")

##################################################################

# 30. RUN SPADE FOR COPPER

estonia_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                       data=estonia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=74,
                       sex.lab="women",
                       
                       output.name = "estonia_wom_cu")

estonia_cu <- subset(estonia_cu, select = c(age, HI))
estonia_cu <- estonia_cu[order(estonia_cu$age),]

write.csv(estonia_cu, "all_intakes/estonia_w_cu.csv")

# Men
estonia_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                       data=estonia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=11, max.age=75,
                       sex.lab="men",
                       
                       output.name = "estonia_men_cu")

estonia_cu <- subset(estonia_cu, select = c(age, HI))
estonia_cu <- estonia_cu[order(estonia_cu$age),]

write.csv(estonia_cu, "all_intakes/estonia_m_cu.csv")


##################################################################

# 31. RUN SPADE FOR BETA CAROTENE

estonia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                      data=estonia_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=11, max.age=74,
                      sex.lab="women",
                      
                      output.name = "estonia_wom_betacarot")

estonia_betacarot <- subset(estonia_betacarot, select = c(age, HI))
estonia_betacarot <- estonia_betacarot[order(estonia_betacarot$age),]

write.csv(estonia_betacarot, "all_intakes/estonia_w_betacarot.csv")

# Men
estonia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                      data=estonia_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=11, max.age=75,
                      sex.lab="men",
                      
                      output.name = "estonia_men_betacarot")

estonia_betacarot <- subset(estonia_betacarot, select = c(age, HI))
estonia_betacarot <- estonia_betacarot[order(estonia_betacarot$age),]

write.csv(estonia_betacarot, "all_intakes/estonia_m_betacarot.csv")

