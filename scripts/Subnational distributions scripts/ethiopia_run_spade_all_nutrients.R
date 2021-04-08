# Running SPADE: ethiopia data
# Created by Simone Passarelli on March 25 2021

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "ethiopia"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "ethiopia"))
###########################################################
# Remove missing obs
summary(ethiopia_spade)
names(ethiopia_spade)

# number of intakes per person:
table(ethiopia_spade$mday)

# Make separate datasets for men and women
ethiopia_wom <- subset(ethiopia_spade, sex==2)
# There are no men in this sample
# Let's have a look at the highest b12 intakes
range(ethiopia_wom$age)

###########################################################

# 1. RUN SPADE FOR IRON
# Women

ethiopia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_iron")

ethiopia_iron <- subset(ethiopia_iron, select = c(age, HI))
ethiopia_iron <- ethiopia_iron[order(ethiopia_iron$age),]

write.csv(ethiopia_iron, "all_intakes/ethiopia_w_iron.csv")

##################################################################

# 2. RUN SPADE FOR ZINC
ethiopia_zinc <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                          data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=45,
                          sex.lab="women",
                          output.name = "ethiopia_wom_zinc")

ethiopia_zinc <- subset(ethiopia_zinc, select = c(age, HI))
ethiopia_zinc <- ethiopia_zinc[order(ethiopia_zinc$age),]

write.csv(ethiopia_zinc, "all_intakes/ethiopia_w_zinc.csv")

##################################################################

# 3. RUN SPADE FOR VIT A

ethiopia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_vita")

ethiopia_vita <- subset(ethiopia_vita, select = c(age, HI))
ethiopia_vita <- ethiopia_vita[order(ethiopia_vita$age),]

write.csv(ethiopia_vita, "all_intakes/ethiopia_w_vita.csv")

##################################################################

# 4. RUN SPADE FOR CALCIUM

ethiopia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_calc")

ethiopia_calc <- subset(ethiopia_calc, select = c(age, HI))
ethiopia_calc <- ethiopia_calc[order(ethiopia_calc$age),]

write.csv(ethiopia_calc, "all_intakes/ethiopia_w_calc.csv")

##################################################################
# COME BACK TO THIS WHEN FILLED IN
# # 6. RUN SPADE FOR OMEGA 3 
# 
# ethiopia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
#                            data=ethiopia_wom, seed=123,  backtrans.nr = 3,
#                            dgts.distr = 2, min.age=18, max.age=45,
#                            sex.lab="women",
#                            
#                            output.name = "ethiopia_wom_omega_3")
# 
# ethiopia_omega_3 <- subset(ethiopia_omega_3, select = c(age, HI))
# ethiopia_omega_3 <- ethiopia_omega_3[order(ethiopia_omega_3$age),]
# 
# write.csv(ethiopia_omega_3, "all_intakes/ethiopia_w_omega_3.csv")

##################################################################

# 8. RUN SPADE FOR ENERGY

ethiopia_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                          data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=18, max.age=45,
                          sex.lab="women",
                          
                          output.name = "ethiopia_wom_energy")

ethiopia_energy <- subset(ethiopia_energy, select = c(age, HI))
ethiopia_energy <- ethiopia_energy[order(ethiopia_energy$age),]

write.csv(ethiopia_energy, "all_intakes/ethiopia_w_energy.csv")

##################################################################

# 9. RUN SPADE FOR PROTEIN

ethiopia_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                           data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=18, max.age=45,
                           sex.lab="women",
                           
                           output.name = "ethiopia_wom_protein")

ethiopia_protein <- subset(ethiopia_protein, select = c(age, HI))
ethiopia_protein <- ethiopia_protein[order(ethiopia_protein$age),]

write.csv(ethiopia_protein, "all_intakes/ethiopia_w_protein.csv")

##################################################################

# 10. RUN SPADE FOR CARB

ethiopia_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_carb")

ethiopia_carb <- subset(ethiopia_carb, select = c(age, HI))
ethiopia_carb <- ethiopia_carb[order(ethiopia_carb$age),]

write.csv(ethiopia_carb, "all_intakes/ethiopia_w_carb.csv")

##################################################################

# 11. RUN SPADE FOR FIBER

ethiopia_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                         data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=18, max.age=45,
                         sex.lab="women",
                         
                         output.name = "ethiopia_wom_fiber")

ethiopia_fiber <- subset(ethiopia_fiber, select = c(age, HI))
ethiopia_fiber <- ethiopia_fiber[order(ethiopia_fiber$age),]

write.csv(ethiopia_fiber, "all_intakes/ethiopia_w_fiber.csv")

##################################################################

# 12. RUN SPADE FOR FAT

ethiopia_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                       data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=18, max.age=45,
                       sex.lab="women",
                       
                       output.name = "ethiopia_wom_fat")

ethiopia_fat <- subset(ethiopia_fat, select = c(age, HI))
ethiopia_fat <- ethiopia_fat[order(ethiopia_fat$age),]

write.csv(ethiopia_fat, "all_intakes/ethiopia_w_fat.csv")

##################################################################

# 13. RUN SPADE FOR THIAMIN

ethiopia_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_thia")

ethiopia_thia <- subset(ethiopia_thia, select = c(age, HI))
ethiopia_thia <- ethiopia_thia[order(ethiopia_thia$age),]

write.csv(ethiopia_thia, "all_intakes/ethiopia_w_thia.csv")

##################################################################

# 14. RUN SPADE FOR RIBOFLAVIN

ethiopia_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_ribo")

ethiopia_ribo <- subset(ethiopia_ribo, select = c(age, HI))
ethiopia_ribo <- ethiopia_ribo[order(ethiopia_ribo$age),]

write.csv(ethiopia_ribo, "all_intakes/ethiopia_w_ribo.csv")

##################################################################

# 15. RUN SPADE FOR NIACIN

ethiopia_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_niac")

ethiopia_niac <- subset(ethiopia_niac, select = c(age, HI))
ethiopia_niac <- ethiopia_niac[order(ethiopia_niac$age),]

write.csv(ethiopia_niac, "all_intakes/ethiopia_w_niac.csv")

##################################################################

# 16. RUN SPADE FOR FOLATE

ethiopia_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_fola")

ethiopia_fola <- subset(ethiopia_fola, select = c(age, HI))
ethiopia_fola <- ethiopia_fola[order(ethiopia_fola$age),]

write.csv(ethiopia_fola, "all_intakes/ethiopia_w_fola.csv")

##################################################################

# 17. RUN SPADE FOR VITAMIN C

ethiopia_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_vitc")

ethiopia_vitc <- subset(ethiopia_vitc, select = c(age, HI))
ethiopia_vitc <- ethiopia_vitc[order(ethiopia_vitc$age),]

write.csv(ethiopia_vitc, "all_intakes/ethiopia_w_vitc.csv")

##################################################################

# 18. RUN SPADE FOR PHOSPHORUS

ethiopia_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                        data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=18, max.age=45,
                        sex.lab="women",
                        
                        output.name = "ethiopia_wom_phos")

ethiopia_phos <- subset(ethiopia_phos, select = c(age, HI))
ethiopia_phos <- ethiopia_phos[order(ethiopia_phos$age),]

write.csv(ethiopia_phos, "all_intakes/ethiopia_w_phos.csv")

##################################################################

# 19. RUN SPADE FOR BETA CAROTENE

ethiopia_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                             data=ethiopia_wom, seed=123,  backtrans.nr = 3,
                             dgts.distr = 2, min.age=18, max.age=45,
                             sex.lab="women",
                             
                             output.name = "ethiopia_wom_betacarot")

ethiopia_betacarot <- subset(ethiopia_betacarot, select = c(age, HI))
ethiopia_betacarot <- ethiopia_betacarot[order(ethiopia_betacarot$age),]

write.csv(ethiopia_betacarot, "all_intakes/ethiopia_w_betacarot.csv")
