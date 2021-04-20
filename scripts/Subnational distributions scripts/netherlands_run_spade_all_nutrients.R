# Running SPADE: netherlands data
# File created on 3/29/21 by Simone Passarelli
# File updated to add weights on 4/20/21

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed","Subnational distributions", "netherlands"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "netherlands"))
###########################################################
# Remove missing obs
summary(netherlands_spade)
names(netherlands_spade)

# number of intakes per person:
table(netherlands_spade$mday)

# Make separate datasets for men and women
netherlands_wom <- subset(netherlands_spade, sex==2)
netherlands_men <- subset(netherlands_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(netherlands_wom$age)
range(netherlands_men$age)

# Women
netherlands_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=netherlands_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=94,
                         sex.lab="women", weights.name ="weights", 
                         output.name = "netherlands_wom_vitb12")

netherlands_vitb12 <- subset(netherlands_vitb12, select = c(age, HI))
netherlands_vitb12 <- netherlands_vitb12[order(netherlands_vitb12$age),]

write.csv(netherlands_vitb12, "all_intakes/netherlands_w_vitb12.csv")

table(netherlands_men$age)

# Men
netherlands_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=netherlands_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=90,
                         sex.lab="men", weights.name ="weights",
                         output.name = "netherlands_men_vitb12")

netherlands_vitb12 <- subset(netherlands_vitb12, select = c(age, HI))
netherlands_vitb12 <- netherlands_vitb12[order(netherlands_vitb12$age),]

write.csv(netherlands_vitb12, "all_intakes/netherlands_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
netherlands_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_iron")

netherlands_iron <- subset(netherlands_iron, select = c(age, HI))
netherlands_iron <- netherlands_iron[order(netherlands_iron$age),]

write.csv(netherlands_iron, "all_intakes/netherlands_w_iron.csv")

# Men
netherlands_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       output.name = "netherlands_men_iron")

netherlands_iron <- subset(netherlands_iron, select = c(age, HI))
netherlands_iron <- netherlands_iron[order(netherlands_iron$age),]

write.csv(netherlands_iron, "all_intakes/netherlands_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
netherlands_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=netherlands_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=94,
                         sex.lab="women", weights.name ="weights",
                         output.name = "netherlands_wom_zinc")

netherlands_zinc_w <- subset(netherlands_zinc_w, select = c(age, HI))
netherlands_zinc_w <- netherlands_zinc_w[order(netherlands_zinc_w$age),]

write.csv(netherlands_zinc_w, "all_intakes/netherlands_w_zinc.csv")

# Men
netherlands_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=netherlands_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=90,
                         sex.lab="men", weights.name ="weights",
                         output.name = "netherlands_men_zinc")

netherlands_zinc_m <- subset(netherlands_zinc_m, select = c(age, HI))
netherlands_zinc_m <- netherlands_zinc_m[order(netherlands_zinc_m$age),]

write.csv(netherlands_zinc_m, "all_intakes/netherlands_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

netherlands_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_vita")

netherlands_vita <- subset(netherlands_vita, select = c(age, HI))
netherlands_vita <- netherlands_vita[order(netherlands_vita$age),]

write.csv(netherlands_vita, "all_intakes/netherlands_w_vita.csv")

# Men
netherlands_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       output.name = "netherlands_men_vita")

netherlands_vita <- subset(netherlands_vita, select = c(age, HI))
netherlands_vita <- netherlands_vita[order(netherlands_vita$age),]

write.csv(netherlands_vita, "all_intakes/netherlands_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

netherlands_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_calc")

netherlands_calc <- subset(netherlands_calc, select = c(age, HI))
netherlands_calc <- netherlands_calc[order(netherlands_calc$age),]

write.csv(netherlands_calc, "all_intakes/netherlands_w_calc.csv")

# Men
netherlands_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       output.name = "netherlands_men_calc")

netherlands_calc <- subset(netherlands_calc, select = c(age, HI))
netherlands_calc <- netherlands_calc[order(netherlands_calc$age),]

write.csv(netherlands_calc, "all_intakes/netherlands_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

netherlands_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=netherlands_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=94,
                          sex.lab="women", weights.name ="weights",
                          output.name = "netherlands_wom_omega_3")

netherlands_omega_3 <- subset(netherlands_omega_3, select = c(age, HI))
netherlands_omega_3 <- netherlands_omega_3[order(netherlands_omega_3$age),]

write.csv(netherlands_omega_3, "all_intakes/netherlands_w_omega_3.csv")

# Men
netherlands_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=netherlands_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=90,
                          sex.lab="men", weights.name ="weights",
                          output.name = "netherlands_men_omega_3")

netherlands_omega_3 <- subset(netherlands_omega_3, select = c(age, HI))
netherlands_omega_3 <- netherlands_omega_3[order(netherlands_omega_3$age),]

write.csv(netherlands_omega_3, "all_intakes/netherlands_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

netherlands_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=netherlands_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=94,
                         sex.lab="women", weights.name ="weights",
                         output.name = "netherlands_wom_energy")

netherlands_energy <- subset(netherlands_energy, select = c(age, HI))
netherlands_energy <- netherlands_energy[order(netherlands_energy$age),]

write.csv(netherlands_energy, "all_intakes/netherlands_w_energy.csv")

# Men
netherlands_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=netherlands_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=90,
                         sex.lab="men", weights.name ="weights",
                         output.name = "netherlands_men_energy")

netherlands_energy <- subset(netherlands_energy, select = c(age, HI))
netherlands_energy <- netherlands_energy[order(netherlands_energy$age),]

write.csv(netherlands_energy, "all_intakes/netherlands_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

netherlands_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=netherlands_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=94,
                          sex.lab="women", weights.name ="weights",
                          output.name = "netherlands_wom_protein")

netherlands_protein <- subset(netherlands_protein, select = c(age, HI))
netherlands_protein <- netherlands_protein[order(netherlands_protein$age),]

write.csv(netherlands_protein, "all_intakes/netherlands_w_protein.csv")

# Men
netherlands_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=netherlands_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=90,
                          sex.lab="men", weights.name ="weights",
                          output.name = "netherlands_men_protein")

netherlands_protein <- subset(netherlands_protein, select = c(age, HI))
netherlands_protein <- netherlands_protein[order(netherlands_protein$age),]

write.csv(netherlands_protein, "all_intakes/netherlands_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

netherlands_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_carb")

netherlands_carb <- subset(netherlands_carb, select = c(age, HI))
netherlands_carb <- netherlands_carb[order(netherlands_carb$age),]

write.csv(netherlands_carb, "all_intakes/netherlands_w_carb.csv")

# Men
netherlands_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       output.name = "netherlands_men_carb")

netherlands_carb <- subset(netherlands_carb, select = c(age, HI))
netherlands_carb <- netherlands_carb[order(netherlands_carb$age),]

write.csv(netherlands_carb, "all_intakes/netherlands_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR FIBER

netherlands_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=netherlands_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=94,
                        sex.lab="women", weights.name ="weights",
                        output.name = "netherlands_wom_fiber")

netherlands_fiber <- subset(netherlands_fiber, select = c(age, HI))
netherlands_fiber <- netherlands_fiber[order(netherlands_fiber$age),]

write.csv(netherlands_fiber, "all_intakes/netherlands_w_fiber.csv")

# Men
netherlands_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=netherlands_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=90,
                        sex.lab="men", weights.name ="weights",
                        output.name = "netherlands_men_fiber")

netherlands_fiber <- subset(netherlands_fiber, select = c(age, HI))
netherlands_fiber <- netherlands_fiber[order(netherlands_fiber$age),]

write.csv(netherlands_fiber, "all_intakes/netherlands_m_fiber.csv")

##################################################################

# 11. RUN SPADE FOR FAT

netherlands_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=netherlands_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=94,
                      sex.lab="women", weights.name ="weights",
                      output.name = "netherlands_wom_fat")

netherlands_fat <- subset(netherlands_fat, select = c(age, HI))
netherlands_fat <- netherlands_fat[order(netherlands_fat$age),]

write.csv(netherlands_fat, "all_intakes/netherlands_w_fat.csv")

# Men
netherlands_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=netherlands_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=90,
                      sex.lab="men", weights.name ="weights",
                      output.name = "netherlands_men_fat")

netherlands_fat <- subset(netherlands_fat, select = c(age, HI))
netherlands_fat <- netherlands_fat[order(netherlands_fat$age),]

write.csv(netherlands_fat, "all_intakes/netherlands_m_fat.csv")


##################################################################

# 12. RUN SPADE FOR MUFA

netherlands_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_mufa")

netherlands_mufa <- subset(netherlands_mufa, select = c(age, HI))
netherlands_mufa <- netherlands_mufa[order(netherlands_mufa$age),]

write.csv(netherlands_mufa, "all_intakes/netherlands_w_mufa.csv")

# Men
netherlands_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_mufa")

netherlands_mufa <- subset(netherlands_mufa, select = c(age, HI))
netherlands_mufa <- netherlands_mufa[order(netherlands_mufa$age),]

write.csv(netherlands_mufa, "all_intakes/netherlands_m_mufa.csv")

##################################################################

# 13. RUN SPADE FOR PUFA

netherlands_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_pufa")

netherlands_pufa <- subset(netherlands_pufa, select = c(age, HI))
netherlands_pufa <- netherlands_pufa[order(netherlands_pufa$age),]

write.csv(netherlands_pufa, "all_intakes/netherlands_w_pufa.csv")

# Men
netherlands_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_pufa")

netherlands_pufa <- subset(netherlands_pufa, select = c(age, HI))
netherlands_pufa <- netherlands_pufa[order(netherlands_pufa$age),]

write.csv(netherlands_pufa, "all_intakes/netherlands_m_pufa.csv")

##################################################################

# 14. RUN SPADE FOR THIAMIN

netherlands_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_thia")

netherlands_thia <- subset(netherlands_thia, select = c(age, HI))
netherlands_thia <- netherlands_thia[order(netherlands_thia$age),]

write.csv(netherlands_thia, "all_intakes/netherlands_w_thia.csv")

# Men
netherlands_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_thia")

netherlands_thia <- subset(netherlands_thia, select = c(age, HI))
netherlands_thia <- netherlands_thia[order(netherlands_thia$age),]

write.csv(netherlands_thia, "all_intakes/netherlands_m_thia.csv")

##################################################################

# 15. RUN SPADE FOR RIBOFLAVIN

netherlands_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_ribo")

netherlands_ribo <- subset(netherlands_ribo, select = c(age, HI))
netherlands_ribo <- netherlands_ribo[order(netherlands_ribo$age),]

write.csv(netherlands_ribo, "all_intakes/netherlands_w_ribo.csv")

# Men
netherlands_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_ribo")

netherlands_ribo <- subset(netherlands_ribo, select = c(age, HI))
netherlands_ribo <- netherlands_ribo[order(netherlands_ribo$age),]

write.csv(netherlands_ribo, "all_intakes/netherlands_m_ribo.csv")

##################################################################
# 16. RUN SPADE FOR NIACIN
niac_notna_w <- netherlands_wom[!is.na(netherlands_wom$niac), ]
niac_notna_m <- netherlands_men[!is.na(netherlands_men$niac), ]


netherlands_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=niac_notna_w, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_niac")

netherlands_niac <- subset(netherlands_niac, select = c(age, HI))
netherlands_niac <- netherlands_niac[order(netherlands_niac$age),]

write.csv(netherlands_niac, "all_intakes/netherlands_w_niac.csv")

# Men
netherlands_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=niac_notna_m, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_niac")

netherlands_niac <- subset(netherlands_niac, select = c(age, HI))
netherlands_niac <- netherlands_niac[order(netherlands_niac$age),]

write.csv(netherlands_niac, "all_intakes/netherlands_m_niac.csv")

##################################################################

# 17. RUN SPADE FOR VITAMIN B6

netherlands_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=netherlands_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=94,
                        sex.lab="women", weights.name ="weights",
                        
                        output.name = "netherlands_wom_vitb6")

netherlands_vitb6 <- subset(netherlands_vitb6, select = c(age, HI))
netherlands_vitb6 <- netherlands_vitb6[order(netherlands_vitb6$age),]

write.csv(netherlands_vitb6, "all_intakes/netherlands_w_vitb6.csv")

# Men
netherlands_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=netherlands_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=90,
                        sex.lab="men", weights.name ="weights",
                        
                        output.name = "netherlands_men_vitb6")

netherlands_vitb6 <- subset(netherlands_vitb6, select = c(age, HI))
netherlands_vitb6 <- netherlands_vitb6[order(netherlands_vitb6$age),]

write.csv(netherlands_vitb6, "all_intakes/netherlands_m_vitb6.csv")

##################################################################

# 18. RUN SPADE FOR FOLATE

fola_notna_w <- netherlands_wom[!is.na(netherlands_wom$fola), ]
fola_notna_m <- netherlands_men[!is.na(netherlands_men$fola), ]

netherlands_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=fola_notna_w, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_fola")

netherlands_fola <- subset(netherlands_fola, select = c(age, HI))
netherlands_fola <- netherlands_fola[order(netherlands_fola$age),]

write.csv(netherlands_fola, "all_intakes/netherlands_w_fola.csv")

# Men
netherlands_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=fola_notna_m, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_fola")

netherlands_fola <- subset(netherlands_fola, select = c(age, HI))
netherlands_fola <- netherlands_fola[order(netherlands_fola$age),]

write.csv(netherlands_fola, "all_intakes/netherlands_m_fola.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN D

netherlands_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_vitd")

netherlands_vitd <- subset(netherlands_vitd, select = c(age, HI))
netherlands_vitd <- netherlands_vitd[order(netherlands_vitd$age),]

write.csv(netherlands_vitd, "all_intakes/netherlands_w_vitd.csv")

# Men
netherlands_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_vitd")

netherlands_vitd <- subset(netherlands_vitd, select = c(age, HI))
netherlands_vitd <- netherlands_vitd[order(netherlands_vitd$age),]

write.csv(netherlands_vitd, "all_intakes/netherlands_m_vitd.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN C

netherlands_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_vitc")

netherlands_vitc <- subset(netherlands_vitc, select = c(age, HI))
netherlands_vitc <- netherlands_vitc[order(netherlands_vitc$age),]

write.csv(netherlands_vitc, "all_intakes/netherlands_w_vitc.csv")

# Men
netherlands_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_vitc")

netherlands_vitc <- subset(netherlands_vitc, select = c(age, HI))
netherlands_vitc <- netherlands_vitc[order(netherlands_vitc$age),]

write.csv(netherlands_vitc, "all_intakes/netherlands_m_vitc.csv")

##################################################################

# 21. RUN SPADE FOR PHOSPHORUS

netherlands_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_phos")

netherlands_phos <- subset(netherlands_phos, select = c(age, HI))
netherlands_phos <- netherlands_phos[order(netherlands_phos$age),]

write.csv(netherlands_phos, "all_intakes/netherlands_w_phos.csv")

# Men
netherlands_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_phos")

netherlands_phos <- subset(netherlands_phos, select = c(age, HI))
netherlands_phos <- netherlands_phos[order(netherlands_phos$age),]

write.csv(netherlands_phos, "all_intakes/netherlands_m_phos.csv")

##################################################################

# 22. RUN SPADE FOR MG

netherlands_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=netherlands_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=94,
                     sex.lab="women", weights.name ="weights",
                     
                     output.name = "netherlands_wom_mg")

netherlands_mg <- subset(netherlands_mg, select = c(age, HI))
netherlands_mg <- netherlands_mg[order(netherlands_mg$age),]

write.csv(netherlands_mg, "all_intakes/netherlands_w_mg.csv")

# Men
netherlands_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=netherlands_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=90,
                     sex.lab="men", weights.name ="weights",
                     
                     output.name = "netherlands_men_mg")

netherlands_mg <- subset(netherlands_mg, select = c(age, HI))
netherlands_mg <- netherlands_mg[order(netherlands_mg$age),]

write.csv(netherlands_mg, "all_intakes/netherlands_m_mg.csv")

##################################################################

# 23. RUN SPADE FOR POTASSIUM

netherlands_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "netherlands_wom_pota")

netherlands_pota <- subset(netherlands_pota, select = c(age, HI))
netherlands_pota <- netherlands_pota[order(netherlands_pota$age),]

write.csv(netherlands_pota, "all_intakes/netherlands_w_pota.csv")

# Men
netherlands_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "netherlands_men_pota")

netherlands_pota <- subset(netherlands_pota, select = c(age, HI))
netherlands_pota <- netherlands_pota[order(netherlands_pota$age),]

write.csv(netherlands_pota, "all_intakes/netherlands_m_pota.csv")

##################################################################

# 24. RUN SPADE FOR VITAMIN E

netherlands_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=netherlands_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=94,
                       sex.lab="women", weights.name ="weights",
                       output.name = "netherlands_wom_vite")

netherlands_vite <- subset(netherlands_vite, select = c(age, HI))
netherlands_vite <- netherlands_vite[order(netherlands_vite$age),]

write.csv(netherlands_vite, "all_intakes/netherlands_w_vite.csv")

# Men
netherlands_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=netherlands_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=90,
                       sex.lab="men", weights.name ="weights",
                       output.name = "netherlands_men_vite")

netherlands_vite <- subset(netherlands_vite, select = c(age, HI))
netherlands_vite <- netherlands_vite[order(netherlands_vite$age),]

write.csv(netherlands_vite, "all_intakes/netherlands_m_vite.csv")


##################################################################

# 25. RUN SPADE FOR ala

netherlands_ala <- f.spade(frml.ia=ala~fp(age), frml.if=ala ~cs(age), 
                          data=netherlands_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=94,
                          sex.lab="women", weights.name ="weights",
                          
                          output.name = "netherlands_wom_ala")

netherlands_ala <- subset(netherlands_ala, select = c(age, HI))
netherlands_ala <- netherlands_ala[order(netherlands_ala$age),]

write.csv(netherlands_ala, "all_intakes/netherlands_w_ala.csv")

# Men
netherlands_ala <- f.spade(frml.ia=ala~fp(age), frml.if=ala ~cs(age), 
                          data=netherlands_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=90,
                          sex.lab="men", weights.name ="weights",
                          
                          output.name = "netherlands_men_ala")

netherlands_ala <- subset(netherlands_ala, select = c(age, HI))
netherlands_ala <- netherlands_ala[order(netherlands_ala$age),]

write.csv(netherlands_ala, "all_intakes/netherlands_m_ala.csv")


##################################################################

# 26. RUN SPADE FOR SELENIUM

netherlands_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=netherlands_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=94,
                     sex.lab="women", weights.name ="weights",
                     
                     output.name = "netherlands_wom_se")

netherlands_se <- subset(netherlands_se, select = c(age, HI))
netherlands_se <- netherlands_se[order(netherlands_se$age),]

write.csv(netherlands_se, "all_intakes/netherlands_w_se.csv")

# Men
netherlands_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=netherlands_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=90,
                     sex.lab=" men", weights.name ="weights",
                     
                     output.name = "netherlands_men_se")

netherlands_se <- subset(netherlands_se, select = c(age, HI))
netherlands_se <- netherlands_se[order(netherlands_se$age),]

write.csv(netherlands_se, "all_intakes/netherlands_m_se.csv")


##################################################################

# 27. RUN SPADE FOR BETA CAROTENE

betacarot_notna_w <- netherlands_wom[!is.na(netherlands_wom$betacarot), ]
betacarot_notna_m <- netherlands_men[!is.na(netherlands_men$betacarot), ]


netherlands_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=betacarot_notna_w, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=1, max.age=94,
                            sex.lab="women", weights.name ="weights",
                            output.name = "netherlands_wom_betacarot")

netherlands_betacarot <- subset(netherlands_betacarot, select = c(age, HI))
netherlands_betacarot <- netherlands_betacarot[order(netherlands_betacarot$age),]

write.csv(netherlands_betacarot, "all_intakes/netherlands_w_betacarot.csv")

# Men
netherlands_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=betacarot_notna_m, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=1, max.age=90,
                            sex.lab="men", weights.name ="weights",
                            output.name = "netherlands_men_betacarot")

netherlands_betacarot <- subset(netherlands_betacarot, select = c(age, HI))
netherlands_betacarot <- netherlands_betacarot[order(netherlands_betacarot$age),]

write.csv(netherlands_betacarot, "all_intakes/netherlands_m_betacarot.csv")


##################################################################

# 28. RUN SPADE FOR ALPHA-LINOLENIC ACID

ala_notna_w <- netherlands_wom[!is.na(netherlands_wom$ala), ]
ala_notna_m <- netherlands_men[!is.na(netherlands_men$ala), ]


netherlands_ala <- f.spade(frml.ia=ala~fp(age), frml.if="no.if", 
                                 data=ala_notna_w, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=1, max.age=94,
                                 sex.lab="women", weights.name ="weights",
                                 output.name = "netherlands_wom_ala")

netherlands_ala <- subset(netherlands_ala, select = c(age, HI))
netherlands_ala <- netherlands_ala[order(netherlands_ala$age),]

write.csv(netherlands_ala, "all_intakes/netherlands_w_ala.csv")

# Men
netherlands_ala <- f.spade(frml.ia=ala~fp(age), frml.if="no.if", 
                                 data=ala_notna_m, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=1, max.age=90,
                                 sex.lab="men", weights.name ="weights",
                                 output.name = "netherlands_men_ala")

netherlands_ala <- subset(netherlands_ala, select = c(age, HI))
netherlands_ala <- netherlands_ala[order(netherlands_ala$age),]

write.csv(netherlands_ala, "all_intakes/netherlands_m_ala.csv")

##################################################################

# 29. RUN SPADE FOR LINOLEIC ACID

la_notna_w <- netherlands_wom[!is.na(netherlands_wom$la), ]
la_notna_m <- netherlands_men[!is.na(netherlands_men$la), ]


netherlands_la <- f.spade(frml.ia=la~fp(age), frml.if="no.if", 
                           data=la_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_la")

netherlands_la <- subset(netherlands_la, select = c(age, HI))
netherlands_la <- netherlands_la[order(netherlands_la$age),]

write.csv(netherlands_la, "all_intakes/netherlands_w_la.csv")

# Men
netherlands_la <- f.spade(frml.ia=la~fp(age), frml.if="no.if", 
                           data=la_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_la")

netherlands_la <- subset(netherlands_la, select = c(age, HI))
netherlands_la <- netherlands_la[order(netherlands_la$age),]

write.csv(netherlands_la, "all_intakes/netherlands_m_la.csv")


##################################################################

# 30. RUN SPADE FOR IODINE

iod_notna_w <- netherlands_wom[!is.na(netherlands_wom$iod), ]
iod_notna_m <- netherlands_men[!is.na(netherlands_men$iod), ]


netherlands_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                           data=iod_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_iod")

netherlands_iod <- subset(netherlands_iod, select = c(age, HI))
netherlands_iod <- netherlands_iod[order(netherlands_iod$age),]

write.csv(netherlands_iod, "all_intakes/netherlands_w_iod.csv")

# Men
netherlands_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                           data=iod_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_iod")

netherlands_iod <- subset(netherlands_iod, select = c(age, HI))
netherlands_iod <- netherlands_iod[order(netherlands_iod$age),]

write.csv(netherlands_iod, "all_intakes/netherlands_m_iod.csv")

##################################################################

# 31. RUN SPADE FOR VITAMIN K

vitk_notna_w <- netherlands_wom[!is.na(netherlands_wom$vitk), ]
vitk_notna_m <- netherlands_men[!is.na(netherlands_men$vitk), ]


netherlands_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                           data=vitk_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_vitk")

netherlands_vitk <- subset(netherlands_vitk, select = c(age, HI))
netherlands_vitk <- netherlands_vitk[order(netherlands_vitk$age),]

write.csv(netherlands_vitk, "all_intakes/netherlands_w_vitk.csv")

# Men
netherlands_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                           data=vitk_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_vitk")

netherlands_vitk <- subset(netherlands_vitk, select = c(age, HI))
netherlands_vitk <- netherlands_vitk[order(netherlands_vitk$age),]

write.csv(netherlands_vitk, "all_intakes/netherlands_m_vitk.csv")


##################################################################

# 32. RUN SPADE FOR CHOLESTEROL

netherlands_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                            data=netherlands_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=1, max.age=94,
                            sex.lab="women", weights.name ="weights",
                            output.name = "netherlands_wom_cholest")

netherlands_cholest <- subset(netherlands_cholest, select = c(age, HI))
netherlands_cholest <- netherlands_cholest[order(netherlands_cholest$age),]

write.csv(netherlands_cholest, "all_intakes/netherlands_w_cholest.csv")

# Men
netherlands_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                            data=netherlands_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=1, max.age=90,
                            sex.lab="men", weights.name ="weights",
                            output.name = "netherlands_men_cholest")

netherlands_cholest <- subset(netherlands_cholest, select = c(age, HI))
netherlands_cholest <- netherlands_cholest[order(netherlands_cholest$age),]

write.csv(netherlands_cholest, "all_intakes/netherlands_m_cholest.csv")


##################################################################

# 33. RUN SPADE FOR SATURATED FAT

netherlands_sfa <- f.spade(frml.ia=sfa~fp(age), frml.if="no.if", 
                               data=netherlands_wom, seed=123,  backtrans.nr = 3,
                               dgts.distr = 2, min.age=1, max.age=94,
                               sex.lab="women", weights.name ="weights",
                               output.name = "netherlands_wom_sfa")

netherlands_sfa <- subset(netherlands_sfa, select = c(age, HI))
netherlands_sfa <- netherlands_sfa[order(netherlands_sfa$age),]

write.csv(netherlands_sfa, "all_intakes/netherlands_w_sfa.csv")

# Men
netherlands_sfa <- f.spade(frml.ia=sfa~fp(age), frml.if="no.if", 
                               data=netherlands_men, seed=123,  backtrans.nr = 3,
                               dgts.distr = 2, min.age=1, max.age=90,
                               sex.lab="men", weights.name ="weights",
                               output.name = "netherlands_men_sfa")

netherlands_sfa <- subset(netherlands_sfa, select = c(age, HI))
netherlands_sfa <- netherlands_sfa[order(netherlands_sfa$age),]

write.csv(netherlands_sfa, "all_intakes/netherlands_m_sfa.csv")

##################################################################

# 34. RUN SPADE FOR TRANS FAT

netherlands_tfa <- f.spade(frml.ia=tfa~fp(age), frml.if="no.if", 
                           data=netherlands_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_tfa")

netherlands_tfa <- subset(netherlands_tfa, select = c(age, HI))
netherlands_tfa <- netherlands_tfa[order(netherlands_tfa$age),]

write.csv(netherlands_tfa, "all_intakes/netherlands_w_tfa.csv")

# Men
netherlands_tfa <- f.spade(frml.ia=tfa~fp(age), frml.if="no.if", 
                           data=netherlands_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_tfa")

netherlands_tfa <- subset(netherlands_tfa, select = c(age, HI))
netherlands_tfa <- netherlands_tfa[order(netherlands_tfa$age),]

write.csv(netherlands_tfa, "all_intakes/netherlands_m_tfa.csv")

##################################################################

# 35. RUN SPADE FOR TRANS FAT

netherlands_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                           data=netherlands_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_cu")

netherlands_cu <- subset(netherlands_cu, select = c(age, HI))
netherlands_cu <- netherlands_cu[order(netherlands_cu$age),]

write.csv(netherlands_cu, "all_intakes/netherlands_w_cu.csv")

# Men
netherlands_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                           data=netherlands_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_cu")

netherlands_cu <- subset(netherlands_cu, select = c(age, HI))
netherlands_cu <- netherlands_cu[order(netherlands_cu$age),]

write.csv(netherlands_cu, "all_intakes/netherlands_m_cu.csv")


##################################################################

# 36. RUN SPADE FOR SODIUM

na_notna_w <- netherlands_wom[!is.na(netherlands_wom$na), ]
na_notna_m <- netherlands_men[!is.na(netherlands_men$na), ]


netherlands_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                           data=na_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=94,
                           sex.lab="women", weights.name ="weights",
                           output.name = "netherlands_wom_na")

netherlands_na <- subset(netherlands_na, select = c(age, HI))
netherlands_na <- netherlands_na[order(netherlands_na$age),]

write.csv(netherlands_na, "all_intakes/netherlands_w_na.csv")

# Men
netherlands_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                           data=na_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=1, max.age=90,
                           sex.lab="men", weights.name ="weights",
                           output.name = "netherlands_men_na")

netherlands_na <- subset(netherlands_na, select = c(age, HI))
netherlands_na <- netherlands_na[order(netherlands_na$age),]

write.csv(netherlands_na, "all_intakes/netherlands_m_na.csv")

