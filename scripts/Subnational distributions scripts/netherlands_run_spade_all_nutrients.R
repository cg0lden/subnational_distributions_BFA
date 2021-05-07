# Running SPADE: belgium data
# File created on 5/7/21 by Simone Passarelli

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed","Subnational distributions", "belgium"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "belgium"))
###########################################################
# Remove missing obs
summary(belgium_spade)
names(belgium_spade)

# number of intakes per person:
table(belgium_spade$mday)

# Make separate datasets for men and women
belgium_wom <- subset(belgium_spade, sex==2)
belgium_men <- subset(belgium_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(belgium_wom$age)
range(belgium_men$age)

# Women
belgium_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=belgium_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="women", weights.name ="weights", 
                         output.name = "belgium_wom_vitb12")

belgium_vitb12 <- subset(belgium_vitb12, select = c(age, HI))
belgium_vitb12 <- belgium_vitb12[order(belgium_vitb12$age),]

write.csv(belgium_vitb12, "all_intakes/belgium_w_vitb12.csv")

table(belgium_men$age)

# Men
belgium_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                         data=belgium_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="men", weights.name ="weights",
                         output.name = "belgium_men_vitb12")

belgium_vitb12 <- subset(belgium_vitb12, select = c(age, HI))
belgium_vitb12 <- belgium_vitb12[order(belgium_vitb12$age),]

write.csv(belgium_vitb12, "all_intakes/belgium_m_vitb12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
belgium_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_iron")

belgium_iron <- subset(belgium_iron, select = c(age, HI))
belgium_iron <- belgium_iron[order(belgium_iron$age),]

write.csv(belgium_iron, "all_intakes/belgium_w_iron.csv")

# Men
belgium_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       output.name = "belgium_men_iron")

belgium_iron <- subset(belgium_iron, select = c(age, HI))
belgium_iron <- belgium_iron[order(belgium_iron$age),]

write.csv(belgium_iron, "all_intakes/belgium_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
belgium_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=belgium_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="women", weights.name ="weights",
                         output.name = "belgium_wom_zinc")

belgium_zinc_w <- subset(belgium_zinc_w, select = c(age, HI))
belgium_zinc_w <- belgium_zinc_w[order(belgium_zinc_w$age),]

write.csv(belgium_zinc_w, "all_intakes/belgium_w_zinc.csv")

# Men
belgium_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=belgium_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="men", weights.name ="weights",
                         output.name = "belgium_men_zinc")

belgium_zinc_m <- subset(belgium_zinc_m, select = c(age, HI))
belgium_zinc_m <- belgium_zinc_m[order(belgium_zinc_m$age),]

write.csv(belgium_zinc_m, "all_intakes/belgium_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

belgium_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_vita")

belgium_vita <- subset(belgium_vita, select = c(age, HI))
belgium_vita <- belgium_vita[order(belgium_vita$age),]

write.csv(belgium_vita, "all_intakes/belgium_w_vita.csv")

# Men
belgium_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       output.name = "belgium_men_vita")

belgium_vita <- subset(belgium_vita, select = c(age, HI))
belgium_vita <- belgium_vita[order(belgium_vita$age),]

write.csv(belgium_vita, "all_intakes/belgium_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

belgium_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_calc")

belgium_calc <- subset(belgium_calc, select = c(age, HI))
belgium_calc <- belgium_calc[order(belgium_calc$age),]

write.csv(belgium_calc, "all_intakes/belgium_w_calc.csv")

# Men
belgium_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       output.name = "belgium_men_calc")

belgium_calc <- subset(belgium_calc, select = c(age, HI))
belgium_calc <- belgium_calc[order(belgium_calc$age),]

write.csv(belgium_calc, "all_intakes/belgium_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

belgium_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=belgium_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="women", weights.name ="weights",
                          output.name = "belgium_wom_omega_3")

belgium_omega_3 <- subset(belgium_omega_3, select = c(age, HI))
belgium_omega_3 <- belgium_omega_3[order(belgium_omega_3$age),]

write.csv(belgium_omega_3, "all_intakes/belgium_w_omega_3.csv")

# Men
belgium_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3 ~cs(age),
                          data=belgium_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="men", weights.name ="weights",
                          output.name = "belgium_men_omega_3")

belgium_omega_3 <- subset(belgium_omega_3, select = c(age, HI))
belgium_omega_3 <- belgium_omega_3[order(belgium_omega_3$age),]

write.csv(belgium_omega_3, "all_intakes/belgium_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

belgium_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=belgium_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="women", weights.name ="weights",
                         output.name = "belgium_wom_energy")

belgium_energy <- subset(belgium_energy, select = c(age, HI))
belgium_energy <- belgium_energy[order(belgium_energy$age),]

write.csv(belgium_energy, "all_intakes/belgium_w_energy.csv")

# Men
belgium_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                         data=belgium_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=3, max.age=64,
                         sex.lab="men", weights.name ="weights",
                         output.name = "belgium_men_energy")

belgium_energy <- subset(belgium_energy, select = c(age, HI))
belgium_energy <- belgium_energy[order(belgium_energy$age),]

write.csv(belgium_energy, "all_intakes/belgium_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR PROTEIN

belgium_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=belgium_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="women", weights.name ="weights",
                          output.name = "belgium_wom_protein")

belgium_protein <- subset(belgium_protein, select = c(age, HI))
belgium_protein <- belgium_protein[order(belgium_protein$age),]

write.csv(belgium_protein, "all_intakes/belgium_w_protein.csv")

# Men
belgium_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=belgium_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="men", weights.name ="weights",
                          output.name = "belgium_men_protein")

belgium_protein <- subset(belgium_protein, select = c(age, HI))
belgium_protein <- belgium_protein[order(belgium_protein$age),]

write.csv(belgium_protein, "all_intakes/belgium_m_protein.csv")

##################################################################

# 9. RUN SPADE FOR CARB

belgium_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_carb")

belgium_carb <- subset(belgium_carb, select = c(age, HI))
belgium_carb <- belgium_carb[order(belgium_carb$age),]

write.csv(belgium_carb, "all_intakes/belgium_w_carb.csv")

# Men
belgium_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       output.name = "belgium_men_carb")

belgium_carb <- subset(belgium_carb, select = c(age, HI))
belgium_carb <- belgium_carb[order(belgium_carb$age),]

write.csv(belgium_carb, "all_intakes/belgium_m_carb.csv")

##################################################################

# 10. RUN SPADE FOR FIBER

belgium_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=belgium_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=64,
                        sex.lab="women", weights.name ="weights",
                        output.name = "belgium_wom_fiber")

belgium_fiber <- subset(belgium_fiber, select = c(age, HI))
belgium_fiber <- belgium_fiber[order(belgium_fiber$age),]

write.csv(belgium_fiber, "all_intakes/belgium_w_fiber.csv")

# Men
belgium_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=belgium_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=64,
                        sex.lab="men", weights.name ="weights",
                        output.name = "belgium_men_fiber")

belgium_fiber <- subset(belgium_fiber, select = c(age, HI))
belgium_fiber <- belgium_fiber[order(belgium_fiber$age),]

write.csv(belgium_fiber, "all_intakes/belgium_m_fiber.csv")

##################################################################

# 11. RUN SPADE FOR FAT

belgium_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=belgium_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=3, max.age=64,
                      sex.lab="women", weights.name ="weights",
                      output.name = "belgium_wom_fat")

belgium_fat <- subset(belgium_fat, select = c(age, HI))
belgium_fat <- belgium_fat[order(belgium_fat$age),]

write.csv(belgium_fat, "all_intakes/belgium_w_fat.csv")

# Men
belgium_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                      data=belgium_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=3, max.age=64,
                      sex.lab="men", weights.name ="weights",
                      output.name = "belgium_men_fat")

belgium_fat <- subset(belgium_fat, select = c(age, HI))
belgium_fat <- belgium_fat[order(belgium_fat$age),]

write.csv(belgium_fat, "all_intakes/belgium_m_fat.csv")


##################################################################

# 12. RUN SPADE FOR MUFA

belgium_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_mufa")

belgium_mufa <- subset(belgium_mufa, select = c(age, HI))
belgium_mufa <- belgium_mufa[order(belgium_mufa$age),]

write.csv(belgium_mufa, "all_intakes/belgium_w_mufa.csv")

# Men
belgium_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_mufa")

belgium_mufa <- subset(belgium_mufa, select = c(age, HI))
belgium_mufa <- belgium_mufa[order(belgium_mufa$age),]

write.csv(belgium_mufa, "all_intakes/belgium_m_mufa.csv")

##################################################################

# 13. RUN SPADE FOR PUFA

belgium_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_pufa")

belgium_pufa <- subset(belgium_pufa, select = c(age, HI))
belgium_pufa <- belgium_pufa[order(belgium_pufa$age),]

write.csv(belgium_pufa, "all_intakes/belgium_w_pufa.csv")

# Men
belgium_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_pufa")

belgium_pufa <- subset(belgium_pufa, select = c(age, HI))
belgium_pufa <- belgium_pufa[order(belgium_pufa$age),]

write.csv(belgium_pufa, "all_intakes/belgium_m_pufa.csv")

##################################################################

# 14. RUN SPADE FOR THIAMIN

belgium_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_thia")

belgium_thia <- subset(belgium_thia, select = c(age, HI))
belgium_thia <- belgium_thia[order(belgium_thia$age),]

write.csv(belgium_thia, "all_intakes/belgium_w_thia.csv")

# Men
belgium_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_thia")

belgium_thia <- subset(belgium_thia, select = c(age, HI))
belgium_thia <- belgium_thia[order(belgium_thia$age),]

write.csv(belgium_thia, "all_intakes/belgium_m_thia.csv")

##################################################################

# 15. RUN SPADE FOR RIBOFLAVIN

belgium_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_ribo")

belgium_ribo <- subset(belgium_ribo, select = c(age, HI))
belgium_ribo <- belgium_ribo[order(belgium_ribo$age),]

write.csv(belgium_ribo, "all_intakes/belgium_w_ribo.csv")

# Men
belgium_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_ribo")

belgium_ribo <- subset(belgium_ribo, select = c(age, HI))
belgium_ribo <- belgium_ribo[order(belgium_ribo$age),]

write.csv(belgium_ribo, "all_intakes/belgium_m_ribo.csv")

##################################################################
# 16. RUN SPADE FOR NIACIN
niac_notna_w <- belgium_wom[!is.na(belgium_wom$niac), ]
niac_notna_m <- belgium_men[!is.na(belgium_men$niac), ]


belgium_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=niac_notna_w, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_niac")

belgium_niac <- subset(belgium_niac, select = c(age, HI))
belgium_niac <- belgium_niac[order(belgium_niac$age),]

write.csv(belgium_niac, "all_intakes/belgium_w_niac.csv")

# Men
belgium_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=niac_notna_m, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_niac")

belgium_niac <- subset(belgium_niac, select = c(age, HI))
belgium_niac <- belgium_niac[order(belgium_niac$age),]

write.csv(belgium_niac, "all_intakes/belgium_m_niac.csv")

##################################################################

# 17. RUN SPADE FOR VITAMIN B6

belgium_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=belgium_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=64,
                        sex.lab="women", weights.name ="weights",
                        
                        output.name = "belgium_wom_vitb6")

belgium_vitb6 <- subset(belgium_vitb6, select = c(age, HI))
belgium_vitb6 <- belgium_vitb6[order(belgium_vitb6$age),]

write.csv(belgium_vitb6, "all_intakes/belgium_w_vitb6.csv")

# Men
belgium_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=belgium_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=3, max.age=64,
                        sex.lab="men", weights.name ="weights",
                        
                        output.name = "belgium_men_vitb6")

belgium_vitb6 <- subset(belgium_vitb6, select = c(age, HI))
belgium_vitb6 <- belgium_vitb6[order(belgium_vitb6$age),]

write.csv(belgium_vitb6, "all_intakes/belgium_m_vitb6.csv")

##################################################################

# 18. RUN SPADE FOR FOLATE

fola_notna_w <- belgium_wom[!is.na(belgium_wom$fola), ]
fola_notna_m <- belgium_men[!is.na(belgium_men$fola), ]

belgium_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=fola_notna_w, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_fola")

belgium_fola <- subset(belgium_fola, select = c(age, HI))
belgium_fola <- belgium_fola[order(belgium_fola$age),]

write.csv(belgium_fola, "all_intakes/belgium_w_fola.csv")

# Men
belgium_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=fola_notna_m, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_fola")

belgium_fola <- subset(belgium_fola, select = c(age, HI))
belgium_fola <- belgium_fola[order(belgium_fola$age),]

write.csv(belgium_fola, "all_intakes/belgium_m_fola.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN D

belgium_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_vitd")

belgium_vitd <- subset(belgium_vitd, select = c(age, HI))
belgium_vitd <- belgium_vitd[order(belgium_vitd$age),]

write.csv(belgium_vitd, "all_intakes/belgium_w_vitd.csv")

# Men
belgium_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_vitd")

belgium_vitd <- subset(belgium_vitd, select = c(age, HI))
belgium_vitd <- belgium_vitd[order(belgium_vitd$age),]

write.csv(belgium_vitd, "all_intakes/belgium_m_vitd.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN C

belgium_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_vitc")

belgium_vitc <- subset(belgium_vitc, select = c(age, HI))
belgium_vitc <- belgium_vitc[order(belgium_vitc$age),]

write.csv(belgium_vitc, "all_intakes/belgium_w_vitc.csv")

# Men
belgium_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_vitc")

belgium_vitc <- subset(belgium_vitc, select = c(age, HI))
belgium_vitc <- belgium_vitc[order(belgium_vitc$age),]

write.csv(belgium_vitc, "all_intakes/belgium_m_vitc.csv")

##################################################################

# 21. RUN SPADE FOR PHOSPHORUS

belgium_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_phos")

belgium_phos <- subset(belgium_phos, select = c(age, HI))
belgium_phos <- belgium_phos[order(belgium_phos$age),]

write.csv(belgium_phos, "all_intakes/belgium_w_phos.csv")

# Men
belgium_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_phos")

belgium_phos <- subset(belgium_phos, select = c(age, HI))
belgium_phos <- belgium_phos[order(belgium_phos$age),]

write.csv(belgium_phos, "all_intakes/belgium_m_phos.csv")

##################################################################

# 22. RUN SPADE FOR MG

belgium_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=belgium_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=64,
                     sex.lab="women", weights.name ="weights",
                     
                     output.name = "belgium_wom_mg")

belgium_mg <- subset(belgium_mg, select = c(age, HI))
belgium_mg <- belgium_mg[order(belgium_mg$age),]

write.csv(belgium_mg, "all_intakes/belgium_w_mg.csv")

# Men
belgium_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=belgium_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=64,
                     sex.lab="men", weights.name ="weights",
                     
                     output.name = "belgium_men_mg")

belgium_mg <- subset(belgium_mg, select = c(age, HI))
belgium_mg <- belgium_mg[order(belgium_mg$age),]

write.csv(belgium_mg, "all_intakes/belgium_m_mg.csv")

##################################################################

# 23. RUN SPADE FOR POTASSIUM

belgium_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       
                       output.name = "belgium_wom_pota")

belgium_pota <- subset(belgium_pota, select = c(age, HI))
belgium_pota <- belgium_pota[order(belgium_pota$age),]

write.csv(belgium_pota, "all_intakes/belgium_w_pota.csv")

# Men
belgium_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       
                       output.name = "belgium_men_pota")

belgium_pota <- subset(belgium_pota, select = c(age, HI))
belgium_pota <- belgium_pota[order(belgium_pota$age),]

write.csv(belgium_pota, "all_intakes/belgium_m_pota.csv")

##################################################################

# 24. RUN SPADE FOR VITAMIN E

belgium_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=belgium_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="women", weights.name ="weights",
                       output.name = "belgium_wom_vite")

belgium_vite <- subset(belgium_vite, select = c(age, HI))
belgium_vite <- belgium_vite[order(belgium_vite$age),]

write.csv(belgium_vite, "all_intakes/belgium_w_vite.csv")

# Men
belgium_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=belgium_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=3, max.age=64,
                       sex.lab="men", weights.name ="weights",
                       output.name = "belgium_men_vite")

belgium_vite <- subset(belgium_vite, select = c(age, HI))
belgium_vite <- belgium_vite[order(belgium_vite$age),]

write.csv(belgium_vite, "all_intakes/belgium_m_vite.csv")


##################################################################

# 25. RUN SPADE FOR ala

belgium_ala <- f.spade(frml.ia=ala~fp(age), frml.if=ala ~cs(age), 
                          data=belgium_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="women", weights.name ="weights",
                          
                          output.name = "belgium_wom_ala")

belgium_ala <- subset(belgium_ala, select = c(age, HI))
belgium_ala <- belgium_ala[order(belgium_ala$age),]

write.csv(belgium_ala, "all_intakes/belgium_w_ala.csv")

# Men
belgium_ala <- f.spade(frml.ia=ala~fp(age), frml.if=ala ~cs(age), 
                          data=belgium_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=3, max.age=64,
                          sex.lab="men", weights.name ="weights",
                          
                          output.name = "belgium_men_ala")

belgium_ala <- subset(belgium_ala, select = c(age, HI))
belgium_ala <- belgium_ala[order(belgium_ala$age),]

write.csv(belgium_ala, "all_intakes/belgium_m_ala.csv")


##################################################################

# 26. RUN SPADE FOR SELENIUM

belgium_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=belgium_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=64,
                     sex.lab="women", weights.name ="weights",
                     
                     output.name = "belgium_wom_se")

belgium_se <- subset(belgium_se, select = c(age, HI))
belgium_se <- belgium_se[order(belgium_se$age),]

write.csv(belgium_se, "all_intakes/belgium_w_se.csv")

# Men
belgium_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                     data=belgium_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=3, max.age=64,
                     sex.lab=" men", weights.name ="weights",
                     
                     output.name = "belgium_men_se")

belgium_se <- subset(belgium_se, select = c(age, HI))
belgium_se <- belgium_se[order(belgium_se$age),]

write.csv(belgium_se, "all_intakes/belgium_m_se.csv")


##################################################################

# 27. RUN SPADE FOR BETA CAROTENE

betacarot_notna_w <- belgium_wom[!is.na(belgium_wom$betacarot), ]
betacarot_notna_m <- belgium_men[!is.na(belgium_men$betacarot), ]


belgium_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=betacarot_notna_w, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=3, max.age=64,
                            sex.lab="women", weights.name ="weights",
                            output.name = "belgium_wom_betacarot")

belgium_betacarot <- subset(belgium_betacarot, select = c(age, HI))
belgium_betacarot <- belgium_betacarot[order(belgium_betacarot$age),]

write.csv(belgium_betacarot, "all_intakes/belgium_w_betacarot.csv")

# Men
belgium_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                            data=betacarot_notna_m, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=3, max.age=64,
                            sex.lab="men", weights.name ="weights",
                            output.name = "belgium_men_betacarot")

belgium_betacarot <- subset(belgium_betacarot, select = c(age, HI))
belgium_betacarot <- belgium_betacarot[order(belgium_betacarot$age),]

write.csv(belgium_betacarot, "all_intakes/belgium_m_betacarot.csv")


##################################################################

# 28. RUN SPADE FOR ALPHA-LINOLENIC ACID

ala_notna_w <- belgium_wom[!is.na(belgium_wom$ala), ]
ala_notna_m <- belgium_men[!is.na(belgium_men$ala), ]


belgium_ala <- f.spade(frml.ia=ala~fp(age), frml.if="no.if", 
                                 data=ala_notna_w, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=3, max.age=64,
                                 sex.lab="women", weights.name ="weights",
                                 output.name = "belgium_wom_ala")

belgium_ala <- subset(belgium_ala, select = c(age, HI))
belgium_ala <- belgium_ala[order(belgium_ala$age),]

write.csv(belgium_ala, "all_intakes/belgium_w_ala.csv")

# Men
belgium_ala <- f.spade(frml.ia=ala~fp(age), frml.if="no.if", 
                                 data=ala_notna_m, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=3, max.age=64,
                                 sex.lab="men", weights.name ="weights",
                                 output.name = "belgium_men_ala")

belgium_ala <- subset(belgium_ala, select = c(age, HI))
belgium_ala <- belgium_ala[order(belgium_ala$age),]

write.csv(belgium_ala, "all_intakes/belgium_m_ala.csv")

##################################################################

# 29. RUN SPADE FOR LINOLEIC ACID

la_notna_w <- belgium_wom[!is.na(belgium_wom$la), ]
la_notna_m <- belgium_men[!is.na(belgium_men$la), ]


belgium_la <- f.spade(frml.ia=la~fp(age), frml.if="no.if", 
                           data=la_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_la")

belgium_la <- subset(belgium_la, select = c(age, HI))
belgium_la <- belgium_la[order(belgium_la$age),]

write.csv(belgium_la, "all_intakes/belgium_w_la.csv")

# Men
belgium_la <- f.spade(frml.ia=la~fp(age), frml.if="no.if", 
                           data=la_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_la")

belgium_la <- subset(belgium_la, select = c(age, HI))
belgium_la <- belgium_la[order(belgium_la$age),]

write.csv(belgium_la, "all_intakes/belgium_m_la.csv")


##################################################################

# 30. RUN SPADE FOR IODINE

iod_notna_w <- belgium_wom[!is.na(belgium_wom$iod), ]
iod_notna_m <- belgium_men[!is.na(belgium_men$iod), ]


belgium_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                           data=iod_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_iod")

belgium_iod <- subset(belgium_iod, select = c(age, HI))
belgium_iod <- belgium_iod[order(belgium_iod$age),]

write.csv(belgium_iod, "all_intakes/belgium_w_iod.csv")

# Men
belgium_iod <- f.spade(frml.ia=iod~fp(age), frml.if="no.if", 
                           data=iod_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_iod")

belgium_iod <- subset(belgium_iod, select = c(age, HI))
belgium_iod <- belgium_iod[order(belgium_iod$age),]

write.csv(belgium_iod, "all_intakes/belgium_m_iod.csv")

##################################################################

# 31. RUN SPADE FOR VITAMIN K

vitk_notna_w <- belgium_wom[!is.na(belgium_wom$vitk), ]
vitk_notna_m <- belgium_men[!is.na(belgium_men$vitk), ]


belgium_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                           data=vitk_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_vitk")

belgium_vitk <- subset(belgium_vitk, select = c(age, HI))
belgium_vitk <- belgium_vitk[order(belgium_vitk$age),]

write.csv(belgium_vitk, "all_intakes/belgium_w_vitk.csv")

# Men
belgium_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                           data=vitk_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_vitk")

belgium_vitk <- subset(belgium_vitk, select = c(age, HI))
belgium_vitk <- belgium_vitk[order(belgium_vitk$age),]

write.csv(belgium_vitk, "all_intakes/belgium_m_vitk.csv")


##################################################################

# 32. RUN SPADE FOR CHOLESTEROL

belgium_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                            data=belgium_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=3, max.age=64,
                            sex.lab="women", weights.name ="weights",
                            output.name = "belgium_wom_cholest")

belgium_cholest <- subset(belgium_cholest, select = c(age, HI))
belgium_cholest <- belgium_cholest[order(belgium_cholest$age),]

write.csv(belgium_cholest, "all_intakes/belgium_w_cholest.csv")

# Men
belgium_cholest <- f.spade(frml.ia=cholest~fp(age), frml.if="no.if", 
                            data=belgium_men, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=3, max.age=64,
                            sex.lab="men", weights.name ="weights",
                            output.name = "belgium_men_cholest")

belgium_cholest <- subset(belgium_cholest, select = c(age, HI))
belgium_cholest <- belgium_cholest[order(belgium_cholest$age),]

write.csv(belgium_cholest, "all_intakes/belgium_m_cholest.csv")


##################################################################

# 33. RUN SPADE FOR SATURATED FAT

belgium_sfa <- f.spade(frml.ia=sfa~fp(age), frml.if="no.if", 
                               data=belgium_wom, seed=123,  backtrans.nr = 3,
                               dgts.distr = 2, min.age=3, max.age=64,
                               sex.lab="women", weights.name ="weights",
                               output.name = "belgium_wom_sfa")

belgium_sfa <- subset(belgium_sfa, select = c(age, HI))
belgium_sfa <- belgium_sfa[order(belgium_sfa$age),]

write.csv(belgium_sfa, "all_intakes/belgium_w_sfa.csv")

# Men
belgium_sfa <- f.spade(frml.ia=sfa~fp(age), frml.if="no.if", 
                               data=belgium_men, seed=123,  backtrans.nr = 3,
                               dgts.distr = 2, min.age=3, max.age=64,
                               sex.lab="men", weights.name ="weights",
                               output.name = "belgium_men_sfa")

belgium_sfa <- subset(belgium_sfa, select = c(age, HI))
belgium_sfa <- belgium_sfa[order(belgium_sfa$age),]

write.csv(belgium_sfa, "all_intakes/belgium_m_sfa.csv")

##################################################################

# 34. RUN SPADE FOR TRANS FAT

belgium_tfa <- f.spade(frml.ia=tfa~fp(age), frml.if="no.if", 
                           data=belgium_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_tfa")

belgium_tfa <- subset(belgium_tfa, select = c(age, HI))
belgium_tfa <- belgium_tfa[order(belgium_tfa$age),]

write.csv(belgium_tfa, "all_intakes/belgium_w_tfa.csv")

# Men
belgium_tfa <- f.spade(frml.ia=tfa~fp(age), frml.if="no.if", 
                           data=belgium_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_tfa")

belgium_tfa <- subset(belgium_tfa, select = c(age, HI))
belgium_tfa <- belgium_tfa[order(belgium_tfa$age),]

write.csv(belgium_tfa, "all_intakes/belgium_m_tfa.csv")

##################################################################

# 35. RUN SPADE FOR TRANS FAT

belgium_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                           data=belgium_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_cu")

belgium_cu <- subset(belgium_cu, select = c(age, HI))
belgium_cu <- belgium_cu[order(belgium_cu$age),]

write.csv(belgium_cu, "all_intakes/belgium_w_cu.csv")

# Men
belgium_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                           data=belgium_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_cu")

belgium_cu <- subset(belgium_cu, select = c(age, HI))
belgium_cu <- belgium_cu[order(belgium_cu$age),]

write.csv(belgium_cu, "all_intakes/belgium_m_cu.csv")


##################################################################

# 36. RUN SPADE FOR SODIUM

na_notna_w <- belgium_wom[!is.na(belgium_wom$na), ]
na_notna_m <- belgium_men[!is.na(belgium_men$na), ]


belgium_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                           data=na_notna_w, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="women", weights.name ="weights",
                           output.name = "belgium_wom_na")

belgium_na <- subset(belgium_na, select = c(age, HI))
belgium_na <- belgium_na[order(belgium_na$age),]

write.csv(belgium_na, "all_intakes/belgium_w_na.csv")

# Men
belgium_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                           data=na_notna_m, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=3, max.age=64,
                           sex.lab="men", weights.name ="weights",
                           output.name = "belgium_men_na")

belgium_na <- subset(belgium_na, select = c(age, HI))
belgium_na <- belgium_na[order(belgium_na$age),]

write.csv(belgium_na, "all_intakes/belgium_m_na.csv")

