# Running SPADE: lao data
# File created on 12/3/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "Lao"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Lao"))
# TOTAL.output <- (here("output", "lao", 2_logbooks"))
###########################################################
# Remove missing obs
summary(Lao_spade)

# Make separate datasets for men and women
lao_wom <- subset(Lao_spade, sex==2)
lao_men <- subset(Lao_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(lao_wom$age)
range(lao_men$age)

#round the age variables down
lao_wom$age <- floor(lao_wom$age)
lao_men$age <- floor(lao_men$age)

# number of intakes per person:
table(lao_wom$age)

# Women
lao_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=lao_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=82,
                      sex.lab="women", 
                      output.name = "lao_wom_b12")

lao_b12 <- subset(lao_b12, select = c(age, HI))
lao_b12 <- lao_b12[order(lao_b12$age),]

write.csv(lao_b12, "all_intakes/lao_w_b12.csv")

# Men
lao_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                      data=lao_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=89,
                      sex.lab="men",
                      output.name = "lao_men_b12")

lao_b12 <- subset(lao_b12, select = c(age, HI))
lao_b12 <- lao_b12[order(lao_b12$age),]

write.csv(lao_b12, "all_intakes/lao_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
lao_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=lao_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=82,
                       sex.lab="women",
                       output.name = "lao_wom_iron")

lao_iron <- subset(lao_iron, select = c(age, HI))
lao_iron <- lao_iron[order(lao_iron$age),]

write.csv(lao_iron, "all_intakes/lao_w_iron.csv")

# Men
lao_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       sex.lab="men",
                       output.name = "lao_men_iron")

lao_iron <- subset(lao_iron, select = c(age, HI))
lao_iron <- lao_iron[order(lao_iron$age),]

write.csv(lao_iron, "all_intakes/lao_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
lao_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=lao_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=0, max.age=82,
                         sex.lab="women",
                         output.name = "lao_wom_zinc")

lao_zinc_w <- subset(lao_zinc_w, select = c(age, HI))
lao_zinc_w <- lao_zinc_w[order(lao_zinc_w$age),]

write.csv(lao_zinc_w, "all_intakes/lao_w_zinc.csv")

# Men
lao_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=lao_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=0, max.age=89,
                         sex.lab="men",
                         output.name = "lao_men_zinc")

lao_zinc_m <- subset(lao_zinc_m, select = c(age, HI))
lao_zinc_m <- lao_zinc_m[order(lao_zinc_m$age),]

write.csv(lao_zinc_m, "all_intakes/lao_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

lao_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=lao_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=82,
                       sex.lab="women",
                       output.name = "lao_wom_vita")

lao_vita <- subset(lao_vita, select = c(age, HI))
lao_vita <- lao_vita[order(lao_vita$age),]

write.csv(lao_vita, "all_intakes/lao_w_vita.csv")

# Men
lao_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       sex.lab="men",
                       output.name = "lao_men_vita")

lao_vita <- subset(lao_vita, select = c(age, HI))
lao_vita <- lao_vita[order(lao_vita$age),]

write.csv(lao_vita, "all_intakes/lao_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

lao_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=lao_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                       output.name = "lao_wom_calc")

lao_calc <- subset(lao_calc, select = c(age, HI))
lao_calc <- lao_calc[order(lao_calc$age),]

write.csv(lao_calc, "all_intakes/lao_w_calc.csv")

# Men
lao_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       sex.lab="men",
                       output.name = "lao_men_calc")

lao_calc <- subset(lao_calc, select = c(age, HI))
lao_calc <- lao_calc[order(lao_calc$age),]

write.csv(lao_calc, "all_intakes/lao_m_calc.csv")


##################################################################

# 8. RUN SPADE FOR OMEGA 3 


lao_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                          data=lao_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=0, max.age=82,
                          sex.lab="women",
                          output.name = "lao_wom_omega_3")

lao_omega_3 <- subset(lao_omega_3, select = c(age, HI))
lao_omega_3 <- lao_omega_3[order(lao_omega_3$age),]

write.csv(lao_omega_3, "all_intakes/lao_w_omega_3.csv")

# Men
lao_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                          data=lao_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=0, max.age=89,
                          sex.lab="men",
                          output.name = "lao_men_omega_3")

lao_omega_3 <- subset(lao_omega_3, select = c(age, HI))
lao_omega_3 <- lao_omega_3[order(lao_omega_3$age),]

write.csv(lao_omega_3, "all_intakes/lao_m_omega_3.csv")
##################################################################

# 9. RUN SPADE FOR VITAMIN C

lao_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if=vitc~cs(age),
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_vitc")

lao_vitc <- subset(lao_vitc, select = c(age, HI))
lao_vitc <- lao_vitc[order(lao_vitc$age),]

write.csv(lao_vitc, "all_intakes/lao_w_vitc.csv")

# Men
lao_vitc <- f.spade(frml.ia=vitc~fp(age),  frml.if=vitc~cs(age),
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_vitc")

lao_vitc <- subset(lao_vitc, select = c(age, HI))
lao_vitc <- lao_vitc[order(lao_vitc$age),]

write.csv(lao_vitc, "all_intakes/lao_m_vitc.csv")


##################################################################
# 10. RUN SPADE FOR THIAMIN

lao_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_niac")

lao_niac <- subset(lao_niac, select = c(age, HI))
lao_niac <- lao_niac[order(lao_niac$age),]

write.csv(lao_niac, "all_intakes/lao_w_niac.csv")

# Men
lao_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_niac")

lao_niac <- subset(lao_niac, select = c(age, HI))
lao_niac <- lao_niac[order(lao_niac$age),]

write.csv(lao_niac, "all_intakes/lao_m_niac.csv")


##################################################################
# 11. RUN SPADE FOR THIAMIN

lao_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_thia")

lao_thia <- subset(lao_thia, select = c(age, HI))
lao_thia <- lao_thia[order(lao_thia$age),]

write.csv(lao_thia, "all_intakes/lao_w_thia.csv")

# Men
lao_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_thia")

lao_thia <- subset(lao_thia, select = c(age, HI))
lao_thia <- lao_thia[order(lao_thia$age),]

write.csv(lao_thia, "all_intakes/lao_m_thia.csv")

##################################################################
# 12. RUN SPADE FOR RIBOFLAVIN

lao_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_ribo")

lao_ribo <- subset(lao_ribo, select = c(age, HI))
lao_ribo <- lao_ribo[order(lao_ribo$age),]

write.csv(lao_ribo, "all_intakes/lao_w_ribo.csv")

# Men
lao_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_ribo")

lao_ribo <- subset(lao_ribo, select = c(age, HI))
lao_ribo <- lao_ribo[order(lao_ribo$age),]

write.csv(lao_ribo, "all_intakes/lao_m_ribo.csv")

##################################################################
# 13. RUN SPADE FOR SODIUM

lao_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_na")

lao_na <- subset(lao_na, select = c(age, HI))
lao_na <- lao_na[order(lao_na$age),]

write.csv(lao_na, "all_intakes/lao_w_na.csv")

# Men
lao_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_na")

lao_na <- subset(lao_na, select = c(age, HI))
lao_na <- lao_na[order(lao_na$age),]

write.csv(lao_na, "all_intakes/lao_m_na.csv")

##################################################################
# 14. RUN SPADE FOR ENERGY

lao_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_energy")

lao_energy <- subset(lao_energy, select = c(age, HI))
lao_energy <- lao_energy[order(lao_energy$age),]

write.csv(lao_energy, "all_intakes/lao_w_energy.csv")

# Men
lao_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_energy")

lao_energy <- subset(lao_energy, select = c(age, HI))
lao_energy <- lao_energy[order(lao_energy$age),]

write.csv(lao_energy, "all_intakes/lao_m_energy.csv")

##################################################################
# 15. RUN SPADE FOR FAT

lao_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_fat")

lao_fat <- subset(lao_fat, select = c(age, HI))
lao_fat <- lao_fat[order(lao_fat$age),]

write.csv(lao_fat, "all_intakes/lao_w_fat.csv")

# Men
lao_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_fat")

lao_fat <- subset(lao_fat, select = c(age, HI))
lao_fat <- lao_fat[order(lao_fat$age),]

write.csv(lao_fat, "all_intakes/lao_m_fat.csv")

##################################################################
# 16. RUN SPADE FOR PROTEIN

lao_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_protein")

lao_protein <- subset(lao_protein, select = c(age, HI))
lao_protein <- lao_protein[order(lao_protein$age),]

write.csv(lao_protein, "all_intakes/lao_w_protein.csv")

# Men
lao_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_protein")

lao_protein <- subset(lao_protein, select = c(age, HI))
lao_protein <- lao_protein[order(lao_protein$age),]

write.csv(lao_protein, "all_intakes/lao_m_protein.csv")

##################################################################
# 17. RUN SPADE FOR CARB

lao_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_carb")

lao_carb <- subset(lao_carb, select = c(age, HI))
lao_carb <- lao_carb[order(lao_carb$age),]

write.csv(lao_carb, "all_intakes/lao_w_carb.csv")

# Men
lao_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_carb")

lao_carb <- subset(lao_carb, select = c(age, HI))
lao_carb <- lao_carb[order(lao_carb$age),]

write.csv(lao_carb, "all_intakes/lao_m_carb.csv")

##################################################################
# 18. RUN SPADE FOR FIBER

lao_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                    data=lao_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=82,                     
                    sex.lab="women",
                    output.name = "lao_wom_fiber")

lao_fiber <- subset(lao_fiber, select = c(age, HI))
lao_fiber <- lao_fiber[order(lao_fiber$age),]

write.csv(lao_fiber, "all_intakes/lao_w_fiber.csv")

# Men
lao_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                    data=lao_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=89,
                    sex.lab="men",
                    output.name = "lao_men_fiber")

lao_fiber <- subset(lao_fiber, select = c(age, HI))
lao_fiber <- lao_fiber[order(lao_fiber$age),]

write.csv(lao_fiber, "all_intakes/lao_m_fiber.csv")