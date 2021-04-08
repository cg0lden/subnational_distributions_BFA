# Running SPADE: Philippines data
# File created on 12/3/20 by Simone Passarelli
# Edited by Simone Passarelli  3/3/2021


# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data",  "processed", "Subnational distributions","philippines"))

SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Philippines"))

# TOTAL.output <- (here("output", "phil", 2_logbooks"))
###########################################################
# Remove missing obs
summary(Phil_spade)

# Make separate datasets for men and women
phil_wom <- subset(Phil_spade, sex==2)
#phil_men <- subset(phil_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(phil_wom$age)

#round the age variables down
phil_wom$age <- floor(phil_wom$age)


# number of intakes per person:
table(phil_wom$age)

# Women
phil_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                   data=phil_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=15, max.age=47,
                   sex.lab="women", 
                   output.name = "phil_wom_b12")

phil_b12 <- subset(phil_b12, select = c(age, HI))
phil_b12 <- phil_b12[order(phil_b12$age),]

write.csv(phil_b12, "all_intakes/phil_w_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
phil_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=phil_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=47,
                    sex.lab="women",
                    output.name = "phil_wom_iron")

phil_iron <- subset(phil_iron, select = c(age, HI))
phil_iron <- phil_iron[order(phil_iron$age),]

write.csv(phil_iron, "all_intakes/phil_w_iron.csv")

##################################################################

# 3. RUN SPADE FOR ZINC
phil_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=phil_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=15, max.age=47,
                      sex.lab="women",
                      output.name = "phil_wom_zinc")

phil_zinc_w <- subset(phil_zinc_w, select = c(age, HI))
phil_zinc_w <- phil_zinc_w[order(phil_zinc_w$age),]

write.csv(phil_zinc_w, "all_intakes/phil_w_zinc.csv")


##################################################################

# 4. RUN SPADE FOR VIT A

phil_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=phil_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=47,
                    sex.lab="women",
                    output.name = "phil_wom_vita")

phil_vita <- subset(phil_vita, select = c(age, HI))
phil_vita <- phil_vita[order(phil_vita$age),]

write.csv(phil_vita, "all_intakes/phil_w_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

phil_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=phil_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=47,
                    sex.lab="women",
                    output.name = "phil_wom_calc")

phil_calc <- subset(phil_calc, select = c(age, HI))
phil_calc <- phil_calc[order(phil_calc$age),]

write.csv(phil_calc, "all_intakes/phil_w_calc.csv")

##################################################################

# 6. RUN SPADE FOR OMEGA 3 


phil_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=phil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=15, max.age=47,
                       sex.lab="women",
                       output.name = "phil_wom_omega_3")

phil_omega_3 <- subset(phil_omega_3, select = c(age, HI))
phil_omega_3 <- phil_omega_3[order(phil_omega_3$age),]

write.csv(phil_omega_3, "all_intakes/phil_w_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITAMIN C

phil_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if=vitc~cs(age),
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_vitc")

phil_vitc <- subset(phil_vitc, select = c(age, HI))
phil_vitc <- phil_vitc[order(phil_vitc$age),]

write.csv(phil_vitc, "all_intakes/phil_w_vitc.csv")

##################################################################

# 8. RUN SPADE FOR THIAMIN

phil_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_thia")

phil_thia <- subset(phil_thia, select = c(age, HI))
phil_thia <- phil_thia[order(phil_thia$age),]

write.csv(phil_thia, "all_intakes/phil_w_thia.csv")

##################################################################

# 9. RUN SPADE FOR RIBOFLAVIN

phil_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_ribo")

phil_ribo <- subset(phil_ribo, select = c(age, HI))
phil_ribo <- phil_ribo[order(phil_ribo$age),]

write.csv(phil_ribo, "all_intakes/phil_w_ribo.csv")

##################################################################

# 10. RUN SPADE FOR NIACIN

phil_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_niac")

phil_niac <- subset(phil_niac, select = c(age, HI))
phil_niac <- phil_niac[order(phil_niac$age),]

write.csv(phil_niac, "all_intakes/phil_w_niac.csv")

##################################################################

# 11. RUN SPADE FOR ENERGY

phil_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_energy")

phil_energy <- subset(phil_energy, select = c(age, HI))
phil_energy <- phil_energy[order(phil_energy$age),]

write.csv(phil_energy, "all_intakes/phil_w_energy.csv")

##################################################################

# 12. RUN SPADE FOR CARB

phil_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_carb")

phil_carb <- subset(phil_carb, select = c(age, HI))
phil_carb <- phil_carb[order(phil_carb$age),]

write.csv(phil_carb, "all_intakes/phil_w_carb.csv")

##################################################################

# 13. RUN SPADE FOR PROTEIN

phil_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_protein")

phil_protein <- subset(phil_protein, select = c(age, HI))
phil_protein <- phil_protein[order(phil_protein$age),]

write.csv(phil_protein, "all_intakes/phil_w_protein.csv")

##################################################################

# 14. RUN SPADE FOR FAT

phil_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                     data=phil_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=15, max.age=47, 
                     sex.lab="women",
                     output.name = "phil_wom_fat")

phil_fat <- subset(phil_fat, select = c(age, HI))
phil_fat <- phil_fat[order(phil_fat$age),]

write.csv(phil_fat, "all_intakes/phil_w_fat.csv")