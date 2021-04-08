# Running SPADE: burkina data
# File created on 12/7/20 by Simone Passarelli
# Edited by Simone Passarelli 2/19/21

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "burkina"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "burkina"))
# TOTAL.output <- (here("output", "burkina", 2_logbooks"))
###########################################################
# Remove missing obs
summary(burkina_spade)

# Make separate datasets for men and women
burkina_wom <- subset(burkina_spade, sex==2)
burkina_men <- subset(burkina_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(burkina_wom$age)
range(burkina_men$age)

# number of intakes per person:
table(burkina_wom$age)
table(burkina_men$age)

# Women
burkina_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=burkina_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=55,
                   sex.lab="women", weights.name = "sample_weight",
                   output.name = "burkina_wom_b12")

burkina_b12 <- subset(burkina_b12, select = c(age, HI))
burkina_b12 <- burkina_b12[order(burkina_b12$age),]

write.csv(burkina_b12, "all_intakes/burkina_w_b12.csv")

# Men
burkina_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=burkina_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4, 
                   sex.lab="men", weights.name = "sample_weight",
                   output.name = "burkina_men_b12")

burkina_b12 <- subset(burkina_b12, select = c(age, HI))
burkina_b12 <- burkina_b12[order(burkina_b12$age),]

write.csv(burkina_b12, "all_intakes/burkina_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
burkina_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=55,
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_iron")

burkina_iron <- subset(burkina_iron, select = c(age, HI))
burkina_iron <- burkina_iron[order(burkina_iron$age),]

write.csv(burkina_iron, "all_intakes/burkina_w_iron.csv")

# Men
burkina_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=burkina_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men", weights.name = "sample_weight",
                    output.name = "burkina_men_iron")

burkina_iron <- subset(burkina_iron, select = c(age, HI))
burkina_iron <- burkina_iron[order(burkina_iron$age),]

write.csv(burkina_iron, "all_intakes/burkina_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
burkina_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=burkina_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=55,
                      sex.lab="women", weights.name = "sample_weight",
                      output.name = "burkina_wom_zinc")

burkina_zinc_w <- subset(burkina_zinc_w, select = c(age, HI))
burkina_zinc_w <- burkina_zinc_w[order(burkina_zinc_w$age),]

write.csv(burkina_zinc_w, "all_intakes/burkina_w_zinc.csv")

# Men
burkina_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=burkina_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="men", weights.name = "sample_weight",
                      output.name = "burkina_men_zinc")

burkina_zinc_m <- subset(burkina_zinc_m, select = c(age, HI))
burkina_zinc_m <- burkina_zinc_m[order(burkina_zinc_m$age),]

write.csv(burkina_zinc_m, "all_intakes/burkina_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

burkina_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=55,
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_vita")

burkina_vita <- subset(burkina_vita, select = c(age, HI))
burkina_vita <- burkina_vita[order(burkina_vita$age),]

write.csv(burkina_vita, "all_intakes/burkina_w_vita.csv")

# Men
burkina_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=burkina_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men", weights.name = "sample_weight",
                    output.name = "burkina_men_vita")

burkina_vita <- subset(burkina_vita, select = c(age, HI))
burkina_vita <- burkina_vita[order(burkina_vita$age),]

write.csv(burkina_vita, "all_intakes/burkina_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

burkina_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=55,
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_calc")

burkina_calc <- subset(burkina_calc, select = c(age, HI))
burkina_calc <- burkina_calc[order(burkina_calc$age),]

write.csv(burkina_calc, "all_intakes/burkina_w_calc.csv")

# Men
burkina_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=burkina_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men", weights.name = "sample_weight",
                    output.name = "burkina_men_calc")

burkina_calc <- subset(burkina_calc, select = c(age, HI))
burkina_calc <- burkina_calc[order(burkina_calc$age),]

write.csv(burkina_calc, "all_intakes/burkina_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR OMEGA 3 


burkina_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=burkina_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=55,
                       sex.lab="women", weights.name = "sample_weight",
                       output.name = "burkina_wom_omega_3")

burkina_omega_3 <- subset(burkina_omega_3, select = c(age, HI))
burkina_omega_3 <- burkina_omega_3[order(burkina_omega_3$age),]

write.csv(burkina_omega_3, "all_intakes/burkina_w_omega_3.csv")

# Men
burkina_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=burkina_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men", weights.name = "sample_weight",
                       output.name = "burkina_men_omega_3")

burkina_omega_3 <- subset(burkina_omega_3, select = c(age, HI))
burkina_omega_3 <- burkina_omega_3[order(burkina_omega_3$age),]

write.csv(burkina_omega_3, "all_intakes/burkina_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR Vitamin C

burkina_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_vitc")

burkina_vitc <- subset(burkina_vitc, select = c(age, HI))
burkina_vitc <- burkina_vitc[order(burkina_vitc$age),]

write.csv(burkina_vitc, "all_intakes/burkina_w_vitc.csv")

# Men
burkina_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_vitc")

burkina_vitc <- subset(burkina_vitc, select = c(age, HI))
burkina_vitc <- burkina_vitc[order(burkina_vitc$age),]

write.csv(burkina_vitc, "all_intakes/burkina_m_vitc.csv")

##################################################################

# 8. RUN SPADE FOR THIAMIN

burkina_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_thia")

burkina_thia <- subset(burkina_thia, select = c(age, HI))
burkina_thia <- burkina_thia[order(burkina_thia$age),]

write.csv(burkina_thia, "all_intakes/burkina_w_thia.csv")

# Men
burkina_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_thia")

burkina_thia <- subset(burkina_thia, select = c(age, HI))
burkina_thia <- burkina_thia[order(burkina_thia$age),]

write.csv(burkina_thia, "all_intakes/burkina_m_thia.csv")

##################################################################

# 8. RUN SPADE FOR RIBOFLAVIN

burkina_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_ribo")

burkina_ribo <- subset(burkina_ribo, select = c(age, HI))
burkina_ribo <- burkina_ribo[order(burkina_ribo$age),]

write.csv(burkina_ribo, "all_intakes/burkina_w_ribo.csv")

# Men
burkina_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_ribo")

burkina_ribo <- subset(burkina_ribo, select = c(age, HI))
burkina_ribo <- burkina_ribo[order(burkina_ribo$age),]

write.csv(burkina_ribo, "all_intakes/burkina_m_ribo.csv")

##################################################################

# 9. RUN SPADE FOR NIACIN

burkina_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_niac")

burkina_niac <- subset(burkina_niac, select = c(age, HI))
burkina_niac <- burkina_niac[order(burkina_niac$age),]

write.csv(burkina_niac, "all_intakes/burkina_w_niac.csv")

# Men
burkina_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_niac")

burkina_niac <- subset(burkina_niac, select = c(age, HI))
burkina_niac <- burkina_niac[order(burkina_niac$age),]

write.csv(burkina_niac, "all_intakes/burkina_m_niac.csv")

##################################################################

# 10. RUN SPADE FOR VITAMIN B6

burkina_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_vitb6")

burkina_vitb6 <- subset(burkina_vitb6, select = c(age, HI))
burkina_vitb6 <- burkina_vitb6[order(burkina_vitb6$age),]

write.csv(burkina_vitb6, "all_intakes/burkina_w_vitb6.csv")

# Men
burkina_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_vitb6")

burkina_vitb6 <- subset(burkina_vitb6, select = c(age, HI))
burkina_vitb6 <- burkina_vitb6[order(burkina_vitb6$age),]

write.csv(burkina_vitb6, "all_intakes/burkina_m_vitb6.csv")

##################################################################

# 11. RUN SPADE FOR FOLATE

burkina_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                         data=burkina_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=55,
                         sex.lab="women", weights.name = "sample_weight",
                         output.name = "burkina_wom_fola")

burkina_fola <- subset(burkina_fola, select = c(age, HI))
burkina_fola <- burkina_fola[order(burkina_fola$age),]

write.csv(burkina_fola, "all_intakes/burkina_w_fola.csv")

# Men
burkina_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                         data=burkina_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=4,
                         sex.lab="men", weights.name = "sample_weight",
                         output.name = "burkina_men_fola")

burkina_fola <- subset(burkina_fola, select = c(age, HI))
burkina_fola <- burkina_fola[order(burkina_fola$age),]

write.csv(burkina_fola, "all_intakes/burkina_m_fola.csv")

##################################################################

# 12. RUN SPADE FOR BETA CAROTENE

burkina_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_fola")

burkina_fola <- subset(burkina_fola, select = c(age, HI))
burkina_fola <- burkina_fola[order(burkina_fola$age),]

write.csv(burkina_fola, "all_intakes/burkina_w_fola.csv")

# Men
burkina_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_fola")

burkina_fola <- subset(burkina_fola, select = c(age, HI))
burkina_fola <- burkina_fola[order(burkina_fola$age),]

write.csv(burkina_fola, "all_intakes/burkina_m_fola.csv")

##################################################################

# 13. RUN SPADE FOR ENERGY

burkina_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=55,
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_energy")

burkina_energy <- subset(burkina_energy, select = c(age, HI))
burkina_energy <- burkina_energy[order(burkina_energy$age),]

write.csv(burkina_energy, "all_intakes/burkina_w_energy.csv")

# Men
burkina_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                        data=burkina_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men", weights.name = "sample_weight",
                        output.name = "burkina_men_energy")

burkina_energy <- subset(burkina_energy, select = c(age, HI))
burkina_energy <- burkina_energy[order(burkina_energy$age),]

write.csv(burkina_energy, "all_intakes/burkina_m_energy.csv")

##################################################################

# 14. RUN SPADE FOR ENERGY

burkina_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=burkina_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=55,
                          sex.lab="women", weights.name = "sample_weight",
                          output.name = "burkina_wom_protein")

burkina_protein <- subset(burkina_protein, select = c(age, HI))
burkina_protein <- burkina_protein[order(burkina_protein$age),]

write.csv(burkina_protein, "all_intakes/burkina_w_protein.csv")

# Men
burkina_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                          data=burkina_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="men", weights.name = "sample_weight",
                          output.name = "burkina_men_protein")

burkina_protein <- subset(burkina_protein, select = c(age, HI))
burkina_protein <- burkina_protein[order(burkina_protein$age),]

write.csv(burkina_protein, "all_intakes/burkina_m_protein.csv")

##################################################################

# 15. RUN SPADE FOR FAT

burkina_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                          data=burkina_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=55,
                          sex.lab="women", weights.name = "sample_weight",
                          output.name = "burkina_wom_fat")

burkina_fat <- subset(burkina_fat, select = c(age, HI))
burkina_fat <- burkina_fat[order(burkina_fat$age),]

write.csv(burkina_fat, "all_intakes/burkina_w_fat.csv")

# Men
burkina_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                          data=burkina_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="men", weights.name = "sample_weight",
                          output.name = "burkina_men_fat")

burkina_fat <- subset(burkina_fat, select = c(age, HI))
burkina_fat <- burkina_fat[order(burkina_fat$age),]

write.csv(burkina_fat, "all_intakes/burkina_m_fat.csv")

##################################################################

# 16. RUN SPADE FOR CARBOHYDRATE

burkina_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=burkina_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=55,
                       sex.lab="women", weights.name = "sample_weight",
                       output.name = "burkina_wom_carb")

burkina_carb <- subset(burkina_carb, select = c(age, HI))
burkina_carb <- burkina_carb[order(burkina_carb$age),]

write.csv(burkina_carb, "all_intakes/burkina_w_carb.csv")

# Men
burkina_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                       data=burkina_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men", weights.name = "sample_weight",
                       output.name = "burkina_men_carb")

burkina_carb <- subset(burkina_carb, select = c(age, HI))
burkina_carb <- burkina_carb[order(burkina_carb$age),]

write.csv(burkina_carb, "all_intakes/burkina_m_carb.csv")