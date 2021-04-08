# Running SPADE: mexico data
# File created on 11/24/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "mexico"))
SPADE.OUTPUT.PATH <- (here("output", "mexico"))
# TOTAL.output <- (here("output", "mexico", 2_logbooks"))
###########################################################
# Remove missing obs
summary(mexico_spade)
mexico_spade <- na.omit(mexico_spade)
summary(mexico_spade)

#remove obs with weights=0
mexico_spade <- subset(mexico_spade, weight !=0 & vita<10000 )
#removed 6 observations with very high meat values

# Make separate datasets for men and women
mexico_wom <- subset(mexico_spade, sex==2 ) 
mexico_men <- subset(mexico_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(mexico_wom$age)
range(mexico_men$age)

# number of intakes per person:
table(mexico_wom$age)

# Women
mexico_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=mexico_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=97,
                   sex.lab="women",
                   weights.name = "weight",
                   output.name = "mexico_wom_b12")

mexico_b12 <- subset(mexico_b12, select = c(age, HI))
mexico_b12 <- mexico_b12[order(mexico_b12$age),]

write.csv(mexico_b12, "all_intakes/mexico_w_b12.csv")

# Men
mexico_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=mexico_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=89,
                   sex.lab="men",
                   weights.name = "weight",
                   output.name = "mexico_men_b12")

mexico_b12 <- subset(mexico_b12, select = c(age, HI))
mexico_b12 <- mexico_b12[order(mexico_b12$age),]

write.csv(mexico_b12, "all_intakes/mexico_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
mexico_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=mexico_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=97,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "mexico_wom_iron",
                    spade.output.path = "output/mexico/")

mexico_iron <- subset(mexico_iron, select = c(age, HI))
mexico_iron <- mexico_iron[order(mexico_iron$age),]

write.csv(mexico_iron, "all_intakes/mexico_w_iron.csv")

# Men
mexico_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=mexico_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=89,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_iron")

mexico_iron <- subset(mexico_iron, select = c(age, HI))
mexico_iron <- mexico_iron[order(mexico_iron$age),]

write.csv(mexico_iron, "all_intakes/mexico_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
mexico_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=mexico_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=97,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "mexico_wom_zinc")

mexico_zinc_w <- subset(mexico_zinc_w, select = c(age, HI))
mexico_zinc_w <- mexico_zinc_w[order(mexico_zinc_w$age),]

write.csv(mexico_zinc_w, "all_intakes/mexico_w_zinc.csv")

# Men
mexico_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=mexico_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=89,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "mexico_men_zinc")

mexico_zinc_m <- subset(mexico_zinc_m, select = c(age, HI))
mexico_zinc_m <- mexico_zinc_m[order(mexico_zinc_m$age),]

write.csv(mexico_zinc_m, "all_intakes/mexico_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

mexico_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=mexico_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=97,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "mexico_wom_vita")

mexico_vita <- subset(mexico_vita, select = c(age, HI))
mexico_vita <- mexico_vita[order(mexico_vita$age),]

write.csv(mexico_vita, "all_intakes/mexico_w_vita.csv")

# Men
mexico_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=mexico_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=89,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_vita")

mexico_vita <- subset(mexico_vita, select = c(age, HI))
mexico_vita <- mexico_vita[order(mexico_vita$age),]

write.csv(mexico_vita, "all_intakes/mexico_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

mexico_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=mexico_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=97,
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "mexico_wom_calc")

mexico_calc <- subset(mexico_calc, select = c(age, HI))
mexico_calc <- mexico_calc[order(mexico_calc$age),]

write.csv(mexico_calc, "all_intakes/mexico_w_calc.csv")

# Men
mexico_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=mexico_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=89,
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_calc")

mexico_calc <- subset(mexico_calc, select = c(age, HI))
mexico_calc <- mexico_calc[order(mexico_calc$age),]

write.csv(mexico_calc, "all_intakes/mexico_m_calc.csv")

##################################################################
# 6. RUN SPADE FOR OMEGA 3 
mexico_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_omega_3")

mexico_omega_3 <- subset(mexico_omega_3, select = c(age, HI))
mexico_omega_3 <- mexico_omega_3[order(mexico_omega_3$age),]

write.csv(mexico_omega_3, "all_intakes/mexico_w_omega_3.csv")

# Men
mexico_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_omega_3")

mexico_omega_3 <- subset(mexico_omega_3, select = c(age, HI))
mexico_omega_3 <- mexico_omega_3[order(mexico_omega_3$age),]

write.csv(mexico_omega_3, "all_intakes/mexico_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR ENERGY

mexico_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_energy")

mexico_energy <- subset(mexico_energy, select = c(age, HI))
mexico_energy <- mexico_energy[order(mexico_energy$age),]

write.csv(mexico_energy, "all_intakes/mexico_w_energy.csv")

# Men
mexico_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_energy")

mexico_energy <- subset(mexico_energy, select = c(age, HI))
mexico_energy <- mexico_energy[order(mexico_energy$age),]

write.csv(mexico_energy, "all_intakes/mexico_m_energy.csv")

##################################################################

# 8. RUN SPADE FOR CARB

mexico_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                         data=mexico_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=97,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "mexico_wom_carb")

mexico_carb <- subset(mexico_carb, select = c(age, HI))
mexico_carb <- mexico_carb[order(mexico_carb$age),]

write.csv(mexico_carb, "all_intakes/mexico_w_carb.csv")

# Men
mexico_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                         data=mexico_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=89,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "mexico_men_carb")

mexico_carb <- subset(mexico_carb, select = c(age, HI))
mexico_carb <- mexico_carb[order(mexico_carb$age),]

write.csv(mexico_carb, "all_intakes/mexico_m_carb.csv")

##################################################################

# 9. RUN SPADE FOR FAT

mexico_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_fat")

mexico_fat <- subset(mexico_fat, select = c(age, HI))
mexico_fat <- mexico_fat[order(mexico_fat$age),]

write.csv(mexico_fat, "all_intakes/mexico_w_fat.csv")

# Men
mexico_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_fat")

mexico_fat <- subset(mexico_fat, select = c(age, HI))
mexico_fat <- mexico_fat[order(mexico_fat$age),]

write.csv(mexico_fat, "all_intakes/mexico_m_fat.csv")

##################################################################

# 10. RUN SPADE FOR FIBER

mexico_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                      data=mexico_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=97,
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "mexico_wom_fiber")

mexico_fiber <- subset(mexico_fiber, select = c(age, HI))
mexico_fiber <- mexico_fiber[order(mexico_fiber$age),]

write.csv(mexico_fiber, "all_intakes/mexico_w_fiber.csv")

# Men
mexico_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                      data=mexico_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=89,
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "mexico_men_fiber")

mexico_fiber <- subset(mexico_fiber, select = c(age, HI))
mexico_fiber <- mexico_fiber[order(mexico_fiber$age),]

write.csv(mexico_fiber, "all_intakes/mexico_m_fiber.csv")

##################################################################

# 11. RUN SPADE FOR PROTEIN

mexico_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                        data=mexico_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=97,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "mexico_wom_protein")

mexico_protein <- subset(mexico_protein, select = c(age, HI))
mexico_protein <- mexico_protein[order(mexico_protein$age),]

write.csv(mexico_protein, "all_intakes/mexico_w_protein.csv")

# Men
mexico_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                        data=mexico_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=89,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "mexico_men_protein")

mexico_protein <- subset(mexico_protein, select = c(age, HI))
mexico_protein <- mexico_protein[order(mexico_protein$age),]

write.csv(mexico_protein, "all_intakes/mexico_m_protein.csv")

##################################################################

# 12. RUN SPADE FOR SUGAR

mexico_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                          data=mexico_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=97,
                          sex.lab="women",
                          weights.name = "weight",
                          output.name = "mexico_wom_sugar")

mexico_sugar <- subset(mexico_sugar, select = c(age, HI))
mexico_sugar <- mexico_sugar[order(mexico_sugar$age),]

write.csv(mexico_sugar, "all_intakes/mexico_w_sugar.csv")

# Men
mexico_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                          data=mexico_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=89,
                          sex.lab="men",
                          weights.name = "weight",
                          output.name = "mexico_men_sugar")

mexico_sugar <- subset(mexico_sugar, select = c(age, HI))
mexico_sugar <- mexico_sugar[order(mexico_sugar$age),]

write.csv(mexico_sugar, "all_intakes/mexico_m_sugar.csv")

##################################################################

# 13. RUN SPADE FOR MG

mexico_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                        data=mexico_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=97,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "mexico_wom_mg")

mexico_mg <- subset(mexico_mg, select = c(age, HI))
mexico_mg <- mexico_mg[order(mexico_mg$age),]

write.csv(mexico_mg, "all_intakes/mexico_w_mg.csv")

# Men
mexico_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                        data=mexico_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=89,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "mexico_men_mg")

mexico_mg <- subset(mexico_mg, select = c(age, HI))
mexico_mg <- mexico_mg[order(mexico_mg$age),]

write.csv(mexico_mg, "all_intakes/mexico_m_mg.csv")

##################################################################

# 14. RUN SPADE FOR PHOSPHORUS

mexico_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                        data=mexico_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=97,
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "mexico_wom_phos")

mexico_phos <- subset(mexico_phos, select = c(age, HI))
mexico_phos <- mexico_phos[order(mexico_phos$age),]

write.csv(mexico_phos, "all_intakes/mexico_w_phos.csv")

# Men
mexico_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                        data=mexico_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=89,
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "mexico_men_phos")

mexico_phos <- subset(mexico_phos, select = c(age, HI))
mexico_phos <- mexico_phos[order(mexico_phos$age),]

write.csv(mexico_phos, "all_intakes/mexico_m_phos.csv")

##################################################################

# 15. RUN SPADE FOR POTASSIUM

mexico_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_pota")

mexico_pota <- subset(mexico_pota, select = c(age, HI))
mexico_pota <- mexico_pota[order(mexico_pota$age),]

write.csv(mexico_pota, "all_intakes/mexico_w_pota.csv")

# Men
mexico_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_pota")

mexico_pota <- subset(mexico_pota, select = c(age, HI))
mexico_pota <- mexico_pota[order(mexico_pota$age),]

write.csv(mexico_pota, "all_intakes/mexico_m_pota.csv")

##################################################################

# 16. RUN SPADE FOR SODIUM

mexico_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_na")

mexico_na <- subset(mexico_na, select = c(age, HI))
mexico_na <- mexico_na[order(mexico_na$age),]

write.csv(mexico_na, "all_intakes/mexico_w_na.csv")

# Men
mexico_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_na")

mexico_na <- subset(mexico_na, select = c(age, HI))
mexico_na <- mexico_na[order(mexico_na$age),]

write.csv(mexico_na, "all_intakes/mexico_m_na.csv")

##################################################################

# 17. RUN SPADE FOR COPPER

mexico_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_cu")

mexico_cu <- subset(mexico_cu, select = c(age, HI))
mexico_cu <- mexico_cu[order(mexico_cu$age),]

write.csv(mexico_cu, "all_intakes/mexico_w_cu.csv")

# Men
mexico_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_cu")

mexico_cu <- subset(mexico_cu, select = c(age, HI))
mexico_cu <- mexico_cu[order(mexico_cu$age),]

write.csv(mexico_cu, "all_intakes/mexico_m_cu.csv")

##################################################################

# 17. RUN SPADE FOR MANGANESE

mexico_mang <- f.spade(frml.ia=mang~fp(age), frml.if="no.if", 
                     data=mexico_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=97,
                     sex.lab="women",
                     weights.name = "weight",
                     output.name = "mexico_wom_mang")

mexico_mang <- subset(mexico_mang, select = c(age, HI))
mexico_mang <- mexico_mang[order(mexico_mang$age),]

write.csv(mexico_mang, "all_intakes/mexico_w_mang.csv")

# Men
mexico_mang <- f.spade(frml.ia=mang~fp(age), frml.if="no.if", 
                     data=mexico_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=89,
                     sex.lab="men",
                     weights.name = "weight",
                     output.name = "mexico_men_mang")

mexico_mang <- subset(mexico_mang, select = c(age, HI))
mexico_mang <- mexico_mang[order(mexico_mang$age),]

write.csv(mexico_mang, "all_intakes/mexico_m_mang.csv")

##################################################################

# 18. RUN SPADE FOR SELENIUM

mexico_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_se")

mexico_se <- subset(mexico_se, select = c(age, HI))
mexico_se <- mexico_se[order(mexico_se$age),]

write.csv(mexico_se, "all_intakes/mexico_w_se.csv")

# Men
mexico_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_se")

mexico_se <- subset(mexico_se, select = c(age, HI))
mexico_se <- mexico_se[order(mexico_se$age),]

write.csv(mexico_se, "all_intakes/mexico_m_se.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN C

mexico_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_vitc")

mexico_vitc <- subset(mexico_vitc, select = c(age, HI))
mexico_vitc <- mexico_vitc[order(mexico_vitc$age),]

write.csv(mexico_vitc, "all_intakes/mexico_w_vitc.csv")

# Men
mexico_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_vitc")

mexico_vitc <- subset(mexico_vitc, select = c(age, HI))
mexico_vitc <- mexico_vitc[order(mexico_vitc$age),]

write.csv(mexico_vitc, "all_intakes/mexico_m_vitc.csv")

##################################################################

# 20. RUN SPADE FOR THIAMIN

mexico_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_thia")

mexico_thia <- subset(mexico_thia, select = c(age, HI))
mexico_thia <- mexico_thia[order(mexico_thia$age),]

write.csv(mexico_thia, "all_intakes/mexico_w_thia.csv")

# Men
mexico_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_thia")

mexico_thia <- subset(mexico_thia, select = c(age, HI))
mexico_thia <- mexico_thia[order(mexico_thia$age),]

write.csv(mexico_thia, "all_intakes/mexico_m_thia.csv")

##################################################################

# 21. RUN SPADE FOR RIBOFLAVIN

mexico_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_ribo")

mexico_ribo <- subset(mexico_ribo, select = c(age, HI))
mexico_ribo <- mexico_ribo[order(mexico_ribo$age),]

write.csv(mexico_ribo, "all_intakes/mexico_w_ribo.csv")

# Men
mexico_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_ribo")

mexico_ribo <- subset(mexico_ribo, select = c(age, HI))
mexico_ribo <- mexico_ribo[order(mexico_ribo$age),]

write.csv(mexico_ribo, "all_intakes/mexico_m_ribo.csv")

##################################################################

# 22. RUN SPADE FOR NIACIN

mexico_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_niac")

mexico_niac <- subset(mexico_niac, select = c(age, HI))
mexico_niac <- mexico_niac[order(mexico_niac$age),]

write.csv(mexico_niac, "all_intakes/mexico_w_niac.csv")

# Men
mexico_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_niac")

mexico_niac <- subset(mexico_niac, select = c(age, HI))
mexico_niac <- mexico_niac[order(mexico_niac$age),]

write.csv(mexico_niac, "all_intakes/mexico_m_niac.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN B6

mexico_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                         data=mexico_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=97,
                         sex.lab="women",
                         weights.name = "weight",
                         output.name = "mexico_wom_vitb6")

mexico_vitb6 <- subset(mexico_vitb6, select = c(age, HI))
mexico_vitb6 <- mexico_vitb6[order(mexico_vitb6$age),]

write.csv(mexico_vitb6, "all_intakes/mexico_w_vitb6.csv")

# Men
mexico_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                         data=mexico_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=1, max.age=89,
                         sex.lab="men",
                         weights.name = "weight",
                         output.name = "mexico_men_vitb6")

mexico_vitb6 <- subset(mexico_vitb6, select = c(age, HI))
mexico_vitb6 <- mexico_vitb6[order(mexico_vitb6$age),]

write.csv(mexico_vitb6, "all_intakes/mexico_m_vitb6.csv")

##################################################################

# 23. RUN SPADE FOR FOLATE

mexico_fola <- f.spade(frml.ia=fola~fp(age), frml.if=fola~cs(age),
                          data=mexico_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=97,
                          sex.lab="women",
                          weights.name = "weight",
                          output.name = "mexico_wom_fola")

mexico_fola <- subset(mexico_fola, select = c(age, HI))
mexico_fola <- mexico_fola[order(mexico_fola$age),]

write.csv(mexico_fola, "all_intakes/mexico_w_fola.csv")

# Men
mexico_fola <- f.spade(frml.ia=fola~fp(age), frml.if=fola~cs(age),
                          data=mexico_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=89,
                          sex.lab="men",
                          weights.name = "weight",
                          output.name = "mexico_men_fola")

mexico_fola <- subset(mexico_fola, select = c(age, HI))
mexico_fola <- mexico_fola[order(mexico_fola$age),]

write.csv(mexico_fola, "all_intakes/mexico_m_fola.csv")

##################################################################

# 25. RUN SPADE FOR VITAMIN K

mexico_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_vitk")

mexico_vitk <- subset(mexico_vitk, select = c(age, HI))
mexico_vitk <- mexico_vitk[order(mexico_vitk$age),]

write.csv(mexico_vitk, "all_intakes/mexico_w_vitk.csv")

# Men
mexico_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_vitk")

mexico_vitk <- subset(mexico_vitk, select = c(age, HI))
mexico_vitk <- mexico_vitk[order(mexico_vitk$age),]

write.csv(mexico_vitk, "all_intakes/mexico_m_vitk.csv")

##################################################################

# 26. RUN SPADE FOR VITAMIN E

mexico_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_vite")

mexico_vite <- subset(mexico_vite, select = c(age, HI))
mexico_vite <- mexico_vite[order(mexico_vite$age),]

write.csv(mexico_vite, "all_intakes/mexico_w_vite.csv")

# Men
mexico_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_vite")

mexico_vite <- subset(mexico_vite, select = c(age, HI))
mexico_vite <- mexico_vite[order(mexico_vite$age),]

write.csv(mexico_vite, "all_intakes/mexico_m_vite.csv")

##################################################################

# 27. RUN SPADE FOR VITAMIN D

mexico_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_vitd")

mexico_vitd <- subset(mexico_vitd, select = c(age, HI))
mexico_vitd <- mexico_vitd[order(mexico_vitd$age),]

write.csv(mexico_vitd, "all_intakes/mexico_w_vitd.csv")

# Men
mexico_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_vitd")

mexico_vitd <- subset(mexico_vitd, select = c(age, HI))
mexico_vitd <- mexico_vitd[order(mexico_vitd$age),]

write.csv(mexico_vitd, "all_intakes/mexico_m_vitd.csv")

##################################################################

# 28. RUN SPADE FOR BETA CAROTENE

mexico_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_betacarot")

mexico_betacarot <- subset(mexico_betacarot, select = c(age, HI))
mexico_betacarot <- mexico_betacarot[order(mexico_betacarot$age),]

write.csv(mexico_betacarot, "all_intakes/mexico_w_betacarot.csv")

# Men
mexico_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_betacarot")

mexico_betacarot <- subset(mexico_betacarot, select = c(age, HI))
mexico_betacarot <- mexico_betacarot[order(mexico_betacarot$age),]

write.csv(mexico_betacarot, "all_intakes/mexico_m_betacarot.csv")

##################################################################

# 29. RUN SPADE FOR SATURATED FAT

mexico_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_satfat")

mexico_satfat <- subset(mexico_satfat, select = c(age, HI))
mexico_satfat <- mexico_satfat[order(mexico_satfat$age),]

write.csv(mexico_satfat, "all_intakes/mexico_w_satfat.csv")

# Men
mexico_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_satfat")

mexico_satfat <- subset(mexico_satfat, select = c(age, HI))
mexico_satfat <- mexico_satfat[order(mexico_satfat$age),]

write.csv(mexico_satfat, "all_intakes/mexico_m_satfat.csv")

##################################################################

# 30. RUN SPADE FOR MUFA

mexico_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_mufa")

mexico_mufa <- subset(mexico_mufa, select = c(age, HI))
mexico_mufa <- mexico_mufa[order(mexico_mufa$age),]

write.csv(mexico_mufa, "all_intakes/mexico_w_mufa.csv")

# Men
mexico_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_mufa")

mexico_mufa <- subset(mexico_mufa, select = c(age, HI))
mexico_mufa <- mexico_mufa[order(mexico_mufa$age),]

write.csv(mexico_mufa, "all_intakes/mexico_m_mufa.csv")

##################################################################

# 31. RUN SPADE FOR PUFA
mexico_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_pufa")

mexico_pufa <- subset(mexico_pufa, select = c(age, HI))
mexico_pufa <- mexico_pufa[order(mexico_pufa$age),]

write.csv(mexico_pufa, "all_intakes/mexico_w_pufa.csv")

# Men
mexico_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_pufa")

mexico_pufa <- subset(mexico_pufa, select = c(age, HI))
mexico_pufa <- mexico_pufa[order(mexico_pufa$age),]

write.csv(mexico_pufa, "all_intakes/mexico_m_pufa.csv")