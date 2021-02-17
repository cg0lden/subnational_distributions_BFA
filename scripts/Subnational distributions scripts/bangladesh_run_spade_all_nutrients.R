# Running SPADE: bangldadesh data
# File created on 12/15/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "bangladesh"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Bangladesh"))

###########################################################
# Remove missing obs
summary(bangladesh_spade)

# Make separate datasets for men and women
bang_wom <- subset(bangladesh_spade, sex==2 )

# No men in this dataset
###########################################################
# 1. RUN SPADE FOR B12

range(bang_wom$age)
summary(bang_wom)
#check the two missing values of age, 427 and 434 mday 1

# number of intakes per person:
table(bang_wom$age)

#have to run as two part model because so many zeroes
# Women
bang_b12 <- f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age),
                       data=bang_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=16, max.age=70,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women", 
                       output.name = "bang_wom_b12")

bang_b12 <- subset(bang_b12, select = c(age, HI))
bang_b12 <- bang_b12[order(bang_b12$age),]

write.csv(bang_b12, "all_intakes/bang_w_b12.csv")


##################################################################

# 2. RUN SPADE FOR IRON
# Women
bang_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                        data=bang_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=16, max.age=70,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "bang_wom_iron")

bang_iron <- subset(bang_iron, select = c(age, HI))
bang_iron <- bang_iron[order(bang_iron$age),]

write.csv(bang_iron, "all_intakes/bang_w_iron.csv")

##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
bang_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                          data=bang_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=16, max.age=70,
                          age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                          sex.lab="women",
                          output.name = "bang_wom_zinc")

bang_zinc_w <- subset(bang_zinc_w, select = c(age, HI))
bang_zinc_w <- bang_zinc_w[order(bang_zinc_w$age),]

write.csv(bang_zinc_w, "all_intakes/bang_w_zinc.csv")


##################################################################

# 4. RUN SPADE FOR VIT A

bang_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                        data=bang_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=16, max.age=70,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "bang_wom_vita")

bang_vita <- subset(bang_vita, select = c(age, HI))
bang_vita <- bang_vita[order(bang_vita$age),]

write.csv(bang_vita, "all_intakes/bang_w_vita.csv")


##################################################################

# 5. RUN SPADE FOR CALCIUM

bang_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                        data=bang_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=16, max.age=70,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        
                        output.name = "bang_wom_calc")

bang_calc <- subset(bang_calc, select = c(age, HI))
bang_calc <- bang_calc[order(bang_calc$age),]

write.csv(bang_calc, "all_intakes/bang_w_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 


bang_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                           data=bang_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=16, max.age=70,
                           age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                           sex.lab="women",
                           output.name = "bang_wom_omega_3")

bang_omega_3 <- subset(bang_omega_3, select = c(age, HI))
bang_omega_3 <- bang_omega_3[order(bang_omega_3$age),]

write.csv(bang_omega_3, "all_intakes/bang_w_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITC

bang_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_vitc")

bang_vitc <- subset(bang_vitc, select = c(age, HI))
bang_vitc <- bang_vitc[order(bang_vitc$age),]

write.csv(bang_vitc, "all_intakes/bang_w_vitc.csv")

##################################################################

# 8. RUN SPADE FOR THIA

bang_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_thia")

bang_thia <- subset(bang_thia, select = c(age, HI))
bang_thia <- bang_thia[order(bang_thia$age),]

write.csv(bang_thia, "all_intakes/bang_w_thia.csv")

##################################################################

# 9. RUN SPADE FOR RIBO

bang_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_ribo")

bang_ribo <- subset(bang_ribo, select = c(age, HI))
bang_ribo <- bang_ribo[order(bang_ribo$age),]

write.csv(bang_ribo, "all_intakes/bang_w_ribo.csv")

##################################################################

# 9. RUN SPADE FOR VITB6

bang_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_vitb6")

bang_vitb6 <- subset(bang_vitb6, select = c(age, HI))
bang_vitb6 <- bang_vitb6[order(bang_vitb6$age),]

write.csv(bang_vitb6, "all_intakes/bang_w_vitb6.csv")

##################################################################

# 10. RUN SPADE FOR FOLATE

bang_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                      data=bang_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=16, max.age=70,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "bang_wom_fola")

bang_fola <- subset(bang_fola, select = c(age, HI))
bang_fola <- bang_fola[order(bang_fola$age),]

write.csv(bang_fola, "all_intakes/bang_w_fola.csv")

##################################################################

# 11. RUN SPADE FOR PROTEIN

bang_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_protein")

bang_protein <- subset(bang_protein, select = c(age, HI))
bang_protein <- bang_protein[order(bang_protein$age),]

write.csv(bang_protein, "all_intakes/bang_w_protein.csv")

##################################################################

# 12. RUN SPADE FOR CARBOHYDRATES

bang_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_carb")

bang_carb <- subset(bang_carb, select = c(age, HI))
bang_carb <- bang_carb[order(bang_carb$age),]

write.csv(bang_carb, "all_intakes/bang_w_carb.csv")

##################################################################

# 13. RUN SPADE FOR FAT

bang_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                     data=bang_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=16, max.age=70,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women",
                     output.name = "bang_wom_fat")

bang_fat <- subset(bang_fat, select = c(age, HI))
bang_fat <- bang_fat[order(bang_fat$age),]

write.csv(bang_fat, "all_intakes/bang_w_fat.csv")