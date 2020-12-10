# Running SPADE: rom data
# File created on 12/7/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3
# There is no zinc for rom

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "rom"))
SPADE.OUTPUT.PATH <- (here("output", "rom"))

###########################################################
# Remove missing obs
summary(rom_spade)

# Make separate datasets for men and women
rom_wom <- subset(rom_spade, sex==2)
rom_men <- subset(rom_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(rom_wom$age)
range(rom_men$age)

#round the age variables down
rom_wom$age <- floor(rom_wom$age)
rom_men$age <- floor(rom_men$age)

# number of intakes per person:
table(rom_wom$age)

# Women
rom_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                     data=rom_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=92,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="women", 
                     output.name = "rom_wom_b12")

rom_b12 <- subset(rom_b12, select = c(age, HI))
rom_b12 <- rom_b12[order(rom_b12$age),]

write.csv(rom_b12, "all_intakes/rom_w_b12.csv")

# Men
rom_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                     data=rom_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=88,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="men",
                     output.name = "rom_men_b12")

rom_b12 <- subset(rom_b12, select = c(age, HI))
rom_b12 <- rom_b12[order(rom_b12$age),]

write.csv(rom_b12, "all_intakes/rom_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
rom_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "rom_wom_iron",
                      spade.output.path = "output/rom/")

rom_iron <- subset(rom_iron, select = c(age, HI))
rom_iron <- rom_iron[order(rom_iron$age),]

write.csv(rom_iron, "all_intakes/rom_w_iron.csv")

# Men
rom_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      output.name = "rom_men_iron",
                      spade.output.path = "output/rom/")

rom_iron <- subset(rom_iron, select = c(age, HI))
rom_iron <- rom_iron[order(rom_iron$age),]

write.csv(rom_iron, "all_intakes/rom_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
 rom_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=rom_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=92,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "rom_wom_zinc",
                       spade.output.path = "output/rom/")
 
 rom_zinc_w <- subset(rom_zinc_w, select = c(age, HI))
 rom_zinc_w <- rom_zinc_w[order(rom_zinc_w$age),]
 
 write.csv(rom_zinc_w, "all_intakes/rom_w_zinc.csv")
 
#  Men
 rom_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=rom_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       output.name = "rom_men_zinc",
                       spade.output.path = "output/rom/")
 
 rom_zinc_m <- subset(rom_zinc_m, select = c(age, HI))
 rom_zinc_m <- rom_zinc_m[order(rom_zinc_m$age),]
 
 write.csv(rom_zinc_m, "all_intakes/rom_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

rom_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "rom_wom_vita",
                      spade.output.path = "output/rom/")

rom_vita <- subset(rom_vita, select = c(age, HI))
rom_vita <- rom_vita[order(rom_vita$age),]

write.csv(rom_vita, "all_intakes/rom_w_vita.csv")

# Men
rom_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      output.name = "rom_men_vita",
                      spade.output.path = "output/rom/")

rom_vita <- subset(rom_vita, select = c(age, HI))
rom_vita <- rom_vita[order(rom_vita$age),]

write.csv(rom_vita, "all_intakes/rom_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

rom_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      
                      output.name = "rom_wom_calc",
                      spade.output.path = "output/rom/")

rom_calc <- subset(rom_calc, select = c(age, HI))
rom_calc <- rom_calc[order(rom_calc$age),]

write.csv(rom_calc, "all_intakes/rom_w_calc.csv")

# Men
rom_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      
                      output.name = "rom_men_calc",
                      spade.output.path = "output/rom/")

rom_calc <- subset(rom_calc, select = c(age, HI))
rom_calc <- rom_calc[order(rom_calc$age),]

write.csv(rom_calc, "all_intakes/rom_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

rom_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                          data=rom_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=92,
                          age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                          sex.lab="women",
                          output.name = "rom_wom_red_meat",
                          spade.output.path = "output/rom/")

rom_red_meat <- subset(rom_red_meat, select = c(age, HI))
rom_red_meat <- rom_red_meat[order(rom_red_meat$age),]

write.csv(rom_red_meat, "all_intakes/rom_w_red_meat.csv")

# Men
rom_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                          data=rom_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=19, max.age=88,
                          age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                          sex.lab="men",
                          
                          output.name = "rom_men_red_meat",
                          spade.output.path = "output/rom/")

rom_red_meat <- subset(rom_red_meat, select = c(age, HI))
rom_red_meat <- rom_red_meat[order(rom_red_meat$age),]

write.csv(rom_red_meat, "all_intakes/rom_m_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR OMEGA 3 


rom_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                         data=rom_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=92,
                         age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                         sex.lab="women",
                         output.name = "rom_wom_omega_3",
                         spade.output.path = "output/rom/")

rom_omega_3 <- subset(rom_omega_3, select = c(age, HI))
rom_omega_3 <- rom_omega_3[order(rom_omega_3$age),]

write.csv(rom_omega_3, "all_intakes/rom_w_omega_3.csv")

# Men
rom_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                         data=rom_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                         sex.lab="men",
                         
                         output.name = "rom_men_omega_3",
                         spade.output.path = "output/rom/")

rom_omega_3 <- subset(rom_omega_3, select = c(age, HI))
rom_omega_3 <- rom_omega_3[order(rom_omega_3$age),]

write.csv(rom_omega_3, "all_intakes/rom_m_omega_3.csv")