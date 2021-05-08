# Running SPADE: italy data
# File created on 12/7/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3
# There is no zinc for italy

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "italy"))
SPADE.OUTPUT.PATH <- (here("output", "italy"))

###########################################################
# Remove missing obs
summary(italy_spade)

# Make separate datasets for men and women
italy_wom <- subset(italy_spade, sex==2)
italy_men <- subset(italy_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(italy_wom$age)
range(italy_men$age)

#round the age variables down
italy_wom$age <- floor(italy_wom$age)
italy_men$age <- floor(italy_men$age)

# number of intakes per person:
table(italy_wom$age)

# Women
italy_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=italy_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=97,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="women", 
                   output.name = "italy_wom_b12")

italy_b12 <- subset(italy_b12, select = c(age, HI))
italy_b12 <- italy_b12[order(italy_b12$age),]

write.csv(italy_b12, "all_intakes/italy_w_b12.csv")

# Men
italy_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=italy_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=92,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="men",
                   output.name = "italy_men_b12")

italy_b12 <- subset(italy_b12, select = c(age, HI))
italy_b12 <- italy_b12[order(italy_b12$age),]

write.csv(italy_b12, "all_intakes/italy_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
italy_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "italy_wom_iron",
                    spade.output.path = "output/italy/")

italy_iron <- subset(italy_iron, select = c(age, HI))
italy_iron <- italy_iron[order(italy_iron$age),]

write.csv(italy_iron, "all_intakes/italy_w_iron.csv")

# Men
italy_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "italy_men_iron",
                    spade.output.path = "output/italy/")

italy_iron <- subset(italy_iron, select = c(age, HI))
italy_iron <- italy_iron[order(italy_iron$age),]

write.csv(italy_iron, "all_intakes/italy_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC
 italy_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "italy_wom_zinc",
                       spade.output.path = "output/italy/")
 
 italy_zinc_w <- subset(italy_zinc_w, select = c(age, HI))
 italy_zinc_w <- italy_zinc_w[order(italy_zinc_w$age),]
 
 write.csv(italy_zinc_w, "all_intakes/italy_w_zinc.csv")
 
  Men
 italy_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       output.name = "italy_men_zinc",
                       spade.output.path = "output/italy/")
 
 italy_zinc_m <- subset(italy_zinc_m, select = c(age, HI))
 italy_zinc_m <- italy_zinc_m[order(italy_zinc_m$age),]
 
 write.csv(italy_zinc_m, "all_intakes/italy_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

italy_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "italy_wom_vita",
                    spade.output.path = "output/italy/")

italy_vita <- subset(italy_vita, select = c(age, HI))
italy_vita <- italy_vita[order(italy_vita$age),]

write.csv(italy_vita, "all_intakes/italy_w_vita.csv")

# Men
italy_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "italy_men_vita",
                    spade.output.path = "output/italy/")

italy_vita <- subset(italy_vita, select = c(age, HI))
italy_vita <- italy_vita[order(italy_vita$age),]

write.csv(italy_vita, "all_intakes/italy_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    
                    output.name = "italy_wom_calc",
                    spade.output.path = "output/italy/")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_w_calc.csv")

# Men
italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    
                    output.name = "italy_men_calc",
                    spade.output.path = "output/italy/")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

italy_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=italy_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=97,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "italy_wom_red_meat",
                        spade.output.path = "output/italy/")

italy_red_meat <- subset(italy_red_meat, select = c(age, HI))
italy_red_meat <- italy_red_meat[order(italy_red_meat$age),]

write.csv(italy_red_meat, "all_intakes/italy_w_red_meat.csv")

# Men
italy_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=italy_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=92,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="men",
                        
                        output.name = "italy_men_red_meat",
                        spade.output.path = "output/italy/")

italy_red_meat <- subset(italy_red_meat, select = c(age, HI))
italy_red_meat <- italy_red_meat[order(italy_red_meat$age),]

write.csv(italy_red_meat, "all_intakes/italy_m_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR OMEGA 3 


italy_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "italy_wom_omega_3",
                       spade.output.path = "output/italy/")

italy_omega_3 <- subset(italy_omega_3, select = c(age, HI))
italy_omega_3 <- italy_omega_3[order(italy_omega_3$age),]

write.csv(italy_omega_3, "all_intakes/italy_w_omega_3.csv")

# Men
italy_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       
                       output.name = "italy_men_omega_3",
                       spade.output.path = "output/italy/")

italy_omega_3 <- subset(italy_omega_3, select = c(age, HI))
italy_omega_3 <- italy_omega_3[order(italy_omega_3$age),]

write.csv(italy_omega_3, "all_intakes/italy_m_omega_3.csv")