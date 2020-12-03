# Running SPADE: lao data
# File created on 11/24/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Lao"))
SPADE.OUTPUT.PATH <- (here("output", "Lao"))
# TOTAL.output <- (here("output", "lao", 2_logbooks"))
###########################################################
# Remove missing obs
summary(Lao_spade)

# Make separate datasets for men and women
lao_wom <- subset(lao_spade, sex==2)
lao_men <- subset(lao_spade, sex==1)

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
lao_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                      data=lao_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=82,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women", 
                      output.name = "lao_wom_b12")

lao_b12 <- subset(lao_b12, select = c(age, HI))
lao_b12 <- lao_b12[order(lao_b12$age),]

write.csv(lao_b12, "all_intakes/lao_w_b12.csv")

# Men
lao_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                      data=lao_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=89,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
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
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "lao_wom_iron",
                       spade.output.path = "output/lao/")

lao_iron <- subset(lao_iron, select = c(age, HI))
lao_iron <- lao_iron[order(lao_iron$age),]

write.csv(lao_iron, "all_intakes/lao_w_iron.csv")

# Men
lao_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       output.name = "lao_men_iron",
                       spade.output.path = "output/lao/")

lao_iron <- subset(lao_iron, select = c(age, HI))
lao_iron <- lao_iron[order(lao_iron$age),]

write.csv(lao_iron, "all_intakes/lao_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
lao_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=lao_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=0, max.age=82,
                         age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                         sex.lab="women",
                         output.name = "lao_wom_zinc",
                         spade.output.path = "output/lao/")

lao_zinc_w <- subset(lao_zinc_w, select = c(age, HI))
lao_zinc_w <- lao_zinc_w[order(lao_zinc_w$age),]

write.csv(lao_zinc_w, "all_intakes/lao_w_zinc.csv")

# Men
lao_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                         data=lao_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=0, max.age=89,
                         age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                         sex.lab="men",
                         output.name = "lao_men_zinc",
                         spade.output.path = "output/lao/")

lao_zinc_m <- subset(lao_zinc_m, select = c(age, HI))
lao_zinc_m <- lao_zinc_m[order(lao_zinc_m$age),]

write.csv(lao_zinc_m, "all_intakes/lao_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

lao_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=lao_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=82,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "lao_wom_vita",
                       spade.output.path = "output/lao/")

lao_vita <- subset(lao_vita, select = c(age, HI))
lao_vita <- lao_vita[order(lao_vita$age),]

write.csv(lao_vita, "all_intakes/lao_w_vita.csv")

# Men
lao_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       output.name = "lao_men_vita",
                       spade.output.path = "output/lao/")

lao_vita <- subset(lao_vita, select = c(age, HI))
lao_vita <- lao_vita[order(lao_vita$age),]

write.csv(lao_vita, "all_intakes/lao_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

lao_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=lao_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=82,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
 
                       output.name = "lao_wom_calc",
                       spade.output.path = "output/lao/")

lao_calc <- subset(lao_calc, select = c(age, HI))
lao_calc <- lao_calc[order(lao_calc$age),]

write.csv(lao_calc, "all_intakes/lao_w_calc.csv")

# Men
lao_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                       data=lao_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=89,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
 
                       output.name = "lao_men_calc",
                       spade.output.path = "output/lao/")

lao_calc <- subset(lao_calc, select = c(age, HI))
lao_calc <- lao_calc[order(lao_calc$age),]

write.csv(lao_calc, "all_intakes/lao_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

lao_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                           data=lao_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=0, max.age=82,
                           age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                           sex.lab="women",
                           output.name = "lao_wom_red_meat",
                           spade.output.path = "output/lao/")

lao_red_meat <- subset(lao_red_meat, select = c(age, HI))
lao_red_meat <- lao_red_meat[order(lao_red_meat$age),]

write.csv(lao_red_meat, "all_intakes/lao_w_red_meat.csv")

# Men
lao_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                           data=lao_men, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=0, max.age=89,
                           age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                           sex.lab="men",
     
                           output.name = "lao_men_red_meat",
                           spade.output.path = "output/lao/")

lao_red_meat <- subset(lao_red_meat, select = c(age, HI))
lao_red_meat <- lao_red_meat[order(lao_red_meat$age),]

write.csv(lao_red_meat, "all_intakes/lao_m_red_meat.csv")
##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

lao_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age), 
                                 data=lao_wom, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=0, max.age=82,
                                 age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                                 sex.lab="women",
           
                                 output.name = "lao_wom_processed_meat",
                                 spade.output.path = "output/lao/")

lao_processed_meat <- subset(lao_processed_meat, select = c(age, HI))
lao_processed_meat <- lao_processed_meat[order(lao_processed_meat$age),]

write.csv(lao_processed_meat, "all_intakes/lao_w_processed_meat.csv")

# Men
lao_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age),
                                 data=lao_men, seed=123,  backtrans.nr = 3,
                                 dgts.distr = 2, min.age=0, max.age=89,
                                 age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                                 sex.lab="men",
           
                                 output.name = "lao_men_processed_meat",
                                 spade.output.path = "output/lao/")

lao_processed_meat <- subset(lao_processed_meat, select = c(age, HI))
lao_processed_meat <- lao_processed_meat[order(lao_processed_meat$age),]

write.csv(lao_processed_meat, "all_intakes/lao_m_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA 3 


lao_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                          data=lao_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=0, max.age=82,
                          age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                          sex.lab="women",
                          output.name = "lao_wom_omega_3",
                          spade.output.path = "output/lao/")

lao_omega_3 <- subset(lao_omega_3, select = c(age, HI))
lao_omega_3 <- lao_omega_3[order(lao_omega_3$age),]

write.csv(lao_omega_3, "all_intakes/lao_w_omega_3.csv")

# Men
lao_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                          data=lao_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=0, max.age=89,
                          age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                          sex.lab="men",
    
                          output.name = "lao_men_omega_3",
                          spade.output.path = "output/lao/")

lao_omega_3 <- subset(lao_omega_3, select = c(age, HI))
lao_omega_3 <- lao_omega_3[order(lao_omega_3$age),]

write.csv(lao_omega_3, "all_intakes/lao_m_omega_3.csv")