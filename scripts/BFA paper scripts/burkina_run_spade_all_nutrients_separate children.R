# Running SPADE: burkina data
# File created on 12/7/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3


# new file made to try to fix weird distributions
# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "burkina"))
SPADE.OUTPUT.PATH <- (here("output", "burkina"))
# TOTAL.output <- (here("output", "burkina", 2_logbooks"))
###########################################################
# Remove missing obs
summary(burkina_spade)

# Make separate datasets for children and adults
burkina_child <- subset(burkina_spade, age < 19)
burkina_wom <- subset(burkina_spade, age>=19)

###########################################################
# 1. RUN SPADE FOR B12

range(burkina_wom$age)
range(burkina_child$age)

# number of intakes per person:
table(burkina_spade$age)

# Women
burkina_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=burkina_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=19, max.age=55,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="women", weights.name = "sample_weight",
                   output.name = "burkina_wom_b12")

burkina_b12 <- subset(burkina_b12, select = c(age, HI))
burkina_b12 <- burkina_b12[order(burkina_b12$age),]

write.csv(burkina_b12, "all_intakes/burkina_w_ad_b12.csv")

# Children
burkina_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=burkina_child, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4, 
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="both", weights.name = "sample_weight",
                   output.name = "burkina_child_b12")

burkina_b12 <- subset(burkina_b12, select = c(age, HI))
burkina_b12 <- burkina_b12[order(burkina_b12$age),]

write.csv(burkina_b12, "all_intakes/burkina_c_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
burkina_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=55,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_iron",
                    spade.output.path = "output/burkina/")

burkina_iron <- subset(burkina_iron, select = c(age, HI))
burkina_iron <- burkina_iron[order(burkina_iron$age),]

write.csv(burkina_iron, "all_intakes/burkina_w_ad_iron.csv")

# Men
burkina_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=burkina_child, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="both", weights.name = "sample_weight",
                    output.name = "burkina_child_iron",
                    spade.output.path = "output/burkina/")

burkina_iron <- subset(burkina_iron, select = c(age, HI))
burkina_iron <- burkina_iron[order(burkina_iron$age),]

write.csv(burkina_iron, "all_intakes/burkina_c_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
burkina_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=burkina_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=55,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women", weights.name = "sample_weight",
                      output.name = "burkina_wom_zinc",
                      spade.output.path = "output/burkina/")

burkina_zinc_w <- subset(burkina_zinc_w, select = c(age, HI))
burkina_zinc_w <- burkina_zinc_w[order(burkina_zinc_w$age),]

write.csv(burkina_zinc_w, "all_intakes/burkina_w_ad_zinc.csv")

# Men
burkina_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=burkina_child, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="both", weights.name = "sample_weight",
                      output.name = "burkina_child_zinc",
                      spade.output.path = "output/burkina/")

burkina_zinc_m <- subset(burkina_zinc_m, select = c(age, HI))
burkina_zinc_m <- burkina_zinc_m[order(burkina_zinc_m$age),]

write.csv(burkina_zinc_m, "all_intakes/burkina_c_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

burkina_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=55,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_vita",
                    spade.output.path = "output/burkina/")

burkina_vita <- subset(burkina_vita, select = c(age, HI))
burkina_vita <- burkina_vita[order(burkina_vita$age),]

write.csv(burkina_vita, "all_intakes/burkina_w_ad_vita.csv")

# Men
burkina_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=burkina_child, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="both", weights.name = "sample_weight",
                    output.name = "burkina_child_vita",
                    spade.output.path = "output/burkina/")

burkina_vita <- subset(burkina_vita, select = c(age, HI))
burkina_vita <- burkina_vita[order(burkina_vita$age),]

write.csv(burkina_vita, "all_intakes/burkina_c_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

burkina_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=burkina_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=55,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women", weights.name = "sample_weight",
                    output.name = "burkina_wom_calc",
                    spade.output.path = "output/burkina/")

burkina_calc <- subset(burkina_calc, select = c(age, HI))
burkina_calc <- burkina_calc[order(burkina_calc$age),]

write.csv(burkina_calc, "all_intakes/burkina_w_ad_calc.csv")

# Men
burkina_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=burkina_child, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="both", weights.name = "sample_weight",
                    output.name = "burkina_child_calc",
                    spade.output.path = "output/burkina/")

burkina_calc <- subset(burkina_calc, select = c(age, HI))
burkina_calc <- burkina_calc[order(burkina_calc$age),]

write.csv(burkina_calc, "all_intakes/burkina_c_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

burkina_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=burkina_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=19, max.age=55,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women", weights.name = "sample_weight",
                        output.name = "burkina_wom_red_meat",
                        spade.output.path = "output/burkina/")

burkina_red_meat <- subset(burkina_red_meat, select = c(age, HI))
burkina_red_meat <- burkina_red_meat[order(burkina_red_meat$age),]

write.csv(burkina_red_meat, "all_intakes/burkina_w_ad_red_meat.csv")

# Men
burkina_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=burkina_child, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="both", weights.name = "sample_weight",
                        output.name = "burkina_child_red_meat",
                        spade.output.path = "output/burkina/")

burkina_red_meat <- subset(burkina_red_meat, select = c(age, HI))
burkina_red_meat <- burkina_red_meat[order(burkina_red_meat$age),]

write.csv(burkina_red_meat, "all_intakes/burkina_c_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR OMEGA 3 


burkina_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=burkina_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=55,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women", weights.name = "sample_weight",
                       output.name = "burkina_wom_omega_3",
                       spade.output.path = "output/burkina/")

burkina_omega_3 <- subset(burkina_omega_3, select = c(age, HI))
burkina_omega_3 <- burkina_omega_3[order(burkina_omega_3$age),]

write.csv(burkina_omega_3, "all_intakes/burkina_w_ad_omega_3.csv")

# Men
burkina_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=burkina_child, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="both", weights.name = "sample_weight",
                       output.name = "burkina_child_omega_3",
                       spade.output.path = "output/burkina/")

burkina_omega_3 <- subset(burkina_omega_3, select = c(age, HI))
burkina_omega_3 <- burkina_omega_3[order(burkina_omega_3$age),]

write.csv(burkina_omega_3, "all_intakes/burkina_c_omega_3.csv")