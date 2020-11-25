# Running SPADE: mexico data
# File created on 11/24/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "mexico"))
SPADE.OUTPUT.PATH <- (here("output", "mexico"))
# TOTAL.output <- (here("output", "mexico", 2_logbooks"))
###########################################################
# Remove missing obs
summary(mexico_spade)
mexico_spade <- na.omit(mexico_spade)
summary(mexico_spade)

#remove obs with weights=0
mexico_spade <- subset(mexico_spade, weight !=0)


# Make separate datasets for men and women
mexico_wom <- subset(mexico_spade, sex==2)
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
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
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
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
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
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
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
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_iron",
                    spade.output.path = "output/mexico/")

mexico_iron <- subset(mexico_iron, select = c(age, HI))
mexico_iron <- mexico_iron[order(mexico_iron$age),]

write.csv(mexico_iron, "all_intakes/mexico_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
mexico_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=mexico_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=97,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      weights.name = "weight",
                      output.name = "mexico_wom_zinc",
                      spade.output.path = "output/mexico/")

mexico_zinc_w <- subset(mexico_zinc_w, select = c(age, HI))
mexico_zinc_w <- mexico_zinc_w[order(mexico_zinc_w$age),]

write.csv(mexico_zinc_w, "all_intakes/mexico_w_zinc.csv")

# Men
mexico_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=mexico_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=89,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      weights.name = "weight",
                      output.name = "mexico_men_zinc",
                      spade.output.path = "output/mexico/")

mexico_zinc_m <- subset(mexico_zinc_m, select = c(age, HI))
mexico_zinc_m <- mexico_zinc_m[order(mexico_zinc_m$age),]

write.csv(mexico_zinc_m, "all_intakes/mexico_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

mexico_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=mexico_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=97,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "mexico_wom_vita",
                    spade.output.path = "output/mexico/")

mexico_vita <- subset(mexico_vita, select = c(age, HI))
mexico_vita <- mexico_vita[order(mexico_vita$age),]

write.csv(mexico_vita, "all_intakes/mexico_w_vita.csv")

# Men
mexico_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=mexico_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=89,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_vita",
                    spade.output.path = "output/mexico/")

mexico_vita <- subset(mexico_vita, select = c(age, HI))
mexico_vita <- mexico_vita[order(mexico_vita$age),]

write.csv(mexico_vita, "all_intakes/mexico_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

mexico_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=mexico_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=97,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight",
                    output.name = "mexico_wom_calc",
                    spade.output.path = "output/mexico/")

mexico_calc <- subset(mexico_calc, select = c(age, HI))
mexico_calc <- mexico_calc[order(mexico_calc$age),]

write.csv(mexico_calc, "all_intakes/mexico_w_calc.csv")

# Men
mexico_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=mexico_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=89,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight",
                    output.name = "mexico_men_calc",
                    spade.output.path = "output/mexico/")

mexico_calc <- subset(mexico_calc, select = c(age, HI))
mexico_calc <- mexico_calc[order(mexico_calc$age),]

write.csv(mexico_calc, "all_intakes/mexico_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

mexico_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=mexico_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=97,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        weights.name = "weight",
                        output.name = "mexico_wom_red_meat",
                        spade.output.path = "output/mexico/")

mexico_red_meat <- subset(mexico_red_meat, select = c(age, HI))
mexico_red_meat <- mexico_red_meat[order(mexico_red_meat$age),]

write.csv(mexico_red_meat, "all_intakes/mexico_w_red_meat.csv")

# Men
mexico_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=mexico_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=89,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="men",
                        weights.name = "weight",
                        output.name = "mexico_men_red_meat",
                        spade.output.path = "output/mexico/")

mexico_red_meat <- subset(mexico_red_meat, select = c(age, HI))
mexico_red_meat <- mexico_red_meat[order(mexico_red_meat$age),]

write.csv(mexico_red_meat, "all_intakes/mexico_m_red_meat.csv")
##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

mexico_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age), 
                              data=mexico_wom, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=1, max.age=97,
                              age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                              sex.lab="women",
                              weights.name = "weight",
                              output.name = "mexico_wom_processed_meat",
                              spade.output.path = "output/mexico/")

mexico_processed_meat <- subset(mexico_processed_meat, select = c(age, HI))
mexico_processed_meat <- mexico_processed_meat[order(mexico_processed_meat$age),]

write.csv(mexico_processed_meat, "all_intakes/mexico_w_processed_meat.csv")

# Men
mexico_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age),
                              data=mexico_men, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=1, max.age=89,
                              age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                              sex.lab="men",
                              weights.name = "weight",
                              output.name = "mexico_men_processed_meat",
                              spade.output.path = "output/mexico/")

mexico_processed_meat <- subset(mexico_processed_meat, select = c(age, HI))
mexico_processed_meat <- mexico_processed_meat[order(mexico_processed_meat$age),]

write.csv(mexico_processed_meat, "all_intakes/mexico_m_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA 3 


mexico_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                       data=mexico_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=97,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       weights.name = "weight",
                       output.name = "mexico_wom_omega_3",
                       spade.output.path = "output/mexico/")

mexico_omega_3 <- subset(mexico_omega_3, select = c(age, HI))
mexico_omega_3 <- mexico_omega_3[order(mexico_omega_3$age),]

write.csv(mexico_omega_3, "all_intakes/mexico_w_omega_3.csv")

# Men
mexico_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                       data=mexico_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=89,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       weights.name = "weight",
                       output.name = "mexico_men_omega_3",
                       spade.output.path = "output/mexico/")

mexico_omega_3 <- subset(mexico_omega_3, select = c(age, HI))
mexico_omega_3 <- mexico_omega_3[order(mexico_omega_3$age),]

write.csv(mexico_omega_3, "all_intakes/mexico_m_omega_3.csv")