# Running SPADE: Philippines data
# File created on 12/3/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Philippines"))

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
phil_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=phil_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=15, max.age=47,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
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
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "phil_wom_iron",
                    spade.output.path = "output/phil/")

phil_iron <- subset(phil_iron, select = c(age, HI))
phil_iron <- phil_iron[order(phil_iron$age),]

write.csv(phil_iron, "all_intakes/phil_w_iron.csv")

##################################################################

# 3. RUN SPADE FOR ZINC
phil_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=phil_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=15, max.age=47,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "phil_wom_zinc",
                      spade.output.path = "output/phil/")

phil_zinc_w <- subset(phil_zinc_w, select = c(age, HI))
phil_zinc_w <- phil_zinc_w[order(phil_zinc_w$age),]

write.csv(phil_zinc_w, "all_intakes/phil_w_zinc.csv")


##################################################################

# 4. RUN SPADE FOR VIT A

phil_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=phil_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=47,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "phil_wom_vita",
                    spade.output.path = "output/phil/")

phil_vita <- subset(phil_vita, select = c(age, HI))
phil_vita <- phil_vita[order(phil_vita$age),]

write.csv(phil_vita, "all_intakes/phil_w_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

phil_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=phil_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=47,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    
                    output.name = "phil_wom_calc",
                    spade.output.path = "output/phil/")

phil_calc <- subset(phil_calc, select = c(age, HI))
phil_calc <- phil_calc[order(phil_calc$age),]

write.csv(phil_calc, "all_intakes/phil_w_calc.csv")

##################################################################

# 6. RUN SPADE FOR RED MEAT

phil_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=phil_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=15, max.age=47,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "phil_wom_red_meat",
                        spade.output.path = "output/phil/")

phil_red_meat <- subset(phil_red_meat, select = c(age, HI))
phil_red_meat <- phil_red_meat[order(phil_red_meat$age),]

write.csv(phil_red_meat, "all_intakes/phil_w_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

phil_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age), 
                              data=phil_wom, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=15, max.age=47,
                              age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                              sex.lab="women",
                              
                              output.name = "phil_wom_processed_meat",
                              spade.output.path = "output/phil/")

phil_processed_meat <- subset(phil_processed_meat, select = c(age, HI))
phil_processed_meat <- phil_processed_meat[order(phil_processed_meat$age),]

write.csv(phil_processed_meat, "all_intakes/phil_w_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA 3 


phil_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=phil_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=15, max.age=47,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "phil_wom_omega_3",
                       spade.output.path = "output/phil/")

phil_omega_3 <- subset(phil_omega_3, select = c(age, HI))
phil_omega_3 <- phil_omega_3[order(phil_omega_3$age),]

write.csv(phil_omega_3, "all_intakes/phil_w_omega_3.csv")
