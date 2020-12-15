# Running SPADE: bangldadesh data
# File created on 12/15/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "bangladesh"))
SPADE.OUTPUT.PATH <- (here("output", "bang"))

###########################################################
# Remove missing obs
summary(bang_spade)

# Make separate datasets for men and women
bang_wom <- subset(bang_spade, sex==2 )

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
                        output.name = "bang_wom_iron",
                        spade.output.path = "output/bang/")

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
                          output.name = "bang_wom_zinc",
                          spade.output.path = "output/bang/")

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
                        output.name = "bang_wom_vita",
                        spade.output.path = "output/bang/")

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
                        
                        output.name = "bang_wom_calc",
                        spade.output.path = "output/bang/")

bang_calc <- subset(bang_calc, select = c(age, HI))
bang_calc <- bang_calc[order(bang_calc$age),]

write.csv(bang_calc, "all_intakes/bang_w_calc.csv")


##################################################################

# 6. RUN SPADE FOR RED MEAT

bang_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                            data=bang_wom, seed=123,  backtrans.nr = 3,
                            dgts.distr = 2, min.age=16, max.age=70,
                            age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                            sex.lab="women",
                            output.name = "bang_wom_red_meat",
                            spade.output.path = "output/bang/")

bang_red_meat <- subset(bang_red_meat, select = c(age, HI))
bang_red_meat <- bang_red_meat[order(bang_red_meat$age),]

write.csv(bang_red_meat, "all_intakes/bang_w_red_meat.csv")


##################################################################

# 7. RUN SPADE FOR OMEGA 3 


bang_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                           data=bang_wom, seed=123,  backtrans.nr = 3,
                           dgts.distr = 2, min.age=16, max.age=70,
                           age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                           sex.lab="women",
                           output.name = "bang_wom_omega_3",
                           spade.output.path = "output/bang/")

bang_omega_3 <- subset(bang_omega_3, select = c(age, HI))
bang_omega_3 <- bang_omega_3[order(bang_omega_3$age),]

write.csv(bang_omega_3, "all_intakes/bang_w_omega_3.csv")

