# Running SPADE: bolivia data
# File created on 12/15/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "bolivia"))
SPADE.OUTPUT.PATH <- (here("output", "bolivia"))

###########################################################
# Remove missing obs
summary(bolivia_spade)

# Make separate datasets for men and women
bolivia_wom <- subset(bolivia_spade, sex==2 & !is.na(age))
# have to remove two observations for women because the age isn't reported
# Also missing birth year and age in months for these obs
bolivia_men <- subset(bolivia_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(bolivia_wom$age)
summary(bolivia_wom)
#check the two missing values of age, 427 and 434 mday 1

range(bolivia_men$age)

# number of intakes per person:
table(bolivia_wom$age)

# Women
bolivia_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bolivia_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=4, max.age=52,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="women", 
                   output.name = "bolivia_wom_b12")

bolivia_b12 <- subset(bolivia_b12, select = c(age, HI))
bolivia_b12 <- bolivia_b12[order(bolivia_b12$age),]

write.csv(bolivia_b12, "all_intakes/bolivia_w_b12.csv")

# Men
bolivia_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bolivia_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=4, max.age=37,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="men",
                   output.name = "bolivia_men_b12")

bolivia_b12 <- subset(bolivia_b12, select = c(age, HI))
bolivia_b12 <- bolivia_b12[order(bolivia_b12$age),]

write.csv(bolivia_b12, "all_intakes/bolivia_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
bolivia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=bolivia_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=52,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "bolivia_wom_iron",
                    spade.output.path = "output/bolivia/")

bolivia_iron <- subset(bolivia_iron, select = c(age, HI))
bolivia_iron <- bolivia_iron[order(bolivia_iron$age),]

write.csv(bolivia_iron, "all_intakes/bolivia_w_iron.csv")

# Men
bolivia_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=bolivia_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=37,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "bolivia_men_iron",
                    spade.output.path = "output/bolivia/")

bolivia_iron <- subset(bolivia_iron, select = c(age, HI))
bolivia_iron <- bolivia_iron[order(bolivia_iron$age),]

write.csv(bolivia_iron, "all_intakes/bolivia_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
bolivia_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bolivia_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=4, max.age=52,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "bolivia_wom_zinc",
                      spade.output.path = "output/bolivia/")

bolivia_zinc_w <- subset(bolivia_zinc_w, select = c(age, HI))
bolivia_zinc_w <- bolivia_zinc_w[order(bolivia_zinc_w$age),]

write.csv(bolivia_zinc_w, "all_intakes/bolivia_w_zinc.csv")

#  Men
bolivia_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bolivia_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=4, max.age=37,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      output.name = "bolivia_men_zinc",
                      spade.output.path = "output/bolivia/")

bolivia_zinc_m <- subset(bolivia_zinc_m, select = c(age, HI))
bolivia_zinc_m <- bolivia_zinc_m[order(bolivia_zinc_m$age),]

write.csv(bolivia_zinc_m, "all_intakes/bolivia_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

bolivia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bolivia_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=52,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "bolivia_wom_vita",
                    spade.output.path = "output/bolivia/")

bolivia_vita <- subset(bolivia_vita, select = c(age, HI))
bolivia_vita <- bolivia_vita[order(bolivia_vita$age),]

write.csv(bolivia_vita, "all_intakes/bolivia_w_vita.csv")

# Men
bolivia_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bolivia_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=37,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "bolivia_men_vita",
                    spade.output.path = "output/bolivia/")

bolivia_vita <- subset(bolivia_vita, select = c(age, HI))
bolivia_vita <- bolivia_vita[order(bolivia_vita$age),]

write.csv(bolivia_vita, "all_intakes/bolivia_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

bolivia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bolivia_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=52,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    
                    output.name = "bolivia_wom_calc",
                    spade.output.path = "output/bolivia/")

bolivia_calc <- subset(bolivia_calc, select = c(age, HI))
bolivia_calc <- bolivia_calc[order(bolivia_calc$age),]

write.csv(bolivia_calc, "all_intakes/bolivia_w_calc.csv")

# Men
bolivia_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bolivia_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=4, max.age=37,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    
                    output.name = "bolivia_men_calc",
                    spade.output.path = "output/bolivia/")

bolivia_calc <- subset(bolivia_calc, select = c(age, HI))
bolivia_calc <- bolivia_calc[order(bolivia_calc$age),]

write.csv(bolivia_calc, "all_intakes/bolivia_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

bolivia_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=bolivia_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=4, max.age=52,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "bolivia_wom_red_meat",
                        spade.output.path = "output/bolivia/")

bolivia_red_meat <- subset(bolivia_red_meat, select = c(age, HI))
bolivia_red_meat <- bolivia_red_meat[order(bolivia_red_meat$age),]

write.csv(bolivia_red_meat, "all_intakes/bolivia_w_red_meat.csv")

# Men
bolivia_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=bolivia_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=4, max.age=37,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="men",
                        
                        output.name = "bolivia_men_red_meat",
                        spade.output.path = "output/bolivia/")

bolivia_red_meat <- subset(bolivia_red_meat, select = c(age, HI))
bolivia_red_meat <- bolivia_red_meat[order(bolivia_red_meat$age),]

write.csv(bolivia_red_meat, "all_intakes/bolivia_m_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR OMEGA 3 


bolivia_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=bolivia_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=4, max.age=52,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "bolivia_wom_omega_3",
                       spade.output.path = "output/bolivia/")

bolivia_omega_3 <- subset(bolivia_omega_3, select = c(age, HI))
bolivia_omega_3 <- bolivia_omega_3[order(bolivia_omega_3$age),]

write.csv(bolivia_omega_3, "all_intakes/bolivia_w_omega_3.csv")

# Men
bolivia_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=bolivia_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=4, max.age=37,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       
                       output.name = "bolivia_men_omega_3",
                       spade.output.path = "output/bolivia/")

bolivia_omega_3 <- subset(bolivia_omega_3, select = c(age, HI))
bolivia_omega_3 <- bolivia_omega_3[order(bolivia_omega_3$age),]

write.csv(bolivia_omega_3, "all_intakes/bolivia_m_omega_3.csv")