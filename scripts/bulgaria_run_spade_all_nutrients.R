# Running SPADE: bulg data
# File created on 12/7/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3
# There is no zinc for bulg

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "bulg"))
SPADE.OUTPUT.PATH <- (here("output", "bulg"))

###########################################################
# Remove missing obs
summary(bulg_spade)

# Make separate datasets for men and women
bulg_wom <- subset(bulg_spade, sex==2)
bulg_men <- subset(bulg_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(bulg_wom$age)
range(bulg_men$age)

# Women
bulg_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bulg_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=4,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="women", 
                   output.name = "bulg_wom_b12")

bulg_b12 <- subset(bulg_b12, select = c(age, HI))
bulg_b12 <- bulg_b12[order(bulg_b12$age),]

write.csv(bulg_b12, "all_intakes/bulg_w_b12.csv")

# Men
bulg_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=4,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="men",
                   output.name = "bulg_men_b12")

bulg_b12 <- subset(bulg_b12, select = c(age, HI))
bulg_b12 <- bulg_b12[order(bulg_b12$age),]

write.csv(bulg_b12, "all_intakes/bulg_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
bulg_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "bulg_wom_iron",
                    spade.output.path = "output/bulg/")

bulg_iron <- subset(bulg_iron, select = c(age, HI))
bulg_iron <- bulg_iron[order(bulg_iron$age),]

write.csv(bulg_iron, "all_intakes/bulg_w_iron.csv")

# Men
bulg_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "bulg_men_iron",
                    spade.output.path = "output/bulg/")

bulg_iron <- subset(bulg_iron, select = c(age, HI))
bulg_iron <- bulg_iron[order(bulg_iron$age),]

write.csv(bulg_iron, "all_intakes/bulg_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
bulg_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bulg_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=4,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "bulg_wom_zinc",
                      spade.output.path = "output/bulg/")

bulg_zinc_w <- subset(bulg_zinc_w, select = c(age, HI))
bulg_zinc_w <- bulg_zinc_w[order(bulg_zinc_w$age),]

write.csv(bulg_zinc_w, "all_intakes/bulg_w_zinc.csv")

#  Men
bulg_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bulg_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=4,
                      age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      output.name = "bulg_men_zinc",
                      spade.output.path = "output/bulg/")

bulg_zinc_m <- subset(bulg_zinc_m, select = c(age, HI))
bulg_zinc_m <- bulg_zinc_m[order(bulg_zinc_m$age),]

write.csv(bulg_zinc_m, "all_intakes/bulg_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

bulg_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "bulg_wom_vita",
                    spade.output.path = "output/bulg/")

bulg_vita <- subset(bulg_vita, select = c(age, HI))
bulg_vita <- bulg_vita[order(bulg_vita$age),]

write.csv(bulg_vita, "all_intakes/bulg_w_vita.csv")

# Men
bulg_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "bulg_men_vita",
                    spade.output.path = "output/bulg/")

bulg_vita <- subset(bulg_vita, select = c(age, HI))
bulg_vita <- bulg_vita[order(bulg_vita$age),]

write.csv(bulg_vita, "all_intakes/bulg_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

bulg_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    
                    output.name = "bulg_wom_calc",
                    spade.output.path = "output/bulg/")

bulg_calc <- subset(bulg_calc, select = c(age, HI))
bulg_calc <- bulg_calc[order(bulg_calc$age),]

write.csv(bulg_calc, "all_intakes/bulg_w_calc.csv")

# Men
bulg_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    
                    output.name = "bulg_men_calc",
                    spade.output.path = "output/bulg/")

bulg_calc <- subset(bulg_calc, select = c(age, HI))
bulg_calc <- bulg_calc[order(bulg_calc$age),]

write.csv(bulg_calc, "all_intakes/bulg_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

bulg_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=bulg_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=4,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "bulg_wom_red_meat",
                        spade.output.path = "output/bulg/")

bulg_red_meat <- subset(bulg_red_meat, select = c(age, HI))
bulg_red_meat <- bulg_red_meat[order(bulg_red_meat$age),]

write.csv(bulg_red_meat, "all_intakes/bulg_w_red_meat.csv")

# Men
bulg_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=bulg_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=4,
                        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="men",
                        
                        output.name = "bulg_men_red_meat",
                        spade.output.path = "output/bulg/")

bulg_red_meat <- subset(bulg_red_meat, select = c(age, HI))
bulg_red_meat <- bulg_red_meat[order(bulg_red_meat$age),]

write.csv(bulg_red_meat, "all_intakes/bulg_m_red_meat.csv")

##################################################################

# 7. RUN SPADE FOR OMEGA 3 


bulg_omega_3_w <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=bulg_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=4,
                       age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "bulg_wom_omega_3",
                       spade.output.path = "output/bulg/")

bulg_omega_3_w <- subset(bulg_omega_3_w, select = c(age, HI))
bulg_omega_3_w <- bulg_omega_3_w[order(bulg_omega_3_w$age),]

write.csv(bulg_omega_3_w, "all_intakes/bulg_w_omega_3.csv")


# Men

## NOT RUNNING: try excluding age <1
bulg_men_o <- subset(bulg_men, age > 0)
summary(bulg_men_o)

bulg_omega_3_m <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=bulg_men_o, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       output.name = "bulg_men_omega_3",
                       spade.output.path = "output/bulg/")

bulg_omega_3_m <- subset(bulg_omega_3_m, select = c(age, HI))
bulg_omega_3_m <- bulg_omega_3_m[order(bulg_omega_3_m$age),]

write.csv(bulg_omega_3_m, "all_intakes/bulg_m_omega_3.csv")
