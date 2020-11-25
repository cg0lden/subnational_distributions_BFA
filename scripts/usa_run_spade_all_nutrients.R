# Running SPADE: usa data
# File created on 11/24/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "usa"))
SPADE.OUTPUT.PATH <- (here("output", "usa"))
# TOTAL.output <- (here("output", "usa", 2_logbooks"))
###########################################################
# Remove missing obs
summary(usa_spade)
usa_spade <- na.omit(usa_spade)
summary(usa_spade)


#remove obs with weights=0
usa_spade <- subset(usa_spade, weight1 !=0)


# Make separate datasets for men and women
usa_wom <- subset(usa_spade, sex==2)
usa_men <- subset(usa_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(usa_wom$age)
range(usa_men$age)

# number of intakes per person:
table(table(usa_wom$weight))

# Women
usa_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
        data=usa_wom, seed=123,  backtrans.nr = 3,
        dgts.distr = 2, min.age=0, max.age=80,
        age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
        sex.lab="women",
        weights.name = "weight1",
        output.name = "usa_wom_b12")

usa_b12 <- subset(usa_b12, select = c(age, HI))
usa_b12 <- usa_b12[order(usa_b12$age),]

write.csv(usa_b12, "all_intakes/usa_w_b12.csv")

# Men
usa_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_b12")

usa_b12 <- subset(usa_b12, select = c(age, HI))
usa_b12 <- usa_b12[order(usa_b12$age),]

write.csv(usa_b12, "all_intakes/usa_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
usa_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                   data=usa_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="women",
                   weights.name = "weight1",
                   output.name = "usa_wom_iron",
                   spade.output.path = "output/usa/")

usa_iron <- subset(usa_iron, select = c(age, HI))
usa_iron <- usa_iron[order(usa_iron$age),]

write.csv(usa_iron, "all_intakes/usa_w_iron.csv")

# Men
usa_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_iron",
                   spade.output.path = "output/usa/")

usa_iron <- subset(usa_iron, select = c(age, HI))
usa_iron <- usa_iron[order(usa_iron$age),]

write.csv(usa_iron, "all_intakes/usa_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
usa_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_zinc",
                    spade.output.path = "output/usa/")

usa_zinc_w <- subset(usa_zinc_w, select = c(age, HI))
usa_zinc_w <- usa_zinc_w[order(usa_zinc_w$age),]

write.csv(usa_zinc_w, "all_intakes/usa_w_zinc.csv")

# Men
usa_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_zinc",
                    spade.output.path = "output/usa/")

usa_zinc_m <- subset(usa_zinc_m, select = c(age, HI))
usa_zinc_m <- usa_zinc_m[order(usa_zinc_m$age),]

write.csv(usa_zinc_m, "all_intakes/usa_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

usa_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vita",
                    spade.output.path = "output/usa/")

usa_vita <- subset(usa_vita, select = c(age, HI))
usa_vita <- usa_vita[order(usa_vita$age),]

write.csv(usa_vita, "all_intakes/usa_w_vita.csv")

# Men
usa_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vita",
                    spade.output.path = "output/usa/")

usa_vita <- subset(usa_vita, select = c(age, HI))
usa_vita <- usa_vita[order(usa_vita$age),]

write.csv(usa_vita, "all_intakes/usa_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

usa_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_calc",
                    spade.output.path = "output/usa/")

usa_calc <- subset(usa_calc, select = c(age, HI))
usa_calc <- usa_calc[order(usa_calc$age),]

write.csv(usa_calc, "all_intakes/usa_w_calc.csv")

# Men
usa_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_calc",
                    spade.output.path = "output/usa/")

usa_calc <- subset(usa_calc, select = c(age, HI))
usa_calc <- usa_calc[order(usa_calc$age),]

write.csv(usa_calc, "all_intakes/usa_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

usa_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_red_meat",
                    spade.output.path = "output/usa/")

usa_red_meat <- subset(usa_red_meat, select = c(age, HI))
usa_red_meat <- usa_red_meat[order(usa_red_meat$age),]

write.csv(usa_red_meat, "all_intakes/usa_w_red_meat.csv")

# Men
usa_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_red_meat",
                    spade.output.path = "output/usa/")

usa_red_meat <- subset(usa_red_meat, select = c(age, HI))
usa_red_meat <- usa_red_meat[order(usa_red_meat$age),]

write.csv(usa_red_meat, "all_intakes/usa_m_red_meat.csv")
##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

usa_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_processed_meat",
                    spade.output.path = "output/usa/")

usa_processed_meat <- subset(usa_processed_meat, select = c(age, HI))
usa_processed_meat <- usa_processed_meat[order(usa_processed_meat$age),]

write.csv(usa_processed_meat, "all_intakes/usa_w_processed_meat.csv")

# Men
usa_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_processed_meat",
                    spade.output.path = "output/usa/")

usa_processed_meat <- subset(usa_processed_meat, select = c(age, HI))
usa_processed_meat <- usa_processed_meat[order(usa_processed_meat$age),]

write.csv(usa_processed_meat, "all_intakes/usa_m_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA 3 


usa_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_omega_3",
                    spade.output.path = "output/usa/")

usa_omega_3 <- subset(usa_omega_3, select = c(age, HI))
usa_omega_3 <- usa_omega_3[order(usa_omega_3$age),]

write.csv(usa_omega_3, "all_intakes/usa_w_omega_3.csv")

# Men
usa_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_omega_3",
                    spade.output.path = "output/usa/")

usa_omega_3 <- subset(usa_omega_3, select = c(age, HI))
usa_omega_3 <- usa_omega_3[order(usa_omega_3$age),]

write.csv(usa_omega_3, "all_intakes/usa_m_omega_3.csv")