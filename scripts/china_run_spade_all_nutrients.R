# Running SPADE: china data
# File created on 12/3/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc, calcium, red meat, processed meat, omega 3

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "china"))

###########################################################
# Remove missing obs
summary(china_spade)

china_spade$id <- as.integer(china_spade$id)

# Make separate datasets for men and women
china_wom <- subset(china_spade, sex==2)
china_men <- subset(china_spade, sex==1)

range(china_wom$age)
range(china_men$age)



# THERE IS NO B12 IN THE CHINA DATASET
###########################################################
# 1. RUN SPADE FOR B12
# 
# # Women
# china_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
#                    data=china_wom, seed=123,  backtrans.nr = 3,
#                    dgts.distr = 2, min.age=15, max.age=98,
#                    age.classes=c(15, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
#                    sex.lab="women", 
#                    output.name = "china_wom_b12")
# 
# china_b12 <- subset(china_b12, select = c(age, HI))
# china_b12 <- china_b12[order(china_b12$age),]
# 
# write.csv(china_b12, "all_intakes/china_w_b12.csv")
# 
# # Men
# china_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
#                    data=china_men, seed=123,  backtrans.nr = 3,
#                    dgts.distr = 2, min.age=15, max.age=92,
#                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
#                    sex.lab="men",
#                    output.name = "china_men_b12")
# 
# china_b12 <- subset(china_b12, select = c(age, HI))
# china_b12 <- china_b12[order(china_b12$age),]
# 
# write.csv(china_b12, "all_intakes/china_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
china_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=china_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=98,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "china_wom_iron",
                    spade.output.path = "output/china/")

china_iron <- subset(china_iron, select = c(age, HI))
china_iron <- china_iron[order(china_iron$age),]

write.csv(china_iron, "all_intakes/china_w_iron.csv")

# Men
china_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=china_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=92,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "china_men_iron",
                    spade.output.path = "output/china/")

china_iron <- subset(china_iron, select = c(age, HI))
china_iron <- china_iron[order(china_iron$age),]

write.csv(china_iron, "all_intakes/china_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
china_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=china_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=15, max.age=98,
                      age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="women",
                      output.name = "china_wom_zinc",
                      spade.output.path = "output/china/")

china_zinc_w <- subset(china_zinc_w, select = c(age, HI))
china_zinc_w <- china_zinc_w[order(china_zinc_w$age),]

write.csv(china_zinc_w, "all_intakes/china_w_zinc.csv")

# Men
china_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=china_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=15, max.age=92,
                      age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                      sex.lab="men",
                      output.name = "china_men_zinc",
                      spade.output.path = "output/china/")

china_zinc_m <- subset(china_zinc_m, select = c(age, HI))
china_zinc_m <- china_zinc_m[order(china_zinc_m$age),]

write.csv(china_zinc_m, "all_intakes/china_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

china_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=china_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=98,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    output.name = "china_wom_vita",
                    spade.output.path = "output/china/")

china_vita <- subset(china_vita, select = c(age, HI))
china_vita <- china_vita[order(china_vita$age),]

write.csv(china_vita, "all_intakes/china_w_vita.csv")

# Men
china_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=china_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=92,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    output.name = "china_men_vita",
                    spade.output.path = "output/china/")

china_vita <- subset(china_vita, select = c(age, HI))
china_vita <- china_vita[order(china_vita$age),]

write.csv(china_vita, "all_intakes/china_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

china_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=china_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=98,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="women",
                    
                    output.name = "china_wom_calc",
                    spade.output.path = "output/china/")

china_calc <- subset(china_calc, select = c(age, HI))
china_calc <- china_calc[order(china_calc$age),]

write.csv(china_calc, "all_intakes/china_w_calc.csv")

# Men
china_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=china_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=15, max.age=92,
                    age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    
                    output.name = "china_men_calc",
                    spade.output.path = "output/china/")

china_calc <- subset(china_calc, select = c(age, HI))
china_calc <- china_calc[order(china_calc$age),]

write.csv(china_calc, "all_intakes/china_m_calc.csv")
##################################################################

# 6. RUN SPADE FOR RED MEAT

china_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=china_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=15, max.age=98,
                        age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="women",
                        output.name = "china_wom_red_meat",
                        spade.output.path = "output/china/")

china_red_meat <- subset(china_red_meat, select = c(age, HI))
china_red_meat <- china_red_meat[order(china_red_meat$age),]

write.csv(china_red_meat, "all_intakes/china_w_red_meat.csv")

# Men
china_red_meat <- f.spade(frml.ia=red_meat~fp(age), frml.if=red_meat~cs(age),
                        data=china_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=15, max.age=92,
                        age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                        sex.lab="men",
                        
                        output.name = "china_men_red_meat",
                        spade.output.path = "output/china/")

china_red_meat <- subset(china_red_meat, select = c(age, HI))
china_red_meat <- china_red_meat[order(china_red_meat$age),]

write.csv(china_red_meat, "all_intakes/china_m_red_meat.csv")
##################################################################

# 7. RUN SPADE FOR PROCESSED MEAT

china_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age), 
                              data=china_wom, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=15, max.age=98,
                              age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                              sex.lab="women",
                              
                              output.name = "china_wom_processed_meat",
                              spade.output.path = "output/china/")

china_processed_meat <- subset(china_processed_meat, select = c(age, HI))
china_processed_meat <- china_processed_meat[order(china_processed_meat$age),]

write.csv(china_processed_meat, "all_intakes/china_w_processed_meat.csv")

# Men
china_processed_meat <- f.spade(frml.ia=processed_meat~fp(age), frml.if=processed_meat~cs(age),
                              data=china_men, seed=123,  backtrans.nr = 3,
                              dgts.distr = 2, min.age=15, max.age=92,
                              age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                              sex.lab="men",
                              
                              output.name = "china_men_processed_meat",
                              spade.output.path = "output/china/")

china_processed_meat <- subset(china_processed_meat, select = c(age, HI))
china_processed_meat <- china_processed_meat[order(china_processed_meat$age),]

write.csv(china_processed_meat, "all_intakes/china_m_processed_meat.csv")

##################################################################

# 8. RUN SPADE FOR OMEGA 3 


china_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=china_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=15, max.age=98,
                       age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="women",
                       output.name = "china_wom_omega_3",
                       spade.output.path = "output/china/")

china_omega_3 <- subset(china_omega_3, select = c(age, HI))
china_omega_3 <- china_omega_3[order(china_omega_3$age),]

write.csv(china_omega_3, "all_intakes/china_w_omega_3.csv")

# Men
china_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=china_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=15, max.age=92,
                       age.classes=c( 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                       sex.lab="men",
                       
                       output.name = "china_men_omega_3",
                       spade.output.path = "output/china/")

china_omega_3 <- subset(china_omega_3, select = c(age, HI))
china_omega_3 <- china_omega_3[order(china_omega_3$age),]

write.csv(china_omega_3, "all_intakes/china_m_omega_3.csv")