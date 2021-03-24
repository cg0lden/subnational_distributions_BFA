# Running SPADE: usa data
# File created on March 24th
# Used to look at how distributions change over time
# Years 2011-2018


# Load packages
library(SPADE.RIVMNwCore)
library(here)
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "usa", "older"))

# RUN WITH 2017-2018 DATA #
####################################################################################################################################

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "usa_2007_2008"))


###########################################################
# Remove missing obs
summary(usa_spade)
usa_spade <- na.omit(usa_spade)
summary(usa_spade)
names(usa_spade)

#remove obs with weights=0
usa_spade <- subset(usa_spade, weight1 !=0)
# Remove instance where there are 


# number of intakes per person:
table(usa_spade$mday)

# Make separate datasets for men and women
usa_wom <- subset(usa_spade, sex==2)
usa_men <- subset(usa_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

# for NHANES, use the  day1 recall sample weights because we are using both day 1 and day 2

# Let's have a look at the highest b12 intakes
range(usa_wom$age)
range(usa_men$age)

# Women
usa_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
        data=usa_wom, seed=123,  backtrans.nr = 3,
        dgts.distr = 2, min.age=0, max.age=80,
        sex.lab="women",
        weights.name = "weight1",
        output.name = "usa_wom_vitb12")

usa_vitb12 <- subset(usa_vitb12, select = c(age, HI))
usa_vitb12 <- usa_vitb12[order(usa_vitb12$age),]

write.csv(usa_vitb12, "old_nhanes/usa_w_vitb12.csv")

# Men
usa_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_vitb12")

usa_vitb12 <- subset(usa_vitb12, select = c(age, HI))
usa_vitb12 <- usa_vitb12[order(usa_vitb12$age),]

write.csv(usa_vitb12, "old_nhanes/usa_m_vitb12.csv")


##################################################################

# 2. RUN SPADE FOR IRON
# Women
usa_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                   data=usa_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   sex.lab="women",
                   weights.name = "weight1",
                   output.name = "usa_wom_iron")

usa_iron <- subset(usa_iron, select = c(age, HI))
usa_iron <- usa_iron[order(usa_iron$age),]

write.csv(usa_iron, "old_nhanes/usa_w_iron.csv")

# Men
usa_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_iron")

usa_iron <- subset(usa_iron, select = c(age, HI))
usa_iron <- usa_iron[order(usa_iron$age),]

write.csv(usa_iron, "old_nhanes/usa_m_iron.csv")
##################################################################

# 3. RUN SPADE FOR ZINC
usa_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_zinc")

usa_zinc_w <- subset(usa_zinc_w, select = c(age, HI))
usa_zinc_w <- usa_zinc_w[order(usa_zinc_w$age),]

write.csv(usa_zinc_w, "old_nhanes/usa_w_zinc.csv")

# Men
usa_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_zinc")

usa_zinc_m <- subset(usa_zinc_m, select = c(age, HI))
usa_zinc_m <- usa_zinc_m[order(usa_zinc_m$age),]

write.csv(usa_zinc_m, "old_nhanes/usa_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

usa_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vita")

usa_vita <- subset(usa_vita, select = c(age, HI))
usa_vita <- usa_vita[order(usa_vita$age),]

write.csv(usa_vita, "old_nhanes/usa_w_vita.csv")

# Men
usa_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vita")

usa_vita <- subset(usa_vita, select = c(age, HI))
usa_vita <- usa_vita[order(usa_vita$age),]

write.csv(usa_vita, "old_nhanes/usa_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

usa_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_calc")

usa_calc <- subset(usa_calc, select = c(age, HI))
usa_calc <- usa_calc[order(usa_calc$age),]

write.csv(usa_calc, "old_nhanes/usa_w_calc.csv")

# Men
usa_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_calc")

usa_calc <- subset(usa_calc, select = c(age, HI))
usa_calc <- usa_calc[order(usa_calc$age),]

write.csv(usa_calc, "old_nhanes/usa_m_calc.csv")


##################################################################

# 6. RUN SPADE FOR OMEGA 3 

usa_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_omega_3")

usa_omega_3 <- subset(usa_omega_3, select = c(age, HI))
usa_omega_3 <- usa_omega_3[order(usa_omega_3$age),]

write.csv(usa_omega_3, "old_nhanes/usa_w_omega_3.csv")

# Men
usa_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_omega_3")

usa_omega_3 <- subset(usa_omega_3, select = c(age, HI))
usa_omega_3 <- usa_omega_3[order(usa_omega_3$age),]

write.csv(usa_omega_3, "old_nhanes/usa_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITAMIN E

usa_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vite")

usa_vite <- subset(usa_vite, select = c(age, HI))
usa_vite <- usa_vite[order(usa_vite$age),]

write.csv(usa_vite, "old_nhanes/usa_w_vite.csv")

# Men
usa_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vite")

usa_vite <- subset(usa_vite, select = c(age, HI))
usa_vite <- usa_vite[order(usa_vite$age),]

write.csv(usa_vite, "old_nhanes/usa_m_vite.csv")

##################################################################

# 8. RUN SPADE FOR ENERGY

usa_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_energy")

usa_energy <- subset(usa_energy, select = c(age, HI))
usa_energy <- usa_energy[order(usa_energy$age),]

write.csv(usa_energy, "old_nhanes/usa_w_energy.csv")

# Men
usa_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_energy")

usa_energy <- subset(usa_energy, select = c(age, HI))
usa_energy <- usa_energy[order(usa_energy$age),]

write.csv(usa_energy, "old_nhanes/usa_m_energy.csv")

##################################################################

# 9. RUN SPADE FOR PROTEIN

usa_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_protein")

usa_protein <- subset(usa_protein, select = c(age, HI))
usa_protein <- usa_protein[order(usa_protein$age),]

write.csv(usa_protein, "old_nhanes/usa_w_protein.csv")

# Men
usa_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_protein")

usa_protein <- subset(usa_protein, select = c(age, HI))
usa_protein <- usa_protein[order(usa_protein$age),]

write.csv(usa_protein, "old_nhanes/usa_m_protein.csv")

##################################################################

# 10. RUN SPADE FOR CARB

usa_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_carb")

usa_carb <- subset(usa_carb, select = c(age, HI))
usa_carb <- usa_carb[order(usa_carb$age),]

write.csv(usa_carb, "old_nhanes/usa_w_carb.csv")

# Men
usa_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_carb")

usa_carb <- subset(usa_carb, select = c(age, HI))
usa_carb <- usa_carb[order(usa_carb$age),]

write.csv(usa_carb, "old_nhanes/usa_m_carb.csv")

##################################################################

# 11. RUN SPADE FOR SUGAR

usa_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_sugar")

usa_sugar <- subset(usa_sugar, select = c(age, HI))
usa_sugar <- usa_sugar[order(usa_sugar$age),]

write.csv(usa_sugar, "old_nhanes/usa_w_sugar.csv")

# Men
usa_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_sugar")

usa_sugar <- subset(usa_sugar, select = c(age, HI))
usa_sugar <- usa_sugar[order(usa_sugar$age),]

write.csv(usa_sugar, "old_nhanes/usa_m_sugar.csv")

##################################################################

# 12. RUN SPADE FOR FIBER

usa_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_fiber")

usa_fiber <- subset(usa_fiber, select = c(age, HI))
usa_fiber <- usa_fiber[order(usa_fiber$age),]

write.csv(usa_fiber, "old_nhanes/usa_w_fiber.csv")

# Men
usa_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fiber")

usa_fiber <- subset(usa_fiber, select = c(age, HI))
usa_fiber <- usa_fiber[order(usa_fiber$age),]

write.csv(usa_fiber, "old_nhanes/usa_m_fiber.csv")

##################################################################

# 13. RUN SPADE FOR FAT

usa_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_fat")

usa_fat <- subset(usa_fat, select = c(age, HI))
usa_fat <- usa_fat[order(usa_fat$age),]

write.csv(usa_fat, "old_nhanes/usa_w_fat.csv")

# Men
usa_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fat")

usa_fat <- subset(usa_fat, select = c(age, HI))
usa_fat <- usa_fat[order(usa_fat$age),]

write.csv(usa_fat, "old_nhanes/usa_m_fat.csv")

##################################################################

# 14. RUN SPADE FOR SATFAT

usa_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_satfat")

usa_satfat <- subset(usa_satfat, select = c(age, HI))
usa_satfat <- usa_satfat[order(usa_satfat$age),]

write.csv(usa_satfat, "old_nhanes/usa_w_satfat.csv")

# Men
usa_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_satfat")

usa_satfat <- subset(usa_satfat, select = c(age, HI))
usa_satfat <- usa_satfat[order(usa_satfat$age),]

write.csv(usa_satfat, "old_nhanes/usa_m_satfat.csv")

##################################################################

# 15. RUN SPADE FOR MUFA

usa_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_mufa")

usa_mufa <- subset(usa_mufa, select = c(age, HI))
usa_mufa <- usa_mufa[order(usa_mufa$age),]

write.csv(usa_mufa, "old_nhanes/usa_w_mufa.csv")

# Men
usa_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_mufa")

usa_mufa <- subset(usa_mufa, select = c(age, HI))
usa_mufa <- usa_mufa[order(usa_mufa$age),]

write.csv(usa_mufa, "old_nhanes/usa_m_mufa.csv")

##################################################################

# 16. RUN SPADE FOR PUFA

usa_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_pufa")

usa_pufa <- subset(usa_pufa, select = c(age, HI))
usa_pufa <- usa_pufa[order(usa_pufa$age),]

write.csv(usa_pufa, "old_nhanes/usa_w_pufa.csv")

# Men
usa_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_pufa")

usa_pufa <- subset(usa_pufa, select = c(age, HI))
usa_pufa <- usa_pufa[order(usa_pufa$age),]

write.csv(usa_pufa, "old_nhanes/usa_m_pufa.csv")


# 26. RUN SPADE FOR THIAMIN

usa_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=usa_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=80,
                       sex.lab="women",
                       weights.name = "weight1",
                       output.name = "usa_wom_thia")

usa_thia <- subset(usa_thia, select = c(age, HI))
usa_thia <- usa_thia[order(usa_thia$age),]

write.csv(usa_thia, "old_nhanes/usa_w_thia.csv")

# Men
usa_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=usa_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=80,
                       sex.lab="men",
                       weights.name = "weight1",
                       output.name = "usa_men_thia")

usa_thia <- subset(usa_thia, select = c(age, HI))
usa_thia <- usa_thia[order(usa_thia$age),]

write.csv(usa_thia, "old_nhanes/usa_m_thia.csv")

##################################################################

# 27. RUN SPADE FOR RIBOFLAVIN

usa_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_ribo")

usa_ribo <- subset(usa_ribo, select = c(age, HI))
usa_ribo <- usa_ribo[order(usa_ribo$age),]

write.csv(usa_ribo, "old_nhanes/usa_w_ribo.csv")

# Men
usa_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_ribo")

usa_ribo <- subset(usa_ribo, select = c(age, HI))
usa_ribo <- usa_ribo[order(usa_ribo$age),]

write.csv(usa_ribo, "old_nhanes/usa_m_ribo.csv")

##################################################################

# 28. RUN SPADE FOR NIACIN

usa_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_niac")

usa_niac <- subset(usa_niac, select = c(age, HI))
usa_niac <- usa_niac[order(usa_niac$age),]

write.csv(usa_niac, "old_nhanes/usa_w_niac.csv")

# Men
usa_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_niac")

usa_niac <- subset(usa_niac, select = c(age, HI))
usa_niac <- usa_niac[order(usa_niac$age),]

write.csv(usa_niac, "old_nhanes/usa_m_niac.csv")

##################################################################

# 29. RUN SPADE FOR VITAMIN B6

usa_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vitb6")

usa_vitb6 <- subset(usa_vitb6, select = c(age, HI))
usa_vitb6 <- usa_vitb6[order(usa_vitb6$age),]

write.csv(usa_vitb6, "old_nhanes/usa_w_vitb6.csv")

# Men
usa_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vitb6")

usa_vitb6 <- subset(usa_vitb6, select = c(age, HI))
usa_vitb6 <- usa_vitb6[order(usa_vitb6$age),]

write.csv(usa_vitb6, "old_nhanes/usa_m_vitb6.csv")

##################################################################

# 30. RUN SPADE FOR FOLATE

usa_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_fola")

usa_fola <- subset(usa_fola, select = c(age, HI))
usa_fola <- usa_fola[order(usa_fola$age),]

write.csv(usa_fola, "old_nhanes/usa_w_fola.csv")

# Men
usa_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fola")

usa_fola <- subset(usa_fola, select = c(age, HI))
usa_fola <- usa_fola[order(usa_fola$age),]

write.csv(usa_fola, "old_nhanes/usa_m_fola.csv")

##################################################################

# 32. RUN SPADE FOR VITAMIN D

usa_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vitd")

usa_vitd <- subset(usa_vitd, select = c(age, HI))
usa_vitd <- usa_vitd[order(usa_vitd$age),]

write.csv(usa_vitd, "old_nhanes/usa_w_vitd.csv")

# Men
usa_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vitd")

usa_vitd <- subset(usa_vitd, select = c(age, HI))
usa_vitd <- usa_vitd[order(usa_vitd$age),]

write.csv(usa_vitd, "old_nhanes/usa_m_vitd.csv")


####################################################################################################################################
load(here("data", "processed", "Subnational distributions", "usa_2009_2010"))













####################################################################################################################################
load(here("data", "processed", "Subnational distributions", "usa_2011_2012"))

####################################################################################################################################

load(here("data", "processed", "Subnational distributions", "usa_2013_2014"))


####################################################################################################################################

load(here("data", "processed", "Subnational distributions", "usa_2015_2016"))


####################################################################################################################################
load(here("data", "processed", "Subnational distributions", "usa_2017_2018"))


####################################################################################################################################


