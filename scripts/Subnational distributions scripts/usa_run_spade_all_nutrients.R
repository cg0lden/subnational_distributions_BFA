# Running SPADE: usa data
# File created on 11/24/20 by Simone Passarelli
# Updated by Simone Passarelli on March 17 2021


# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "usa"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "usa"))
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

write.csv(usa_vitb12, "all_intakes/usa_w_vitb12.csv")

# Men
usa_vitb12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_vitb12")

usa_vitb12 <- subset(usa_vitb12, select = c(age, HI))
usa_vitb12 <- usa_vitb12[order(usa_vitb12$age),]

write.csv(usa_vitb12, "all_intakes/usa_m_vitb12.csv")

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

write.csv(usa_iron, "all_intakes/usa_w_iron.csv")

# Men
usa_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                   data=usa_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=80,
                   sex.lab="men",
                   weights.name = "weight1",
                   output.name = "usa_men_iron")

usa_iron <- subset(usa_iron, select = c(age, HI))
usa_iron <- usa_iron[order(usa_iron$age),]

write.csv(usa_iron, "all_intakes/usa_m_iron.csv")
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

write.csv(usa_zinc_w, "all_intakes/usa_w_zinc.csv")

# Men
usa_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_zinc")

usa_zinc_m <- subset(usa_zinc_m, select = c(age, HI))
usa_zinc_m <- usa_zinc_m[order(usa_zinc_m$age),]

write.csv(usa_zinc_m, "all_intakes/usa_m_zinc.csv")

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

write.csv(usa_vita, "all_intakes/usa_w_vita.csv")

# Men
usa_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vita")

usa_vita <- subset(usa_vita, select = c(age, HI))
usa_vita <- usa_vita[order(usa_vita$age),]

write.csv(usa_vita, "all_intakes/usa_m_vita.csv")

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

write.csv(usa_calc, "all_intakes/usa_w_calc.csv")

# Men
usa_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_calc")

usa_calc <- subset(usa_calc, select = c(age, HI))
usa_calc <- usa_calc[order(usa_calc$age),]

write.csv(usa_calc, "all_intakes/usa_m_calc.csv")


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

write.csv(usa_omega_3, "all_intakes/usa_w_omega_3.csv")

# Men
usa_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_omega_3")

usa_omega_3 <- subset(usa_omega_3, select = c(age, HI))
usa_omega_3 <- usa_omega_3[order(usa_omega_3$age),]

write.csv(usa_omega_3, "all_intakes/usa_m_omega_3.csv")

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

write.csv(usa_vite, "all_intakes/usa_w_vite.csv")

# Men
usa_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vite")

usa_vite <- subset(usa_vite, select = c(age, HI))
usa_vite <- usa_vite[order(usa_vite$age),]

write.csv(usa_vite, "all_intakes/usa_m_vite.csv")

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

write.csv(usa_energy, "all_intakes/usa_w_energy.csv")

# Men
usa_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_energy")

usa_energy <- subset(usa_energy, select = c(age, HI))
usa_energy <- usa_energy[order(usa_energy$age),]

write.csv(usa_energy, "all_intakes/usa_m_energy.csv")

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

write.csv(usa_protein, "all_intakes/usa_w_protein.csv")

# Men
usa_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_protein")

usa_protein <- subset(usa_protein, select = c(age, HI))
usa_protein <- usa_protein[order(usa_protein$age),]

write.csv(usa_protein, "all_intakes/usa_m_protein.csv")

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

write.csv(usa_carb, "all_intakes/usa_w_carb.csv")

# Men
usa_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_carb")

usa_carb <- subset(usa_carb, select = c(age, HI))
usa_carb <- usa_carb[order(usa_carb$age),]

write.csv(usa_carb, "all_intakes/usa_m_carb.csv")

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

write.csv(usa_sugar, "all_intakes/usa_w_sugar.csv")

# Men
usa_sugar <- f.spade(frml.ia=sugar~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_sugar")

usa_sugar <- subset(usa_sugar, select = c(age, HI))
usa_sugar <- usa_sugar[order(usa_sugar$age),]

write.csv(usa_sugar, "all_intakes/usa_m_sugar.csv")

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

write.csv(usa_fiber, "all_intakes/usa_w_fiber.csv")

# Men
usa_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fiber")

usa_fiber <- subset(usa_fiber, select = c(age, HI))
usa_fiber <- usa_fiber[order(usa_fiber$age),]

write.csv(usa_fiber, "all_intakes/usa_m_fiber.csv")

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

write.csv(usa_fat, "all_intakes/usa_w_fat.csv")

# Men
usa_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fat")

usa_fat <- subset(usa_fat, select = c(age, HI))
usa_fat <- usa_fat[order(usa_fat$age),]

write.csv(usa_fat, "all_intakes/usa_m_fat.csv")

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

write.csv(usa_satfat, "all_intakes/usa_w_satfat.csv")

# Men
usa_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_satfat")

usa_satfat <- subset(usa_satfat, select = c(age, HI))
usa_satfat <- usa_satfat[order(usa_satfat$age),]

write.csv(usa_satfat, "all_intakes/usa_m_satfat.csv")

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

write.csv(usa_mufa, "all_intakes/usa_w_mufa.csv")

# Men
usa_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_mufa")

usa_mufa <- subset(usa_mufa, select = c(age, HI))
usa_mufa <- usa_mufa[order(usa_mufa$age),]

write.csv(usa_mufa, "all_intakes/usa_m_mufa.csv")

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

write.csv(usa_pufa, "all_intakes/usa_w_pufa.csv")

# Men
usa_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_pufa")

usa_pufa <- subset(usa_pufa, select = c(age, HI))
usa_pufa <- usa_pufa[order(usa_pufa$age),]

write.csv(usa_pufa, "all_intakes/usa_m_pufa.csv")

##################################################################

# 17. RUN SPADE FOR RETINOL

usa_retinol <- f.spade(frml.ia=retinol~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_retinol")

usa_retinol <- subset(usa_retinol, select = c(age, HI))
usa_retinol <- usa_retinol[order(usa_retinol$age),]

write.csv(usa_retinol, "all_intakes/usa_w_retinol.csv")

# Men
usa_retinol <- f.spade(frml.ia=retinol~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_retinol")

usa_retinol <- subset(usa_retinol, select = c(age, HI))
usa_retinol <- usa_retinol[order(usa_retinol$age),]

write.csv(usa_retinol, "all_intakes/usa_m_retinol.csv")

##################################################################

# 18. RUN SPADE FOR ALPHA CAROTENE

usa_alphacarot <- f.spade(frml.ia=alphacarot~fp(age), frml.if=alphacarot~cs(age),
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_alphacarot")

usa_alphacarot <- subset(usa_alphacarot, select = c(age, HI))
usa_alphacarot <- usa_alphacarot[order(usa_alphacarot$age),]

write.csv(usa_alphacarot, "all_intakes/usa_w_alphacarot.csv")

# Men
usa_alphacarot <- f.spade(frml.ia=alphacarot~fp(age), frml.if=alphacarot~cs(age),
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_alphacarot")

usa_alphacarot <- subset(usa_alphacarot, select = c(age, HI))
usa_alphacarot <- usa_alphacarot[order(usa_alphacarot$age),]

write.csv(usa_alphacarot, "all_intakes/usa_m_alphacarot.csv")

##################################################################

# 19. RUN SPADE FOR BETA CAROTENE

usa_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_betacarot")

usa_betacarot <- subset(usa_betacarot, select = c(age, HI))
usa_betacarot <- usa_betacarot[order(usa_betacarot$age),]

write.csv(usa_betacarot, "all_intakes/usa_w_betacarot.csv")

# Men
usa_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_betacarot")

usa_betacarot <- subset(usa_betacarot, select = c(age, HI))
usa_betacarot <- usa_betacarot[order(usa_betacarot$age),]

write.csv(usa_betacarot, "all_intakes/usa_m_betacarot.csv")

##################################################################

# 20. RUN SPADE FOR BETACRYPT

usa_betacrypt <- f.spade(frml.ia=betacrypt~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_betacrypt")

usa_betacrypt <- subset(usa_betacrypt, select = c(age, HI))
usa_betacrypt <- usa_betacrypt[order(usa_betacrypt$age),]

write.csv(usa_betacrypt, "all_intakes/usa_w_betacrypt.csv")

# Men
usa_betacrypt <- f.spade(frml.ia=betacrypt~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_betacrypt")

usa_betacrypt <- subset(usa_betacrypt, select = c(age, HI))
usa_betacrypt <- usa_betacrypt[order(usa_betacrypt$age),]

write.csv(usa_betacrypt, "all_intakes/usa_m_betacrypt.csv")

##################################################################

# 21. RUN SPADE FOR LYCOPENE

usa_lyco <- f.spade(frml.ia=lyco~fp(age), frml.if=lyco~cs(age),
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_lyco")

usa_lyco <- subset(usa_lyco, select = c(age, HI))
usa_lyco <- usa_lyco[order(usa_lyco$age),]

write.csv(usa_lyco, "all_intakes/usa_w_lyco.csv")

# Men
usa_lyco <- f.spade(frml.ia=lyco~fp(age), frml.if=lyco~cs(age),
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_lyco")

usa_lyco <- subset(usa_lyco, select = c(age, HI))
usa_lyco <- usa_lyco[order(usa_lyco$age),]

write.csv(usa_lyco, "all_intakes/usa_m_lyco.csv")

##################################################################

# 22. RUN SPADE FOR CAFFEINE

usa_caff <- f.spade(frml.ia=caff~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_caff")

usa_caff <- subset(usa_caff, select = c(age, HI))
usa_caff <- usa_caff[order(usa_caff$age),]

write.csv(usa_caff, "all_intakes/usa_w_caff.csv")

# Men
usa_caff <- f.spade(frml.ia=caff~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_caff")

usa_caff <- subset(usa_caff, select = c(age, HI))
usa_caff <- usa_caff[order(usa_caff$age),]

write.csv(usa_caff, "all_intakes/usa_m_caff.csv")

##################################################################

# 23. RUN SPADE FOR lutein+zeaxanthin

usa_lutzea <- f.spade(frml.ia=lutzea~fp(age), frml.if="no.if", 
                      data=usa_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=80,
                      sex.lab="women",
                      weights.name = "weight1",
                      output.name = "usa_wom_lutzea")

usa_lutzea <- subset(usa_lutzea, select = c(age, HI))
usa_lutzea <- usa_lutzea[order(usa_lutzea$age),]

write.csv(usa_lutzea, "all_intakes/usa_w_lutzea.csv")

# Men
usa_lutzea <- f.spade(frml.ia=lutzea~fp(age), frml.if="no.if", 
                      data=usa_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=80,
                      sex.lab="men",
                      weights.name = "weight1",
                      output.name = "usa_men_lutzea")

usa_lutzea <- subset(usa_lutzea, select = c(age, HI))
usa_lutzea <- usa_lutzea[order(usa_lutzea$age),]

write.csv(usa_lutzea, "all_intakes/usa_m_lutzea.csv")
usa_lutzea <- subset(usa_lutzea, select = c(age, HI))
usa_lutzea <- usa_lutzea[order(usa_lutzea$age),]

write.csv(usa_lutzea, "all_intakes/usa_m_lutzea.csv")

##################################################################

# 24. RUN SPADE FOR THEOBROMINE

usa_theo <- f.spade(frml.ia=theo~fp(age), frml.if=theo ~cs(age),
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_theo")

usa_theo <- subset(usa_theo, select = c(age, HI))
usa_theo <- usa_theo[order(usa_theo$age),]

write.csv(usa_theo, "all_intakes/usa_w_theo.csv")

# Men
usa_theo <- f.spade(frml.ia=theo~fp(age), frml.if=theo ~cs(age),
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_theo")

usa_theo <- subset(usa_theo, select = c(age, HI))
usa_theo <- usa_theo[order(usa_theo$age),]

write.csv(usa_theo, "all_intakes/usa_m_theo.csv")

##################################################################

# 25. RUN SPADE FOR ALCOHOL

usa_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_alcohol")

usa_alcohol <- subset(usa_alcohol, select = c(age, HI))
usa_alcohol <- usa_alcohol[order(usa_alcohol$age),]

write.csv(usa_alcohol, "all_intakes/usa_w_alcohol.csv")

# Men
usa_alcohol <- f.spade(frml.ia=alcohol~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_alcohol")

usa_alcohol <- subset(usa_alcohol, select = c(age, HI))
usa_alcohol <- usa_alcohol[order(usa_alcohol$age),]

write.csv(usa_alcohol, "all_intakes/usa_m_alcohol.csv")

##################################################################

# 26. RUN SPADE FOR THIAMIN

usa_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=usa_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=80,
                       sex.lab="women",
                       weights.name = "weight1",
                       output.name = "usa_wom_thia")

usa_thia <- subset(usa_thia, select = c(age, HI))
usa_thia <- usa_thia[order(usa_thia$age),]

write.csv(usa_thia, "all_intakes/usa_w_thia.csv")

# Men
usa_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                       data=usa_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=80,
                       sex.lab="men",
                       weights.name = "weight1",
                       output.name = "usa_men_thia")

usa_thia <- subset(usa_thia, select = c(age, HI))
usa_thia <- usa_thia[order(usa_thia$age),]

write.csv(usa_thia, "all_intakes/usa_m_thia.csv")

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

write.csv(usa_ribo, "all_intakes/usa_w_ribo.csv")

# Men
usa_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_ribo")

usa_ribo <- subset(usa_ribo, select = c(age, HI))
usa_ribo <- usa_ribo[order(usa_ribo$age),]

write.csv(usa_ribo, "all_intakes/usa_m_ribo.csv")

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

write.csv(usa_niac, "all_intakes/usa_w_niac.csv")

# Men
usa_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_niac")

usa_niac <- subset(usa_niac, select = c(age, HI))
usa_niac <- usa_niac[order(usa_niac$age),]

write.csv(usa_niac, "all_intakes/usa_m_niac.csv")

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

write.csv(usa_vitb6, "all_intakes/usa_w_vitb6.csv")

# Men
usa_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vitb6")

usa_vitb6 <- subset(usa_vitb6, select = c(age, HI))
usa_vitb6 <- usa_vitb6[order(usa_vitb6$age),]

write.csv(usa_vitb6, "all_intakes/usa_m_vitb6.csv")

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

write.csv(usa_fola, "all_intakes/usa_w_fola.csv")

# Men
usa_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_fola")

usa_fola <- subset(usa_fola, select = c(age, HI))
usa_fola <- usa_fola[order(usa_fola$age),]

write.csv(usa_fola, "all_intakes/usa_m_fola.csv")

##################################################################

# 31. RUN SPADE FOR CHOLINE

usa_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_chol")

usa_chol <- subset(usa_chol, select = c(age, HI))
usa_chol <- usa_chol[order(usa_chol$age),]

write.csv(usa_chol, "all_intakes/usa_w_chol.csv")

# Men
usa_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_chol")

usa_chol <- subset(usa_chol, select = c(age, HI))
usa_chol <- usa_chol[order(usa_chol$age),]

write.csv(usa_chol, "all_intakes/usa_m_chol.csv")

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

write.csv(usa_vitd, "all_intakes/usa_w_vitd.csv")

# Men
usa_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vitd")

usa_vitd <- subset(usa_vitd, select = c(age, HI))
usa_vitd <- usa_vitd[order(usa_vitd$age),]

write.csv(usa_vitd, "all_intakes/usa_m_vitd.csv")

##################################################################

# 33. RUN SPADE FOR VITAMIN K

usa_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_vitk")

usa_vitk <- subset(usa_vitk, select = c(age, HI))
usa_vitk <- usa_vitk[order(usa_vitk$age),]

write.csv(usa_vitk, "all_intakes/usa_w_vitk.csv")

# Men
usa_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_vitk")

usa_vitk <- subset(usa_vitk, select = c(age, HI))
usa_vitk <- usa_vitk[order(usa_vitk$age),]

write.csv(usa_vitk, "all_intakes/usa_m_vitk.csv")

##################################################################

# 34. RUN SPADE FOR PHOSPHORUS

usa_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_phos")

usa_phos <- subset(usa_phos, select = c(age, HI))
usa_phos <- usa_phos[order(usa_phos$age),]

write.csv(usa_phos, "all_intakes/usa_w_phos.csv")

# Men
usa_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_phos")

usa_phos <- subset(usa_phos, select = c(age, HI))
usa_phos <- usa_phos[order(usa_phos$age),]

write.csv(usa_phos, "all_intakes/usa_m_phos.csv")

##################################################################

# 35. RUN SPADE FOR MG

usa_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_mg")

usa_mg <- subset(usa_mg, select = c(age, HI))
usa_mg <- usa_mg[order(usa_mg$age),]

write.csv(usa_mg, "all_intakes/usa_w_mg.csv")

# Men
usa_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_mg")

usa_mg <- subset(usa_mg, select = c(age, HI))
usa_mg <- usa_mg[order(usa_mg$age),]

write.csv(usa_mg, "all_intakes/usa_m_mg.csv")

##################################################################

# 36. RUN SPADE FOR COPPER

usa_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_cu")

usa_cu <- subset(usa_cu, select = c(age, HI))
usa_cu <- usa_cu[order(usa_cu$age),]

write.csv(usa_cu, "all_intakes/usa_w_cu.csv")

# Men
usa_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_cu")

usa_cu <- subset(usa_cu, select = c(age, HI))
usa_cu <- usa_cu[order(usa_cu$age),]

write.csv(usa_cu, "all_intakes/usa_m_cu.csv")

##################################################################

# 37. RUN SPADE FOR SODIUM

usa_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_na")

usa_na <- subset(usa_na, select = c(age, HI))
usa_na <- usa_na[order(usa_na$age),]

write.csv(usa_na, "all_intakes/usa_w_na.csv")

# Men
usa_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_na")

usa_na <- subset(usa_na, select = c(age, HI))
usa_na <- usa_na[order(usa_na$age),]

write.csv(usa_na, "all_intakes/usa_m_na.csv")

##################################################################

# 38. RUN SPADE FOR POTASSIUM

usa_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                  data=usa_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=80,
                  sex.lab="women",
                  weights.name = "weight1",
                  output.name = "usa_wom_pota")

usa_pota <- subset(usa_pota, select = c(age, HI))
usa_pota <- usa_pota[order(usa_pota$age),]

write.csv(usa_pota, "all_intakes/usa_w_pota.csv")

# Men
usa_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                  data=usa_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=0, max.age=80,
                  sex.lab="men",
                  weights.name = "weight1",
                  output.name = "usa_men_pota")

usa_pota <- subset(usa_pota, select = c(age, HI))
usa_pota <- usa_pota[order(usa_pota$age),]

write.csv(usa_pota, "all_intakes/usa_m_pota.csv")

##################################################################

# 39. RUN SPADE FOR SELENIUM

usa_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                    data=usa_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="women",
                    weights.name = "weight1",
                    output.name = "usa_wom_se")

usa_se <- subset(usa_se, select = c(age, HI))
usa_se <- usa_se[order(usa_se$age),]

write.csv(usa_se, "all_intakes/usa_w_se.csv")

# Men
usa_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                    data=usa_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=80,
                    sex.lab="men",
                    weights.name = "weight1",
                    output.name = "usa_men_se")

usa_se <- subset(usa_se, select = c(age, HI))
usa_se <- usa_se[order(usa_se$age),]

write.csv(usa_se, "all_intakes/usa_m_se.csv")
