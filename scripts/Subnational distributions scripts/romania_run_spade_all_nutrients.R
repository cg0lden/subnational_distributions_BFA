# Running SPADE: rom data
# File created on 12/7/20 by Simone Passarelli
# Updated by SP March 2021
# There is no zinc for rom

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "rom"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Romania"))

###########################################################
# Remove missing obs
summary(rom_spade)

# Make separate datasets for men and women
rom_wom <- subset(rom_spade, sex==2)
rom_men <- subset(rom_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(rom_wom$age)
range(rom_men$age)

#round the age variables down
rom_wom$age <- floor(rom_wom$age)
rom_men$age <- floor(rom_men$age)

# number of intakes per person:
table(rom_wom$age)

# Women
rom_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                     data=rom_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=92,
                     sex.lab="women", 
                     output.name = "rom_wom_b12")

rom_b12 <- subset(rom_b12, select = c(age, HI))
rom_b12 <- rom_b12[order(rom_b12$age),]

write.csv(rom_b12, "all_intakes/rom_w_b12.csv")

# Men
rom_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                     data=rom_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=19, max.age=88,
                     sex.lab="men",
                     output.name = "rom_men_b12")

rom_b12 <- subset(rom_b12, select = c(age, HI))
rom_b12 <- rom_b12[order(rom_b12$age),]

write.csv(rom_b12, "all_intakes/rom_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
rom_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      sex.lab="women",
                      output.name = "rom_wom_iron")

rom_iron <- subset(rom_iron, select = c(age, HI))
rom_iron <- rom_iron[order(rom_iron$age),]

write.csv(rom_iron, "all_intakes/rom_w_iron.csv")

# Men
rom_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "rom_men_iron")

rom_iron <- subset(rom_iron, select = c(age, HI))
rom_iron <- rom_iron[order(rom_iron$age),]

write.csv(rom_iron, "all_intakes/rom_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
 rom_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=rom_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=92,
                       sex.lab="women",
                       output.name = "rom_wom_zinc")
 
 rom_zinc_w <- subset(rom_zinc_w, select = c(age, HI))
 rom_zinc_w <- rom_zinc_w[order(rom_zinc_w$age),]
 
 write.csv(rom_zinc_w, "all_intakes/rom_w_zinc.csv")
 
#  Men
 rom_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                       data=rom_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=19, max.age=88,
                       sex.lab="men",
                       output.name = "rom_men_zinc")
 
 rom_zinc_m <- subset(rom_zinc_m, select = c(age, HI))
 rom_zinc_m <- rom_zinc_m[order(rom_zinc_m$age),]
 
 write.csv(rom_zinc_m, "all_intakes/rom_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

rom_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      sex.lab="women",
                      output.name = "rom_wom_vita")

rom_vita <- subset(rom_vita, select = c(age, HI))
rom_vita <- rom_vita[order(rom_vita$age),]

write.csv(rom_vita, "all_intakes/rom_w_vita.csv")

# Men
rom_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "rom_men_vita")

rom_vita <- subset(rom_vita, select = c(age, HI))
rom_vita <- rom_vita[order(rom_vita$age),]

write.csv(rom_vita, "all_intakes/rom_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

rom_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      sex.lab="women",
                      output.name = "rom_wom_calc")

rom_calc <- subset(rom_calc, select = c(age, HI))
rom_calc <- rom_calc[order(rom_calc$age),]

write.csv(rom_calc, "all_intakes/rom_w_calc.csv")

# Men
rom_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "rom_men_calc")

rom_calc <- subset(rom_calc, select = c(age, HI))
rom_calc <- rom_calc[order(rom_calc$age),]

write.csv(rom_calc, "all_intakes/rom_m_calc.csv")

##################################################################

# 6. RUN SPADE FOR OMEGA 3 

rom_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                         data=rom_wom, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=92,
                         sex.lab="women",
                         output.name = "rom_wom_omega_3")

rom_omega_3 <- subset(rom_omega_3, select = c(age, HI))
rom_omega_3 <- rom_omega_3[order(rom_omega_3$age),]

write.csv(rom_omega_3, "all_intakes/rom_w_omega_3.csv")

# Men
rom_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                         data=rom_men, seed=123,  backtrans.nr = 3,
                         dgts.distr = 2, min.age=19, max.age=88,
                         sex.lab="men",
                         output.name = "rom_men_omega_3")

rom_omega_3 <- subset(rom_omega_3, select = c(age, HI))
rom_omega_3 <- rom_omega_3[order(rom_omega_3$age),]

write.csv(rom_omega_3, "all_intakes/rom_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR VITAMIN C

rom_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_vitc")

rom_vitc <- subset(rom_vitc, select = c(age, HI))
rom_vitc <- rom_vitc[order(rom_vitc$age),]

write.csv(rom_vitc, "all_intakes/rom_w_vitc.csv")

# Men
rom_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_vitc")

rom_vitc <- subset(rom_vitc, select = c(age, HI))
rom_vitc <- rom_vitc[order(rom_vitc$age),]

write.csv(rom_vitc, "all_intakes/rom_m_vitc.csv")

##################################################################

# 8. RUN SPADE FOR NIACIN

rom_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_niac")

rom_niac <- subset(rom_niac, select = c(age, HI))
rom_niac <- rom_niac[order(rom_niac$age),]

write.csv(rom_niac, "all_intakes/rom_w_niac.csv")

# Men
rom_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_niac")

rom_niac <- subset(rom_niac, select = c(age, HI))
rom_niac <- rom_niac[order(rom_niac$age),]

write.csv(rom_niac, "all_intakes/rom_m_niac.csv")

##################################################################

# 9. RUN SPADE FOR RIBOFLAVIN

rom_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_ribo")

rom_ribo <- subset(rom_ribo, select = c(age, HI))
rom_ribo <- rom_ribo[order(rom_ribo$age),]

write.csv(rom_ribo, "all_intakes/rom_w_ribo.csv")

# Men
rom_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_ribo")

rom_ribo <- subset(rom_ribo, select = c(age, HI))
rom_ribo <- rom_ribo[order(rom_ribo$age),]

write.csv(rom_ribo, "all_intakes/rom_m_ribo.csv")

##################################################################

# 10. RUN SPADE FOR BETA CAROTENE

rom_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_betacarot")

rom_betacarot <- subset(rom_betacarot, select = c(age, HI))
rom_betacarot <- rom_betacarot[order(rom_betacarot$age),]

write.csv(rom_betacarot, "all_intakes/rom_w_betacarot.csv")

# Men
rom_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_betacarot")

rom_betacarot <- subset(rom_betacarot, select = c(age, HI))
rom_betacarot <- rom_betacarot[order(rom_betacarot$age),]

write.csv(rom_betacarot, "all_intakes/rom_m_betacarot.csv")

##################################################################

# 11. RUN SPADE FOR THIAMIN

rom_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_thia")

rom_thia <- subset(rom_thia, select = c(age, HI))
rom_thia <- rom_thia[order(rom_thia$age),]

write.csv(rom_thia, "all_intakes/rom_w_thia.csv")

# Men
rom_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_thia")

rom_thia <- subset(rom_thia, select = c(age, HI))
rom_thia <- rom_thia[order(rom_thia$age),]

write.csv(rom_thia, "all_intakes/rom_m_thia.csv")

##################################################################

# 11. RUN SPADE FOR VITAMIN B6

rom_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_vitb6")

rom_vitb6 <- subset(rom_vitb6, select = c(age, HI))
rom_vitb6 <- rom_vitb6[order(rom_vitb6$age),]

write.csv(rom_vitb6, "all_intakes/rom_w_vitb6.csv")

# Men
rom_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_vitb6")

rom_vitb6 <- subset(rom_vitb6, select = c(age, HI))
rom_vitb6 <- rom_vitb6[order(rom_vitb6$age),]

write.csv(rom_vitb6, "all_intakes/rom_m_vitb6.csv")

##################################################################

# 12. RUN SPADE FOR FOLATE

rom_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_fola")

rom_fola <- subset(rom_fola, select = c(age, HI))
rom_fola <- rom_fola[order(rom_fola$age),]

write.csv(rom_fola, "all_intakes/rom_w_fola.csv")

# Men
rom_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_fola")

rom_fola <- subset(rom_fola, select = c(age, HI))
rom_fola <- rom_fola[order(rom_fola$age),]

write.csv(rom_fola, "all_intakes/rom_m_fola.csv")

##################################################################

# 13. RUN SPADE FOR PLANT OMEGA 3 FATTY ACIDS

rom_plantomega3 <- f.spade(frml.ia=plantomega3~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_plantomega3")

rom_plantomega3 <- subset(rom_plantomega3, select = c(age, HI))
rom_plantomega3 <- rom_plantomega3[order(rom_plantomega3$age),]

write.csv(rom_plantomega3, "all_intakes/rom_w_plantomega3.csv")

# Men
rom_plantomega3 <- f.spade(frml.ia=plantomega3~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_plantomega3")

rom_plantomega3 <- subset(rom_plantomega3, select = c(age, HI))
rom_plantomega3 <- rom_plantomega3[order(rom_plantomega3$age),]

write.csv(rom_plantomega3, "all_intakes/rom_m_plantomega3.csv")

##################################################################

# 14. RUN SPADE FOR TRANS FAT

rom_tfat <- f.spade(frml.ia=tfat~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_tfat")

rom_tfat <- subset(rom_tfat, select = c(age, HI))
rom_tfat <- rom_tfat[order(rom_tfat$age),]

write.csv(rom_tfat, "all_intakes/rom_w_tfat.csv")

# Men
rom_tfat <- f.spade(frml.ia=tfat~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_tfat")

rom_tfat <- subset(rom_tfat, select = c(age, HI))
rom_tfat <- rom_tfat[order(rom_tfat$age),]

write.csv(rom_tfat, "all_intakes/rom_m_tfat.csv")

##################################################################

# 15. RUN SPADE FOR TRANS CHOLINE

rom_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_chol")

rom_chol <- subset(rom_chol, select = c(age, HI))
rom_chol <- rom_chol[order(rom_chol$age),]

write.csv(rom_chol, "all_intakes/rom_w_chol.csv")

# Men
rom_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_chol")

rom_chol <- subset(rom_chol, select = c(age, HI))
rom_chol <- rom_chol[order(rom_chol$age),]

write.csv(rom_chol, "all_intakes/rom_m_chol.csv")

##################################################################

# 16. RUN SPADE FOR MG

rom_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_mg")

rom_mg <- subset(rom_mg, select = c(age, HI))
rom_mg <- rom_mg[order(rom_mg$age),]

write.csv(rom_mg, "all_intakes/rom_w_mg.csv")

# Men
rom_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_mg")

rom_mg <- subset(rom_mg, select = c(age, HI))
rom_mg <- rom_mg[order(rom_mg$age),]

write.csv(rom_mg, "all_intakes/rom_m_mg.csv")

##################################################################

# 17. RUN SPADE FOR PHOSPHORUS

rom_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_phos")

rom_phos <- subset(rom_phos, select = c(age, HI))
rom_phos <- rom_phos[order(rom_phos$age),]

write.csv(rom_phos, "all_intakes/rom_w_phos.csv")

# Men
rom_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_phos")

rom_phos <- subset(rom_phos, select = c(age, HI))
rom_phos <- rom_phos[order(rom_phos$age),]

write.csv(rom_phos, "all_intakes/rom_m_phos.csv")

##################################################################

# 18. RUN SPADE FOR POTASSIUM

rom_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_pota")

rom_pota <- subset(rom_pota, select = c(age, HI))
rom_pota <- rom_pota[order(rom_pota$age),]

write.csv(rom_pota, "all_intakes/rom_w_pota.csv")

# Men
rom_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_pota")

rom_pota <- subset(rom_pota, select = c(age, HI))
rom_pota <- rom_pota[order(rom_pota$age),]

write.csv(rom_pota, "all_intakes/rom_m_pota.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN D

rom_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_vitd")

rom_vitd <- subset(rom_vitd, select = c(age, HI))
rom_vitd <- rom_vitd[order(rom_vitd$age),]

write.csv(rom_vitd, "all_intakes/rom_w_vitd.csv")

# Men
rom_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_vitd")

rom_vitd <- subset(rom_vitd, select = c(age, HI))
rom_vitd <- rom_vitd[order(rom_vitd$age),]

write.csv(rom_vitd, "all_intakes/rom_m_vitd.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN E

rom_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_vite")

rom_vite <- subset(rom_vite, select = c(age, HI))
rom_vite <- rom_vite[order(rom_vite$age),]

write.csv(rom_vite, "all_intakes/rom_w_vite.csv")

# Men
rom_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_vite")

rom_vite <- subset(rom_vite, select = c(age, HI))
rom_vite <- rom_vite[order(rom_vite$age),]

write.csv(rom_vite, "all_intakes/rom_m_vite.csv")

##################################################################

# 21. RUN SPADE FOR OMEGA 6

rom_omega6 <- f.spade(frml.ia=omega6~fp(age), frml.if="no.if", 
                    data=rom_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=92,
                    sex.lab="women",
                    output.name = "rom_wom_omega6")

rom_omega6 <- subset(rom_omega6, select = c(age, HI))
rom_omega6 <- rom_omega6[order(rom_omega6$age),]

write.csv(rom_omega6, "all_intakes/rom_w_omega6.csv")

# Men
rom_omega6 <- f.spade(frml.ia=omega6~fp(age), frml.if="no.if", 
                    data=rom_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=19, max.age=88,
                    sex.lab="men",
                    output.name = "rom_men_omega6")

rom_omega6 <- subset(rom_omega6, select = c(age, HI))
rom_omega6 <- rom_omega6[order(rom_omega6$age),]

write.csv(rom_omega6, "all_intakes/rom_m_omega6.csv")

##################################################################

# 22. RUN SPADE FOR SODIUM

rom_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      sex.lab="women",
                      output.name = "rom_wom_na")

rom_na <- subset(rom_na, select = c(age, HI))
rom_na <- rom_na[order(rom_na$age),]

write.csv(rom_na, "all_intakes/rom_w_na.csv")

# Men
rom_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "rom_men_na")

rom_na <- subset(rom_na, select = c(age, HI))
rom_na <- rom_na[order(rom_na$age),]

write.csv(rom_na, "all_intakes/rom_m_na.csv")

##################################################################

# 22. RUN SPADE FOR COPPER

rom_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                      data=rom_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=92,
                      sex.lab="women",
                      output.name = "rom_wom_cu")

rom_cu <- subset(rom_cu, select = c(age, HI))
rom_cu <- rom_cu[order(rom_cu$age),]

write.csv(rom_cu, "all_intakes/rom_w_cu.csv")

# Men
rom_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                      data=rom_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=19, max.age=88,
                      sex.lab="men",
                      output.name = "rom_men_cu")

rom_cu <- subset(rom_cu, select = c(age, HI))
rom_cu <- rom_cu[order(rom_cu$age),]

write.csv(rom_cu, "all_intakes/rom_m_cu.csv")

##################################################################

# 23. RUN SPADE FOR SELENIUM

rom_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_se")

rom_se <- subset(rom_se, select = c(age, HI))
rom_se <- rom_se[order(rom_se$age),]

write.csv(rom_se, "all_intakes/rom_w_se.csv")

# Men
rom_se <- f.spade(frml.ia=se~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_se")

rom_se <- subset(rom_se, select = c(age, HI))
rom_se <- rom_se[order(rom_se$age),]

write.csv(rom_se, "all_intakes/rom_m_se.csv")

##################################################################

# 23. RUN SPADE FOR VITAMIN K

rom_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_vitk")

rom_vitk <- subset(rom_vitk, select = c(age, HI))
rom_vitk <- rom_vitk[order(rom_vitk$age),]

write.csv(rom_vitk, "all_intakes/rom_w_vitk.csv")

# Men
rom_vitk <- f.spade(frml.ia=vitk~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_vitk")

rom_vitk <- subset(rom_vitk, select = c(age, HI))
rom_vitk <- rom_vitk[order(rom_vitk$age),]

write.csv(rom_vitk, "all_intakes/rom_m_vitk.csv")

##################################################################

# 24. RUN SPADE FOR ENERGY

rom_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_energy")

rom_energy <- subset(rom_energy, select = c(age, HI))
rom_energy <- rom_energy[order(rom_energy$age),]

write.csv(rom_energy, "all_intakes/rom_w_energy.csv")

# Men
rom_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_energy")

rom_energy <- subset(rom_energy, select = c(age, HI))
rom_energy <- rom_energy[order(rom_energy$age),]

write.csv(rom_energy, "all_intakes/rom_m_energy.csv")

##################################################################

# 25. RUN SPADE FOR PROTEIN

rom_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_protein")

rom_protein <- subset(rom_protein, select = c(age, HI))
rom_protein <- rom_protein[order(rom_protein$age),]

write.csv(rom_protein, "all_intakes/rom_w_protein.csv")

# Men
rom_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_protein")

rom_protein <- subset(rom_protein, select = c(age, HI))
rom_protein <- rom_protein[order(rom_protein$age),]

write.csv(rom_protein, "all_intakes/rom_m_protein.csv")

##################################################################

# 26. RUN SPADE FOR CARB

rom_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_carb")

rom_carb <- subset(rom_carb, select = c(age, HI))
rom_carb <- rom_carb[order(rom_carb$age),]

write.csv(rom_carb, "all_intakes/rom_w_carb.csv")

# Men
rom_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_carb")

rom_carb <- subset(rom_carb, select = c(age, HI))
rom_carb <- rom_carb[order(rom_carb$age),]

write.csv(rom_carb, "all_intakes/rom_m_carb.csv")

##################################################################

# 27. RUN SPADE FOR FIBER

rom_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_fiber")

rom_fiber <- subset(rom_fiber, select = c(age, HI))
rom_fiber <- rom_fiber[order(rom_fiber$age),]

write.csv(rom_fiber, "all_intakes/rom_w_fiber.csv")

# Men
rom_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_fiber")

rom_fiber <- subset(rom_fiber, select = c(age, HI))
rom_fiber <- rom_fiber[order(rom_fiber$age),]

write.csv(rom_fiber, "all_intakes/rom_m_fiber.csv")

##################################################################

# 28. RUN SPADE FOR FAT

rom_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_fat")

rom_fat <- subset(rom_fat, select = c(age, HI))
rom_fat <- rom_fat[order(rom_fat$age),]

write.csv(rom_fat, "all_intakes/rom_w_fat.csv")

# Men
rom_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_fat")

rom_fat <- subset(rom_fat, select = c(age, HI))
rom_fat <- rom_fat[order(rom_fat$age),]

write.csv(rom_fat, "all_intakes/rom_m_fat.csv")

##################################################################

# 29. RUN SPADE FOR SATURATED FAT

rom_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_satfat")

rom_satfat <- subset(rom_satfat, select = c(age, HI))
rom_satfat <- rom_satfat[order(rom_satfat$age),]

write.csv(rom_satfat, "all_intakes/rom_w_satfat.csv")

# Men
rom_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_satfat")

rom_satfat <- subset(rom_satfat, select = c(age, HI))
rom_satfat <- rom_satfat[order(rom_satfat$age),]

write.csv(rom_satfat, "all_intakes/rom_m_satfat.csv")

##################################################################

# 30. RUN SPADE FOR MUFA

rom_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_mufa")

rom_mufa <- subset(rom_mufa, select = c(age, HI))
rom_mufa <- rom_mufa[order(rom_mufa$age),]

write.csv(rom_mufa, "all_intakes/rom_w_mufa.csv")

# Men
rom_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_mufa")

rom_mufa <- subset(rom_mufa, select = c(age, HI))
rom_mufa <- rom_mufa[order(rom_mufa$age),]

write.csv(rom_mufa, "all_intakes/rom_m_mufa.csv")

##################################################################

# 31. RUN SPADE FOR PUFA

rom_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                  data=rom_wom, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=92,
                  sex.lab="women",
                  output.name = "rom_wom_pufa")

rom_pufa <- subset(rom_pufa, select = c(age, HI))
rom_pufa <- rom_pufa[order(rom_pufa$age),]

write.csv(rom_pufa, "all_intakes/rom_w_pufa.csv")

# Men
rom_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                  data=rom_men, seed=123,  backtrans.nr = 3,
                  dgts.distr = 2, min.age=19, max.age=88,
                  sex.lab="men",
                  output.name = "rom_men_pufa")

rom_pufa <- subset(rom_pufa, select = c(age, HI))
rom_pufa <- rom_pufa[order(rom_pufa$age),]

write.csv(rom_pufa, "all_intakes/rom_m_pufa.csv")