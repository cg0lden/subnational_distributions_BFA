# Running SPADE: italy data
# File created on 12/7/20 by Simone Passarelli
# Edited by Simone Passarelli on 2/22/21 
# There is no zinc for italy

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "italy"))
SPADE.OUTPUT.PATH <- (here("output", "Subnational distributions", "Italy"))

###########################################################
# Remove missing obs
summary(italy_spade)

# Make separate datasets for men and women
italy_wom <- subset(italy_spade, sex==2)
italy_men <- subset(italy_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(italy_wom$age)
range(italy_men$age)

#round the age variables down
italy_wom$age <- floor(italy_wom$age)
italy_men$age <- floor(italy_men$age)

# number of intakes per person:
table(italy_wom$age)

# Women
italy_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                   data=italy_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=97,
                   sex.lab="women", 
                   output.name = "italy_wom_b12")

italy_b12 <- subset(italy_b12, select = c(age, HI))
italy_b12 <- italy_b12[order(italy_b12$age),]

write.csv(italy_b12, "all_intakes/italy_w_b12.csv")

# Men
italy_b12 <- f.spade(frml.ia=vitb12~fp(age), frml.if="no.if", 
                   data=italy_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=0, max.age=92,
                   sex.lab="men",
                   output.name = "italy_men_b12")

italy_b12 <- subset(italy_b12, select = c(age, HI))
italy_b12 <- italy_b12[order(italy_b12$age),]

write.csv(italy_b12, "all_intakes/italy_m_b12.csv")

##################################################################

# 2. RUN SPADE FOR IRON
# Women
italy_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    sex.lab="women",
                    output.name = "italy_wom_iron")

italy_iron <- subset(italy_iron, select = c(age, HI))
italy_iron <- italy_iron[order(italy_iron$age),]

write.csv(italy_iron, "all_intakes/italy_w_iron.csv")

# Men
italy_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    sex.lab="men",
                    output.name = "italy_men_iron")

italy_iron <- subset(italy_iron, select = c(age, HI))
italy_iron <- italy_iron[order(italy_iron$age),]

write.csv(italy_iron, "all_intakes/italy_m_iron.csv")
##################################################################
# 
# 3. RUN SPADE FOR VITAMIN C
 italy_vitc_w <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       sex.lab="women",
                       output.name = "italy_wom_vitc")
 
 italy_vitc_w <- subset(italy_vitc_w, select = c(age, HI))
 italy_vitc_w <- italy_vitc_w[order(italy_vitc_w$age),]
 
 write.csv(italy_vitc_w, "all_intakes/italy_w_vitc.csv")
 
  # Men
 italy_vitc_m <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       sex.lab="men",
                       output.name = "italy_men_vitc")
 
 italy_vitc_m <- subset(italy_vitc_m, select = c(age, HI))
 italy_vitc_m <- italy_vitc_m[order(italy_vitc_m$age),]
 
 write.csv(italy_vitc_m, "all_intakes/italy_m_vitc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

italy_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    sex.lab="women",
                    output.name = "italy_wom_vita")

italy_vita <- subset(italy_vita, select = c(age, HI))
italy_vita <- italy_vita[order(italy_vita$age),]

write.csv(italy_vita, "all_intakes/italy_w_vita.csv")

# Men
italy_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    sex.lab="men",
                    output.name = "italy_men_vita")

italy_vita <- subset(italy_vita, select = c(age, HI))
italy_vita <- italy_vita[order(italy_vita$age),]

write.csv(italy_vita, "all_intakes/italy_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    sex.lab="women",
                    output.name = "italy_wom_calc")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_w_calc.csv")

# Men
italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    sex.lab="men",
                    output.name = "italy_men_calc")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_m_calc.csv")

##################################################################

# 6. RUN SPADE FOR OMEGA 3 


italy_omega_3 <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       sex.lab="women",
                       output.name = "italy_wom_omega_3")

italy_omega_3 <- subset(italy_omega_3, select = c(age, HI))
italy_omega_3 <- italy_omega_3[order(italy_omega_3$age),]

write.csv(italy_omega_3, "all_intakes/italy_w_omega_3.csv")

# Men
italy_omega_3 <- f.spade(frml.ia=omega_3~fp(age),  frml.if=omega_3~cs(age),
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       sex.lab="men",
                       output.name = "italy_men_omega_3")

italy_omega_3 <- subset(italy_omega_3, select = c(age, HI))
italy_omega_3 <- italy_omega_3[order(italy_omega_3$age),]

write.csv(italy_omega_3, "all_intakes/italy_m_omega_3.csv")

##################################################################

# 7. RUN SPADE FOR CALCIUM

italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_calc")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_w_calc.csv")

# Men
italy_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_calc")

italy_calc <- subset(italy_calc, select = c(age, HI))
italy_calc <- italy_calc[order(italy_calc$age),]

write.csv(italy_calc, "all_intakes/italy_m_calc.csv")

##################################################################

# 8. RUN SPADE FOR THIAMIN

italy_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_thia")

italy_thia <- subset(italy_thia, select = c(age, HI))
italy_thia <- italy_thia[order(italy_thia$age),]

write.csv(italy_thia, "all_intakes/italy_w_thia.csv")

# Men
italy_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_thia")

italy_thia <- subset(italy_thia, select = c(age, HI))
italy_thia <- italy_thia[order(italy_thia$age),]

write.csv(italy_thia, "all_intakes/italy_m_thia.csv")

##################################################################

# 9. RUN SPADE FOR RIBOFLAVIN

italy_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_ribo")

italy_ribo <- subset(italy_ribo, select = c(age, HI))
italy_ribo <- italy_ribo[order(italy_ribo$age),]

write.csv(italy_ribo, "all_intakes/italy_w_ribo.csv")

# Men
italy_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_ribo")

italy_ribo <- subset(italy_ribo, select = c(age, HI))
italy_ribo <- italy_ribo[order(italy_ribo$age),]

write.csv(italy_ribo, "all_intakes/italy_m_ribo.csv")

##################################################################

# 10. RUN SPADE FOR NIACIN

italy_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_niac")

italy_niac <- subset(italy_niac, select = c(age, HI))
italy_niac <- italy_niac[order(italy_niac$age),]

write.csv(italy_niac, "all_intakes/italy_w_niac.csv")

# Men
italy_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_niac")

italy_niac <- subset(italy_niac, select = c(age, HI))
italy_niac <- italy_niac[order(italy_niac$age),]

write.csv(italy_niac, "all_intakes/italy_m_niac.csv")

##################################################################

# 11. RUN SPADE FOR VITAMIN B6

italy_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_vitb6")

italy_vitb6 <- subset(italy_vitb6, select = c(age, HI))
italy_vitb6 <- italy_vitb6[order(italy_vitb6$age),]

write.csv(italy_vitb6, "all_intakes/italy_w_vitb6.csv")

# Men
italy_vitb6 <- f.spade(frml.ia=vitb6~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_vitb6")

italy_vitb6 <- subset(italy_vitb6, select = c(age, HI))
italy_vitb6 <- italy_vitb6[order(italy_vitb6$age),]

write.csv(italy_vitb6, "all_intakes/italy_m_vitb6.csv")

##################################################################

# 12. RUN SPADE FOR FOLATE

italy_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       sex.lab="women",
                       output.name = "italy_wom_fola")

italy_fola <- subset(italy_fola, select = c(age, HI))
italy_fola <- italy_fola[order(italy_fola$age),]

write.csv(italy_fola, "all_intakes/italy_w_fola.csv")

# Men
italy_fola <- f.spade(frml.ia=fola~fp(age), frml.if="no.if", 
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       sex.lab="men",
                       output.name = "italy_men_fola")

italy_fola <- subset(italy_fola, select = c(age, HI))
italy_fola <- italy_fola[order(italy_fola$age),]

write.csv(italy_fola, "all_intakes/italy_m_fola.csv")

##################################################################

# 13. RUN SPADE FOR BETA CAROTENE

italy_bcarot <- f.spade(frml.ia=bcarot~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_bcarot")

italy_bcarot <- subset(italy_bcarot, select = c(age, HI))
italy_bcarot <- italy_bcarot[order(italy_bcarot$age),]

write.csv(italy_bcarot, "all_intakes/italy_w_bcarot.csv")

# Men
italy_bcarot <- f.spade(frml.ia=bcarot~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_bcarot")

italy_bcarot <- subset(italy_bcarot, select = c(age, HI))
italy_bcarot <- italy_bcarot[order(italy_bcarot$age),]

write.csv(italy_bcarot, "all_intakes/italy_m_bcarot.csv")

##################################################################

# 14. RUN SPADE FOR FIBER

italy_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=italy_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=97,
                        sex.lab="women",
                        output.name = "italy_wom_fiber")

italy_fiber <- subset(italy_fiber, select = c(age, HI))
italy_fiber <- italy_fiber[order(italy_fiber$age),]

write.csv(italy_fiber, "all_intakes/italy_w_fiber.csv")

# Men
italy_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                        data=italy_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=0, max.age=92,
                        sex.lab="men",
                        output.name = "italy_men_fiber")

italy_fiber <- subset(italy_fiber, select = c(age, HI))
italy_fiber <- italy_fiber[order(italy_fiber$age),]

write.csv(italy_fiber, "all_intakes/italy_m_fiber.csv")

##################################################################

# 15. RUN SPADE FOR MAGNESIUM

italy_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       sex.lab="women",
                       output.name = "italy_wom_mg")

italy_mg <- subset(italy_mg, select = c(age, HI))
italy_mg <- italy_mg[order(italy_mg$age),]

write.csv(italy_mg, "all_intakes/italy_w_mg.csv")

# Men
italy_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       sex.lab="men",
                       output.name = "italy_men_mg")

italy_mg <- subset(italy_mg, select = c(age, HI))
italy_mg <- italy_mg[order(italy_mg$age),]

write.csv(italy_mg, "all_intakes/italy_m_mg.csv")

##################################################################

# 16. RUN SPADE FOR PHOSPHORUS

italy_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=italy_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=97,
                    sex.lab="women",
                    output.name = "italy_wom_phos")

italy_phos <- subset(italy_phos, select = c(age, HI))
italy_phos <- italy_phos[order(italy_phos$age),]

write.csv(italy_phos, "all_intakes/italy_w_phos.csv")

# Men
italy_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                    data=italy_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=0, max.age=92,
                    sex.lab="men",
                    output.name = "italy_men_phos")

italy_phos <- subset(italy_phos, select = c(age, HI))
italy_phos <- italy_phos[order(italy_phos$age),]

write.csv(italy_phos, "all_intakes/italy_m_phos.csv")

##################################################################

# 17. RUN SPADE FOR POTASSIUM

italy_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_pota")

italy_pota <- subset(italy_pota, select = c(age, HI))
italy_pota <- italy_pota[order(italy_pota$age),]

write.csv(italy_pota, "all_intakes/italy_w_pota.csv")

# Men
italy_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_pota")

italy_pota <- subset(italy_pota, select = c(age, HI))
italy_pota <- italy_pota[order(italy_pota$age),]

write.csv(italy_pota, "all_intakes/italy_m_pota.csv")

##################################################################

# 18. RUN SPADE FOR RETOL

italy_retol <- f.spade(frml.ia=retol~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_retol")

italy_retol <- subset(italy_retol, select = c(age, HI))
italy_retol <- italy_retol[order(italy_retol$age),]

write.csv(italy_retol, "all_intakes/italy_w_retol.csv")

# Men
italy_retol <- f.spade(frml.ia=retol~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_retol")

italy_retol <- subset(italy_retol, select = c(age, HI))
italy_retol <- italy_retol[order(italy_retol$age),]

write.csv(italy_retol, "all_intakes/italy_m_retol.csv")

##################################################################

# 19. RUN SPADE FOR VITAMIN D

italy_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=italy_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=97,
                       sex.lab="women",
                       output.name = "italy_wom_vitd")

italy_vitd <- subset(italy_vitd, select = c(age, HI))
italy_vitd <- italy_vitd[order(italy_vitd$age),]

write.csv(italy_vitd, "all_intakes/italy_w_vitd.csv")

# Men
italy_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if="no.if", 
                       data=italy_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=0, max.age=92,
                       sex.lab="men",
                       output.name = "italy_men_vitd")

italy_vitd <- subset(italy_vitd, select = c(age, HI))
italy_vitd <- italy_vitd[order(italy_vitd$age),]

write.csv(italy_vitd, "all_intakes/italy_m_vitd.csv")

##################################################################

# 20. RUN SPADE FOR VITAMIN E

italy_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                      data=italy_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=97,
                      sex.lab="women",
                      output.name = "italy_wom_vite")

italy_vite <- subset(italy_vite, select = c(age, HI))
italy_vite <- italy_vite[order(italy_vite$age),]

write.csv(italy_vite, "all_intakes/italy_w_vite.csv")

# Men
italy_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                      data=italy_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=0, max.age=92,
                      sex.lab="men",
                      output.name = "italy_men_vite")

italy_vite <- subset(italy_vite, select = c(age, HI))
italy_vite <- italy_vite[order(italy_vite$age),]

write.csv(italy_vite, "all_intakes/italy_m_vite.csv")

