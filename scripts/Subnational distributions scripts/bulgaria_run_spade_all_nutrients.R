# Running SPADE: bulg data
# File created on 12/7/20 by Simone Passarelli
# All nutrients: b12, iron, vita,  calcium, red meat, omega 3
# There is no zinc for bulg

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "Subnational distributions", "bulg"))
SPADE.OUTPUT.PATH <- (here("output","Subnational distributions", "Bulgaria"))

###########################################################
# Remove missing obs
summary(bulg_spade)

# Make separate datasets for men and women
bulg_wom <- subset(bulg_spade, sex==2)
bulg_men <- subset(bulg_spade, sex==1)

###########################################################
# 1. RUN SPADE FOR B12

range(bulg_wom$age)
table(bulg_wom$age)
table(bulg_men$age)
range(bulg_men$age)

#ON 2/18/21: had trouble running these models with many zeroes, even with 2-part model
# Solution was to run it only for individuals over age 0 for vitamin b12

# Women
bulg_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bulg_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="women", 
                   output.name = "bulg_wom_b12")

bulg_b12 <- subset(bulg_b12, select = c(age, HI))
bulg_b12 <- bulg_b12[order(bulg_b12$age),]

write.csv(bulg_b12, "all_intakes/bulg_w_b12.csv")

# Men
bulg_b12 <- f.spade(frml.ia=b12~fp(age), frml.if="no.if", 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
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
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="women",
                    output.name = "bulg_wom_iron")

bulg_iron <- subset(bulg_iron, select = c(age, HI))
bulg_iron <- bulg_iron[order(bulg_iron$age),]

write.csv(bulg_iron, "all_intakes/bulg_w_iron.csv")

# Men
bulg_iron <- f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men",
                    output.name = "bulg_men_iron")

bulg_iron <- subset(bulg_iron, select = c(age, HI))
bulg_iron <- bulg_iron[order(bulg_iron$age),]

write.csv(bulg_iron, "all_intakes/bulg_m_iron.csv")
##################################################################
# 
# # 3. RUN SPADE FOR ZINC

#Women 
bulg_zinc_w <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bulg_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="women",
                      output.name = "bulg_wom_zinc")

bulg_zinc_w <- subset(bulg_zinc_w, select = c(age, HI))
bulg_zinc_w <- bulg_zinc_w[order(bulg_zinc_w$age),]

write.csv(bulg_zinc_w, "all_intakes/bulg_w_zinc.csv")

#  Men
bulg_zinc_m <- f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
                      data=bulg_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="men",
                      output.name = "bulg_men_zinc")

bulg_zinc_m <- subset(bulg_zinc_m, select = c(age, HI))
bulg_zinc_m <- bulg_zinc_m[order(bulg_zinc_m$age),]

write.csv(bulg_zinc_m, "all_intakes/bulg_m_zinc.csv")

##################################################################

# 4. RUN SPADE FOR VIT A

bulg_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="women",
                    output.name = "bulg_wom_vita")

bulg_vita <- subset(bulg_vita, select = c(age, HI))
bulg_vita <- bulg_vita[order(bulg_vita$age),]

write.csv(bulg_vita, "all_intakes/bulg_w_vita.csv")

# Men
bulg_vita <- f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men",
                    output.name = "bulg_men_vita")

bulg_vita <- subset(bulg_vita, select = c(age, HI))
bulg_vita <- bulg_vita[order(bulg_vita$age),]

write.csv(bulg_vita, "all_intakes/bulg_m_vita.csv")

##################################################################

# 5. RUN SPADE FOR CALCIUM

bulg_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="women",
                    output.name = "bulg_wom_calc")

bulg_calc <- subset(bulg_calc, select = c(age, HI))
bulg_calc <- bulg_calc[order(bulg_calc$age),]

write.csv(bulg_calc, "all_intakes/bulg_w_calc.csv")

# Men
bulg_calc <- f.spade(frml.ia=calc~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                    sex.lab="men",
                    
                    output.name = "bulg_men_calc")

bulg_calc <- subset(bulg_calc, select = c(age, HI))
bulg_calc <- bulg_calc[order(bulg_calc$age),]

write.csv(bulg_calc, "all_intakes/bulg_m_calc.csv")
##################################################################

# 7. RUN SPADE FOR OMEGA 3 

bulg_omega_3_w <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
                       data=bulg_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="women",
                       output.name = "bulg_wom_omega_3")

bulg_omega_3_w <- subset(bulg_omega_3_w, select = c(age, HI))
bulg_omega_3_w <- bulg_omega_3_w[order(bulg_omega_3_w$age),]

write.csv(bulg_omega_3_w, "all_intakes/bulg_w_omega_3.csv")

# Men

# Will not converge--will have to fill in with women
# summary(bulg_men$omega_3[bulg_men$age >0])
# 
# bulg_omega_3_m <- f.spade(frml.ia=omega_3~fp(age), frml.if=omega_3~cs(age),
#                        data=bulg_men, seed=123,  backtrans.nr = 3,
#                        dgts.distr = 2, min.age=2, max.age=4,
#                        sex.lab="men",
#                        output.name = "bulg_men_omega_3")
# 
# bulg_omega_3_m <- subset(bulg_omega_3_m, select = c(age, HI))
# bulg_omega_3_m <- bulg_omega_3_m[order(bulg_omega_3_m$age),]
# 
# write.csv(bulg_omega_3_m, "all_intakes/bulg_m_omega_3.csv")

##################################################################

# 8. RUN SPADE FOR VITAMIN C

bulg_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_vitc")

bulg_vitc <- subset(bulg_vitc, select = c(age, HI))
bulg_vitc <- bulg_vitc[order(bulg_vitc$age),]

write.csv(bulg_vitc, "all_intakes/bulg_w_vitc.csv")

# Men
bulg_vitc <- f.spade(frml.ia=vitc~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="men",
                     
                     output.name = "bulg_men_vitc")

bulg_vitc <- subset(bulg_vitc, select = c(age, HI))
bulg_vitc <- bulg_vitc[order(bulg_vitc$age),]

write.csv(bulg_vitc, "all_intakes/bulg_m_vitc.csv")
##################################################################
# 10. RUN SPADE FOR THIAMIN

bulg_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_thia")

bulg_thia <- subset(bulg_thia, select = c(age, HI))
bulg_thia <- bulg_thia[order(bulg_thia$age),]

write.csv(bulg_thia, "all_intakes/bulg_w_thia.csv")

# Men
bulg_thia <- f.spade(frml.ia=thia~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="men",
                     
                     output.name = "bulg_men_thia")

bulg_thia <- subset(bulg_thia, select = c(age, HI))
bulg_thia <- bulg_thia[order(bulg_thia$age),]

write.csv(bulg_thia, "all_intakes/bulg_m_thia.csv")
##################################################################
# 11. RUN SPADE FOR NIACIN

bulg_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_niac")

bulg_niac <- subset(bulg_niac, select = c(age, HI))
bulg_niac <- bulg_niac[order(bulg_niac$age),]

write.csv(bulg_niac, "all_intakes/bulg_w_niac.csv")

# Men
bulg_niac <- f.spade(frml.ia=niac~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="men",
                     
                     output.name = "bulg_men_niac")

bulg_niac <- subset(bulg_niac, select = c(age, HI))
bulg_niac <- bulg_niac[order(bulg_niac$age),]

write.csv(bulg_niac, "all_intakes/bulg_m_niac.csv")

##################################################################
# 12. RUN SPADE FOR RIBOFLAVIN

bulg_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_ribo")

bulg_ribo <- subset(bulg_ribo, select = c(age, HI))
bulg_ribo <- bulg_ribo[order(bulg_ribo$age),]

write.csv(bulg_ribo, "all_intakes/bulg_w_ribo.csv")

# Men
bulg_ribo <- f.spade(frml.ia=ribo~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     age.classes=c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79),
                     sex.lab="men",
                     
                     output.name = "bulg_men_ribo")

bulg_ribo <- subset(bulg_ribo, select = c(age, HI))
bulg_ribo <- bulg_ribo[order(bulg_ribo$age),]

write.csv(bulg_ribo, "all_intakes/bulg_m_ribo.csv")

##################################################################
# 13. RUN SPADE FOR BETACAROT

bulg_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if=betacarot~cs(age),
                          data=bulg_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="women",
                          output.name = "bulg_wom_betacarot")

bulg_betacarot <- subset(bulg_betacarot, select = c(age, HI))
bulg_betacarot <- bulg_betacarot[order(bulg_betacarot$age),]

write.csv(bulg_betacarot, "all_intakes/bulg_w_betacarot.csv")

# Men
bulg_betacarot <- f.spade(frml.ia=betacarot~fp(age), frml.if=betacarot~cs(age),
                          data=bulg_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="men",
                          
                          output.name = "bulg_men_betacarot")

bulg_betacarot <- subset(bulg_betacarot, select = c(age, HI))
bulg_betacarot <- bulg_betacarot[order(bulg_betacarot$age),]

write.csv(bulg_betacarot, "all_intakes/bulg_m_betacarot.csv")


##################################################################
# 14. RUN SPADE FOR VITAMIN D

bulg_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if=vitd~cs(age),
                          data=bulg_wom, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="women",
                          output.name = "bulg_wom_vitd")

bulg_vitd <- subset(bulg_vitd, select = c(age, HI))
bulg_vitd <- bulg_vitd[order(bulg_vitd$age),]

write.csv(bulg_vitd, "all_intakes/bulg_w_vitd.csv")

# Men
bulg_vitd <- f.spade(frml.ia=vitd~fp(age), frml.if=vitd~cs(age),
                          data=bulg_men, seed=123,  backtrans.nr = 3,
                          dgts.distr = 2, min.age=1, max.age=4,
                          sex.lab="men",
                          output.name = "bulg_men_vitd")

bulg_vitd <- subset(bulg_vitd, select = c(age, HI))
bulg_vitd <- bulg_vitd[order(bulg_vitd$age),]

write.csv(bulg_vitd, "all_intakes/bulg_m_vitd.csv")

##################################################################
# 15. RUN SPADE FOR VITAMIN E

bulg_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_vite")

bulg_vite <- subset(bulg_vite, select = c(age, HI))
bulg_vite <- bulg_vite[order(bulg_vite$age),]

write.csv(bulg_vite, "all_intakes/bulg_w_vite.csv")

# Men
bulg_vite <- f.spade(frml.ia=vite~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_vite")

bulg_vite <- subset(bulg_vite, select = c(age, HI))
bulg_vite <- bulg_vite[order(bulg_vite$age),]

write.csv(bulg_vite, "all_intakes/bulg_m_vite.csv")

#don't use alcohol or added sugar because sample is only children
names(bulg_wom)

##################################################################
# 16. RUN SPADE FOR ADDED SUGAR

bulg_adsugar <- f.spade(frml.ia=adsugar~fp(age), frml.if=adsugar~cs(age),
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_adsugar")

bulg_adsugar <- subset(bulg_adsugar, select = c(age, HI))
bulg_adsugar <- bulg_adsugar[order(bulg_adsugar$age),]

write.csv(bulg_adsugar, "all_intakes/bulg_w_adsugar.csv")

# Men
bulg_adsugar <- f.spade(frml.ia=adsugar~fp(age), frml.if=adsugar~cs(age),
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_adsugar")

bulg_adsugar <- subset(bulg_adsugar, select = c(age, HI))
bulg_adsugar <- bulg_adsugar[order(bulg_adsugar$age),]

write.csv(bulg_adsugar, "all_intakes/bulg_m_adsugar.csv")

##################################################################
# 17. RUN SPADE FOR CHOLINE

bulg_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                        data=bulg_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="women",
                        output.name = "bulg_wom_chol")

bulg_chol <- subset(bulg_chol, select = c(age, HI))
bulg_chol <- bulg_chol[order(bulg_chol$age),]

write.csv(bulg_chol, "all_intakes/bulg_w_chol.csv")

# Men
bulg_chol <- f.spade(frml.ia=chol~fp(age), frml.if="no.if", 
                        data=bulg_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men",
                        output.name = "bulg_men_chol")

bulg_chol <- subset(bulg_chol, select = c(age, HI))
bulg_chol <- bulg_chol[order(bulg_chol$age),]

write.csv(bulg_chol, "all_intakes/bulg_m_chol.csv")

##################################################################
# 18. RUN SPADE FOR MAGNESIUM

bulg_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_mg")

bulg_mg <- subset(bulg_mg, select = c(age, HI))
bulg_mg <- bulg_mg[order(bulg_mg$age),]

write.csv(bulg_mg, "all_intakes/bulg_w_mg.csv")

# Men
bulg_mg <- f.spade(frml.ia=mg~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_mg")

bulg_mg <- subset(bulg_mg, select = c(age, HI))
bulg_mg <- bulg_mg[order(bulg_mg$age),]

write.csv(bulg_mg, "all_intakes/bulg_m_mg.csv")

##################################################################
# 19. RUN SPADE FOR PHOSPHORUS

bulg_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                   data=bulg_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="women",
                   output.name = "bulg_wom_phos")

bulg_phos <- subset(bulg_phos, select = c(age, HI))
bulg_phos <- bulg_phos[order(bulg_phos$age),]

write.csv(bulg_phos, "all_intakes/bulg_w_phos.csv")

# Men
bulg_phos <- f.spade(frml.ia=phos~fp(age), frml.if="no.if", 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="men",
                   output.name = "bulg_men_phos")

bulg_phos <- subset(bulg_phos, select = c(age, HI))
bulg_phos <- bulg_phos[order(bulg_phos$age),]

write.csv(bulg_phos, "all_intakes/bulg_m_phos.csv")

##################################################################
# 20. RUN SPADE FOR POTASSIUM

bulg_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_pota")

bulg_pota <- subset(bulg_pota, select = c(age, HI))
bulg_pota <- bulg_pota[order(bulg_pota$age),]

write.csv(bulg_pota, "all_intakes/bulg_w_pota.csv")

# Men
bulg_pota <- f.spade(frml.ia=pota~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_pota")

bulg_pota <- subset(bulg_pota, select = c(age, HI))
bulg_pota <- bulg_pota[order(bulg_pota$age),]

write.csv(bulg_pota, "all_intakes/bulg_m_pota.csv")

##################################################################
# 21. RUN SPADE FOR SODIUM

bulg_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_na")

bulg_na <- subset(bulg_na, select = c(age, HI))
bulg_na <- bulg_na[order(bulg_na$age),]

write.csv(bulg_na, "all_intakes/bulg_w_na.csv")

# Men
bulg_na <- f.spade(frml.ia=na~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_na")

bulg_na <- subset(bulg_na, select = c(age, HI))
bulg_na <- bulg_na[order(bulg_na$age),]

write.csv(bulg_na, "all_intakes/bulg_m_na.csv")

##################################################################
# 22. RUN SPADE FOR COPPER

bulg_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                   data=bulg_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="women",
                   output.name = "bulg_wom_cu")

bulg_cu <- subset(bulg_cu, select = c(age, HI))
bulg_cu <- bulg_cu[order(bulg_cu$age),]

write.csv(bulg_cu, "all_intakes/bulg_w_cu.csv")

# Men
bulg_cu <- f.spade(frml.ia=cu~fp(age), frml.if="no.if", 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="men",
                   output.name = "bulg_men_cu")

bulg_cu <- subset(bulg_cu, select = c(age, HI))
bulg_cu <- bulg_cu[order(bulg_cu$age),]

write.csv(bulg_cu, "all_intakes/bulg_m_cu.csv")

##################################################################
# 23. RUN SPADE FOR IODINE

bulg_iod <- f.spade(frml.ia=iod~fp(age), frml.if=iod~cs(age),
                   data=bulg_wom, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="women",
                   output.name = "bulg_wom_iod")

bulg_iod <- subset(bulg_iod, select = c(age, HI))
bulg_iod <- bulg_iod[order(bulg_iod$age),]

write.csv(bulg_iod, "all_intakes/bulg_w_iod.csv")

# Men
bulg_iod <- f.spade(frml.ia=iod~fp(age), frml.if=iod~cs(age), 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="men",
                   output.name = "bulg_men_iod")

bulg_iod <- subset(bulg_iod, select = c(age, HI))
bulg_iod <- bulg_iod[order(bulg_iod$age),]

write.csv(bulg_iod, "all_intakes/bulg_m_iod.csv")

##################################################################
# 24. RUN SPADE FOR SELENIUM

bulg_se <- f.spade(frml.ia=se~fp(age), frml.if=se~cs(age), 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="women",
                    output.name = "bulg_wom_se")

bulg_se <- subset(bulg_se, select = c(age, HI))
bulg_se <- bulg_se[order(bulg_se$age),]

write.csv(bulg_se, "all_intakes/bulg_w_se.csv")

# Men
bulg_se <- f.spade(frml.ia=se~fp(age), frml.if=se~cs(age), 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men",
                    output.name = "bulg_men_se")

bulg_se <- subset(bulg_se, select = c(age, HI))
bulg_se <- bulg_se[order(bulg_se$age),]

write.csv(bulg_se, "all_intakes/bulg_m_se.csv")

##################################################################
# 25. RUN SPADE FOR ENERGY
# try to exclude some high numbers

summary(bulg_wom$energy)
bulg_wom_e <- bulg_wom[bulg_wom$energy > 170 & bulg_wom$age >0, ]

bulg_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                   data=bulg_wom_e, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="women",
                   output.name = "bulg_wom_energy")

bulg_energy <- subset(bulg_energy, select = c(age, HI))
bulg_energy <- bulg_energy[order(bulg_energy$age),]

write.csv(bulg_energy, "all_intakes/bulg_w_energy.csv")

# Men
bulg_energy <- f.spade(frml.ia=energy~fp(age), frml.if="no.if", 
                   data=bulg_men, seed=123,  backtrans.nr = 3,
                   dgts.distr = 2, min.age=1, max.age=4,
                   sex.lab="men",
                   output.name = "bulg_men_energy")

bulg_energy <- subset(bulg_energy, select = c(age, HI))
bulg_energy <- bulg_energy[order(bulg_energy$age),]

write.csv(bulg_energy, "all_intakes/bulg_m_energy.csv")

##################################################################
# 26. RUN SPADE FOR FIBER
# 
bulg_fiber <- f.spade(frml.ia=fiber~fp(age),frml.if="no.if", 
                       data=bulg_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=2, max.age=4,
                       sex.lab="women",
                       output.name = "bulg_wom_fiber")

bulg_fiber <- subset(bulg_fiber, select = c(age, HI))
bulg_fiber <- bulg_fiber[order(bulg_fiber$age),]

write.csv(bulg_fiber, "all_intakes/bulg_w_fiber.csv")

# Men
bulg_fiber <- f.spade(frml.ia=fiber~fp(age), frml.if="no.if", 
                       data=bulg_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       output.name = "bulg_men_fiber")

bulg_fiber <- subset(bulg_fiber, select = c(age, HI))
bulg_fiber <- bulg_fiber[order(bulg_fiber$age),]

write.csv(bulg_fiber, "all_intakes/bulg_m_fiber.csv")

##################################################################
# 27. RUN SPADE FOR CARBOHYDRATE

bulg_carb <- f.spade(frml.ia=carb~fp(age),frml.if="no.if", 
                      data=bulg_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=2, max.age=4,
                      sex.lab="women",
                      output.name = "bulg_wom_carb")

bulg_carb <- subset(bulg_carb, select = c(age, HI))
bulg_carb <- bulg_carb[order(bulg_carb$age),]

write.csv(bulg_carb, "all_intakes/bulg_w_carb.csv")

# Men
bulg_carb <- f.spade(frml.ia=carb~fp(age), frml.if="no.if", 
                      data=bulg_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="men",
                      output.name = "bulg_men_carb")

bulg_carb <- subset(bulg_carb, select = c(age, HI))
bulg_carb <- bulg_carb[order(bulg_carb$age),]

write.csv(bulg_carb, "all_intakes/bulg_m_carb.csv")

##################################################################
# 28. RUN SPADE FOR PROTEIN

bulg_protein <- f.spade(frml.ia=protein~fp(age),frml.if="no.if", 
                      data=bulg_wom, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=2, max.age=4,
                      sex.lab="women",
                      output.name = "bulg_wom_protein")

bulg_protein <- subset(bulg_protein, select = c(age, HI))
bulg_protein <- bulg_protein[order(bulg_protein$age),]

write.csv(bulg_protein, "all_intakes/bulg_w_protein.csv")

# Men
bulg_protein <- f.spade(frml.ia=protein~fp(age), frml.if="no.if", 
                      data=bulg_men, seed=123,  backtrans.nr = 3,
                      dgts.distr = 2, min.age=1, max.age=4,
                      sex.lab="men",
                      output.name = "bulg_men_protein")

bulg_protein <- subset(bulg_protein, select = c(age, HI))
bulg_protein <- bulg_protein[order(bulg_protein$age),]

write.csv(bulg_protein, "all_intakes/bulg_m_protein.csv")

##################################################################
# 29. RUN SPADE FOR FAT

bulg_fat <- f.spade(frml.ia=fat~fp(age),frml.if="no.if", 
                        data=bulg_wom, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=2, max.age=4,
                        sex.lab="women",
                        output.name = "bulg_wom_fat")

bulg_fat <- subset(bulg_fat, select = c(age, HI))
bulg_fat <- bulg_fat[order(bulg_fat$age),]

write.csv(bulg_fat, "all_intakes/bulg_w_fat.csv")

# Men
bulg_fat <- f.spade(frml.ia=fat~fp(age), frml.if="no.if", 
                        data=bulg_men, seed=123,  backtrans.nr = 3,
                        dgts.distr = 2, min.age=1, max.age=4,
                        sex.lab="men",
                        output.name = "bulg_men_fat")

bulg_fat <- subset(bulg_fat, select = c(age, HI))
bulg_fat <- bulg_fat[order(bulg_fat$age),]

write.csv(bulg_fat, "all_intakes/bulg_m_fat.csv")

##################################################################
# 30. RUN SPADE FOR SATFAT

bulg_satfat <- f.spade(frml.ia=satfat~fp(age),frml.if="no.if", 
                    data=bulg_wom, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=2, max.age=4,
                    sex.lab="women",
                    output.name = "bulg_wom_satfat")

bulg_satfat <- subset(bulg_satfat, select = c(age, HI))
bulg_satfat <- bulg_satfat[order(bulg_satfat$age),]

write.csv(bulg_satfat, "all_intakes/bulg_w_satfat.csv")

# Men
bulg_satfat <- f.spade(frml.ia=satfat~fp(age), frml.if="no.if", 
                    data=bulg_men, seed=123,  backtrans.nr = 3,
                    dgts.distr = 2, min.age=1, max.age=4,
                    sex.lab="men",
                    output.name = "bulg_men_satfat")

bulg_satfat <- subset(bulg_satfat, select = c(age, HI))
bulg_satfat <- bulg_satfat[order(bulg_satfat$age),]

write.csv(bulg_satfat, "all_intakes/bulg_m_satfat.csv")

##################################################################
# 31. RUN SPADE FOR MUFA

bulg_mufa <- f.spade(frml.ia=mufa~fp(age),frml.if="no.if", 
                       data=bulg_wom, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=2, max.age=4,
                       sex.lab="women",
                       output.name = "bulg_wom_mufa")

bulg_mufa <- subset(bulg_mufa, select = c(age, HI))
bulg_mufa <- bulg_mufa[order(bulg_mufa$age),]

write.csv(bulg_mufa, "all_intakes/bulg_w_mufa.csv")

# Men
bulg_mufa <- f.spade(frml.ia=mufa~fp(age), frml.if="no.if", 
                       data=bulg_men, seed=123,  backtrans.nr = 3,
                       dgts.distr = 2, min.age=1, max.age=4,
                       sex.lab="men",
                       output.name = "bulg_men_mufa")

bulg_mufa <- subset(bulg_mufa, select = c(age, HI))
bulg_mufa <- bulg_mufa[order(bulg_mufa$age),]

write.csv(bulg_mufa, "all_intakes/bulg_m_mufa.csv")

##################################################################
# 32. RUN SPADE FOR PUFA

bulg_pufa <- f.spade(frml.ia=pufa~fp(age),frml.if="no.if", 
                     data=bulg_wom, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=2, max.age=4,
                     sex.lab="women",
                     output.name = "bulg_wom_pufa")

bulg_pufa <- subset(bulg_pufa, select = c(age, HI))
bulg_pufa <- bulg_pufa[order(bulg_pufa$age),]

write.csv(bulg_pufa, "all_intakes/bulg_w_pufa.csv")

# Men
bulg_pufa <- f.spade(frml.ia=pufa~fp(age), frml.if="no.if", 
                     data=bulg_men, seed=123,  backtrans.nr = 3,
                     dgts.distr = 2, min.age=1, max.age=4,
                     sex.lab="men",
                     output.name = "bulg_men_pufa")

bulg_pufa <- subset(bulg_pufa, select = c(age, HI))
bulg_pufa <- bulg_pufa[order(bulg_pufa$age),]

write.csv(bulg_pufa, "all_intakes/bulg_m_pufa.csv")
