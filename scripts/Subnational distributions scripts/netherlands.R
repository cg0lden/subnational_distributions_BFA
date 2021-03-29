# netherlands  data cleaning
# File created by Simone Passarelli 3/29/21
# Clean netherlands data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw netherlands data 

# Adults, 2012-2016
netherlands1 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% select(!c(cd_id, e_nkj, e_nmj, prot_veg, prot_ani, org_acid, syn_fol, ufa_cis, b1_mj, b3_mj,fibre_mj,
                              foleq, fe_haem, fe_nonhaem, modisac, polysac,vitk1, water, epa, dha, rae )) %>% 
  rename(omega_3=marine, potas=kalium, carb=carbo, phos=phosphorus) %>% 
 select(!c(e_nprot:e_norg_acid)) 

# Children 2006
netherlands2 <-read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children", "SAS", "consumption_food_nut.sas7bdat")) %>% 
  clean_names() %>% select(!c(fco: nevo_name)) %>% select(!c(water, enkj, cd_id, polycho, foleq, modicho, rae, unsfa)) %>%  
  rename(mufa=eov, pufa=mov, potas=k, carb=cho, folate=folium, phos=fo) %>% mutate(b_carotene=NA_real_) 

# 2011 elderly
netherlands3 <-read_sas(here( "data", "raw", "Netherlands", "FCS2011_Elderly", "SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% 
  select(!c(e_nprot: e_norg_acid)) %>% select(!c(fatty_a, ufa_cis, epa, dha, org_acid, fibre_mj, fe_haem, fe_nonhaem))%>% 
  select(!c(cd_id, enkcal_excl, e_nmj, e_nmj_excl, prot_veg, rae, prot_ani, modisac, polysac, syn_fol, re, water, foleq)) %>% 
  rename(omega_3=marine, phos=ph) %>%  mutate(vitk=NA_real_, folate=NA_real_)

names(netherlands1)
names(netherlands2)
names(netherlands3)

# Data are stored in two separate files for 2015 and 2016: combine them
netherlands <- rbind(netherlands1, netherlands2, netherlands3) 

# Round the age variable down to nearest year
netherlands$age <- floor(netherlands$age)

# see which variables are blank
summary(netherlands1)

# To look at fish variables
# 
# netherlands_fish <- netherlands %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(netherlands_fish, here( "data", "raw", "netherlands", "netherlands_ingredients.csv"))

# Merge in aquatic food values
# netherlands_omega <- read_excel(here( "data", "raw", "netherlands", "netherlands_ingredients_dha_epa_DV.xlsx")) %>% 
#   select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 


#Exclude food supplements 

# Look at ingredients
food_names <- netherlands %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

netherlands_nut <- netherlands %>% 
  filter(ingr_code > 2384 | ingr_code<2310) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note, cu, iod, n6, plant_n3, tfa, adsugar, plantpro, animalpro, dairypro, water)) %>% 
  select(!c(recall_d:ingr_amount_proc)) %>% 
  rename(mday=recall_n) %>% 
  group_by(id, mday, age, sex) %>%
  summarize(vitb12 = sum(vitb12),
            iron = sum(fe),
            zinc = sum(zn),
            vita = sum(vita),
            calc = sum(ca),
            vite=sum(vite),
            energy=sum(energy),
            protein=sum(totalpro),
            carb=sum(carb),
            fiber=sum(fiber),
            fat=sum(totalfat),
            satfat=sum(sfa),
            mufa=sum(mufa),
            pufa=sum(pufa),
            thia=sum(vitb1),
            ribo=sum(vitb2),
            niac=sum(vitb3),
            vitb6=sum(vitb6),
            fola=sum(fol),
            vitd=sum(vitd),
            vitc=sum(vitc),
            phos=sum(ph),
            mg=sum(mg),
            na=sum(na),
            pota=sum(k),
            chol=sum(chol),
            se=sum(se),
            betacarot=sum(bcarot),
            retinol=sum(retinol),
            sucrose=sum(totalsucrose),
            alcohol=sum(alcohol),
            omega_3=sum(seafood_n3)) %>% distinct() 


# netherlands_merge_2 <-  merge(netherlands_merge, netherlands_weights, by.all="id", all.x=T)

# Rename and format variables for spade
netherlands_spade <- netherlands_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
netherlands_missings <- netherlands_spade[is.na(netherlands_spade$age), ] # shows you the missings
netherlands_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(netherlands_spade$id)
# for (idid in ids_data){
#   data.id <- netherlands_spade[netherlands_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     netherlands_spade[netherlands_spade$id == idid,"weight"] <- 
#       min(netherlands_spade[netherlands_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(netherlands_spade$id)
for (idid in ids_data){
  data.id <- netherlands_spade[netherlands_spade$id == idid, ]
  if(nrow(data.id) > 1){
    netherlands_spade[netherlands_spade$id == idid,"age"] <- 
      min(netherlands_spade[netherlands_spade$id == idid,"age"])
  }
}

save(netherlands_spade, file=here("data", "processed","Subnational distributions", "netherlands"), replace)   



