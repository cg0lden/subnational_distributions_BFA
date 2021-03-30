# Finland (2007) data cleaning
# File created by Simone Passarelli 3/29/21
# Clean finland data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)


CANT USE FINLAND BECAUSE THEY ONLY PROVIDE MEANS ON GDD, NOT RAW DATA

# Load the raw finland data 

finland <- read_csv(here( "data", "raw", "Finland", "svyc61datac22.csv")) %>% clean_names() %>% 
  filter(dietfactor=="")
names(finland)

# Data need to be reshaped and unneeded nutrients filtered out


# No sample weights are listed


# Round the age variable down to nearest year
finland$age <- floor(finland$age)

# see which variables are blank
summary(finland)

# To look at fish variables
# 
# finland_fish <- finland %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(finland_fish, here( "data", "raw", "finland", "finland_ingredients.csv"))

# Merge in aquatic food values
# finland_omega <- read_excel(here( "data", "raw", "finland", "finland_ingredients_dha_epa_DV.xlsx")) %>% 
#   select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 


#Exclude food supplements 

# Look at ingredients
food_names <- finland %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

finland_nut <- finland %>% 
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


# finland_merge_2 <-  merge(finland_merge, finland_weights, by.all="id", all.x=T)

# Rename and format variables for spade
finland_spade <- finland_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
finland_missings <- finland_spade[is.na(finland_spade$age), ] # shows you the missings
finland_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(finland_spade$id)
# for (idid in ids_data){
#   data.id <- finland_spade[finland_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     finland_spade[finland_spade$id == idid,"weight"] <- 
#       min(finland_spade[finland_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(finland_spade$id)
for (idid in ids_data){
  data.id <- finland_spade[finland_spade$id == idid, ]
  if(nrow(data.id) > 1){
    finland_spade[finland_spade$id == idid,"age"] <- 
      min(finland_spade[finland_spade$id == idid,"age"])
  }
}

save(finland_spade, file=here("data", "processed","Subnational distributions", "finland"), replace)   



