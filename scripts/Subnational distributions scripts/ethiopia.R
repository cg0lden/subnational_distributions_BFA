# ethiopia (2013) National data cleaning
# File created by Simone Passarelli 3/17/21
# Clean ethiopia data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw ethiopia data 
ethiopia <- read_csv(here( "data", "raw", "ethiopia", "ETH_DBFNIWSE_2013_DietData.csv")) %>% clean_names()
names(ethiopia)

# Round the age variable down to nearest year
ethiopia$age <- floor(ethiopia$age)

# see which variables are blank
summary(ethiopia)

# To look at fish variables
# 
# ethiopia_fish <- ethiopia %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(ethiopia_fish, here( "data", "raw", "ethiopia", "ethiopia_ingredients.csv"))

# # Merge in aquatic food values
# ethiopia_omega <- read_excel(here( "data", "raw", "ethiopia", "ethiopia_ingredients_dha_epa_DV.xlsx")) %>% 
#   select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 

#Exclude food supplements 

# Look at ingredients
food_names <- ethiopia %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

ethiopia_nut <- ethiopia %>% 
  # left_join(ethiopia_omega, by="ingr_code") %>% 
  # mutate(omega_3=(ingr_amount_unproc*omega_3_100)/100) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(sfa, mufa, pufa, tfa, chol, na, k, mg, cu, se, iod, n6, plant_n3, vitb6, vitb12,
            adsugar, plantpro, animalpro, dairypro, seafood_n3, water, vitd, vite, vitk, note)) %>% 
  select(!c(recall_d:ingr_amount_proc)) %>% 
  rename(mday=recall_n) %>% 
  group_by(id, mday, age, sex) %>%
  summarize(energy=sum(energy),
            protein=sum(totalpro),
            carb=sum(carb),
            fiber=sum(fiber),
            fat=sum(totalfat),
            calc = sum(ca),
            iron = sum(fe),
            zinc = sum(zn),
            phos=sum(ph),
            vitc=sum(vitc),
            vita = sum(vita),
            thia=sum(vitb1),
            ribo=sum(vitb2),
            niac=sum(vitb3),
            betacarot=sum(bcarot),
            fola=sum(fol)) %>% distinct() 


# ethiopia_merge_2 <-  merge(ethiopia_merge, ethiopia_weights, by.all="id", all.x=T)

# Rename and format variables for spade
ethiopia_spade <- ethiopia_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
ethiopia_missings <- ethiopia_spade[is.na(ethiopia_spade$age), ] # shows you the missings
ethiopia_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(ethiopia_spade$id)
# for (idid in ids_data){
#   data.id <- ethiopia_spade[ethiopia_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     ethiopia_spade[ethiopia_spade$id == idid,"weight"] <- 
#       min(ethiopia_spade[ethiopia_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(ethiopia_spade$id)
for (idid in ids_data){
  data.id <- ethiopia_spade[ethiopia_spade$id == idid, ]
  if(nrow(data.id) > 1){
    ethiopia_spade[ethiopia_spade$id == idid,"age"] <- 
      min(ethiopia_spade[ethiopia_spade$id == idid,"age"])
  }
}

save(ethiopia_spade, file=here("data", "processed","Subnational distributions", "ethiopia"), replace)   



