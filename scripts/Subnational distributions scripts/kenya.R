# kenya (2011) National data cleaning
# File created by Simone Passarelli 3/25/21
# Clean kenya data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw kenya data 

kenya <- read_csv(here( "data", "raw", "kenya", "KEN_KNMS_2011_DietData.csv")) %>% clean_names()
names(kenya1)


iddata <- read_csv(here( "data", "raw", "kenya", "KEN_KNMS_2011_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)
names(iddata)


# Round the age variable down to nearest year
kenya$age <- floor(kenya$age)

# see which variables are blank
summary(kenya)

# To look at fish variables
# 
# kenya_fish <- kenya %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(kenya_fish, here( "data", "raw", "kenya", "kenya_ingredients.csv"))

# Merge in aquatic food values
kenya_omega <- read_excel(here( "data", "raw", "kenya", "kenya_ingredients_dha_epa_DV.xlsx")) %>% 
  select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 

#Exclude food supplements 

# Look at ingredients
food_names <- kenya %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

kenya_nut <- kenya %>% 
  filter(!(ingr_code > 200200103 & ingr_code <	200209501)) %>% 
  left_join(kenya_omega, by="ingr_code") %>% 
  mutate(omega_3=(ingr_amount_unproc*omega_3_100)/100) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note, cu, se, iod, n6, plant_n3, chol, adsugar, plantpro, animalpro, dairypro, seafood_n3, water, omega_3_100)) %>% 
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
            tfat=sum(tfa),
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
            omega_3=sum(omega_3)) %>% distinct() 


# kenya_merge_2 <-  merge(kenya_merge, kenya_weights, by.all="id", all.x=T)

# Rename and format variables for spade
kenya_spade <- kenya_nut %>% 
  left_join(iddata, by=c("id")) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
kenya_missings <- kenya_spade[is.na(kenya_spade$age), ] # shows you the missings
kenya_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(kenya_spade$id)
# for (idid in ids_data){
#   data.id <- kenya_spade[kenya_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     kenya_spade[kenya_spade$id == idid,"weight"] <- 
#       min(kenya_spade[kenya_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(kenya_spade$id)
for (idid in ids_data){
  data.id <- kenya_spade[kenya_spade$id == idid, ]
  if(nrow(data.id) > 1){
    kenya_spade[kenya_spade$id == idid,"age"] <- 
      min(kenya_spade[kenya_spade$id == idid,"age"])
  }
}

save(kenya_spade, file=here("data", "processed","Subnational distributions", "kenya"), replace)   



