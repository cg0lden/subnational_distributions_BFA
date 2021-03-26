# sweden (2010-2011 for adults, 2003 for children) data cleaning
# File created by Simone Passarelli 3/17/21
# Clean sweden data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw sweden data 

sweden1 <- read_csv(here( "data", "raw", "sweden", "Adults","SWE_Riksmaten_2010-2011_DietData.csv")) %>% clean_names()
names(sweden1)

sweden2 <-read_csv(here( "data", "raw", "sweden", "Children", "SWE_Riksmaten_2003_DietData.csv")) %>% clean_names()
names(sweden2)

iddata1 <- read_csv(here( "data", "raw", "sweden", "Adults", "SWE_Riksmaten_2010-2011_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)

iddata2  <- read_csv(here( "data", "raw", "sweden","Children", "SWE_Riksmaten_2003_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)


# Data are stored in two separate files for 2015 and 2016: combine them
sweden <- rbind(sweden1, sweden2) 
iddata <- rbind(iddata1, iddata2)

# Round the age variable down to nearest year
sweden$age <- floor(sweden$age)

# see which variables are blank
summary(sweden)

# To look at fish variables
# 
# sweden_fish <- sweden %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(sweden_fish, here( "data", "raw", "sweden", "sweden_ingredients.csv"))

# Merge in aquatic food values
# sweden_omega <- read_excel(here( "data", "raw", "sweden", "sweden_ingredients_dha_epa_DV.xlsx")) %>% 
#   select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 


LEFT OFF HERE
#Exclude food supplements 

# Look at ingredients
food_names <- sweden %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

sweden_nut <- sweden %>% 
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


# sweden_merge_2 <-  merge(sweden_merge, sweden_weights, by.all="id", all.x=T)

# Rename and format variables for spade
sweden_spade <- sweden_nut %>% 
  left_join(iddata, by=c("id")) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
sweden_missings <- sweden_spade[is.na(sweden_spade$age), ] # shows you the missings
sweden_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(sweden_spade$id)
# for (idid in ids_data){
#   data.id <- sweden_spade[sweden_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     sweden_spade[sweden_spade$id == idid,"weight"] <- 
#       min(sweden_spade[sweden_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(sweden_spade$id)
for (idid in ids_data){
  data.id <- sweden_spade[sweden_spade$id == idid, ]
  if(nrow(data.id) > 1){
    sweden_spade[sweden_spade$id == idid,"age"] <- 
      min(sweden_spade[sweden_spade$id == idid,"age"])
  }
}

save(sweden_spade, file=here("data", "processed","Subnational distributions", "sweden"), replace)   



