# Portugal (2015-2016) National data cleaning
# File created by Simone Passarelli 3/17/21
# Clean portugal data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw portugal data 

portugal1 <- read_csv(here( "data", "raw", "portugal", "PRT_IAN-AF_2015-2016_DietData_part1.csv")) %>% clean_names()
names(portugal1)

portugal2 <-read_excel(here( "data", "raw", "portugal", "PRT_IAN-AF_2015-2016_DietData_part2.xlsx")) %>% clean_names()
names(portugal2)

iddata <- read_csv(here( "data", "raw", "portugal", "PRT_IAN-AF_2015-2016_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)
names(iddata)

# Data are stored in two separate files for 2015 and 2016: combine them
portugal <- rbind(portugal1, portugal2) 

# Round the age variable down to nearest year
portugal$age <- floor(portugal$age)

# see which variables are blank
summary(portugal)

# To look at fish variables

portugal_fish <- portugal %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()

write_csv(portugal_fish, here( "data", "raw", "Portugal", "Portugal_ingredients.csv"))


# rename variables and sum them 

portugal_nut <- portugal %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note, cu, se, iod, n6, plant_n3, chol, adsugar, plantpro, animalpro, dairypro, seafood_n3, water)) %>% 
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
            pota=sum(k)) %>% distinct() 


# portugal_merge_2 <-  merge(portugal_merge, portugal_weights, by.all="id", all.x=T)

# Rename and format variables for spade
portugal_spade <- portugal_nut %>% 
  left_join(iddata, by=c("id")) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
portugal_missings <- portugal_spade[is.na(portugal_spade$age), ] # shows you the missings
portugal_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(portugal_spade$id)
# for (idid in ids_data){
#   data.id <- portugal_spade[portugal_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     portugal_spade[portugal_spade$id == idid,"weight"] <- 
#       min(portugal_spade[portugal_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(portugal_spade$id)
for (idid in ids_data){
  data.id <- portugal_spade[portugal_spade$id == idid, ]
  if(nrow(data.id) > 1){
    portugal_spade[portugal_spade$id == idid,"age"] <- 
      min(portugal_spade[portugal_spade$id == idid,"age"])
  }
}

save(portugal_spade, file=here("data", "processed","Subnational distributions", "portugal"), replace)   



