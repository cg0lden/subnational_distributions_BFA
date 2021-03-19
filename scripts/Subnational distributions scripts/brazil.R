# brazil National 2014 data cleaning
# File created by Simone Passarelli 3/19/21
# Clean brazil data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw brazil data 

brazil <- read_csv(here( "data", "raw", "brazil", "POF2017_2018_Harvard.csv")) %>% clean_names()
names(brazil)

iddata <- read_csv(here( "data", "raw", "brazil", "EST_RTU_2014_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)
names(iddata)


# Round the age variable down to nearest year
brazil$age <- floor(brazil$age)

# see which variables are blank
summary(brazil)

# To look at fish variables

brazil_fish <- brazil %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()

write_csv(brazil_fish, here( "data", "nutrients to add", "brazil", "brazil_ingredients.csv"))


# rename variables and sum them 

brazil_nut <- brazil %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note,  n6, plant_n3, tfa, chol, adsugar, plantpro, animalpro, dairypro, seafood_n3, water)) %>% 
  select(!c(recall_d:ingr_amount_proc)) %>% 
  rename(mday=recall_n) %>% 
  group_by(id, mday, age, sex) %>% summarize(vitb12 = sum(vitb12),
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
                                             iod=sum(iod),
                                             se=sum(se),
                                             cu=sum(cu),
                                             betacarot=sum(bcarot),
                                             pota=sum(k)) %>% distinct() 


# brazil_merge_2 <-  merge(brazil_merge, brazil_weights, by.all="id", all.x=T)

# Rename and format variables for spade
brazil_spade <- brazil_nut %>% 
  left_join(iddata, by=c("id")) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
brazil_missings <- brazil_spade[is.na(brazil_spade$age), ] # shows you the missings
brazil_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(brazil_spade$id)
# for (idid in ids_data){
#   data.id <- brazil_spade[brazil_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     brazil_spade[brazil_spade$id == idid,"weight"] <- 
#       min(brazil_spade[brazil_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(brazil_spade$id)
for (idid in ids_data){
  data.id <- brazil_spade[brazil_spade$id == idid, ]
  if(nrow(data.id) > 1){
    brazil_spade[brazil_spade$id == idid,"age"] <- 
      min(brazil_spade[brazil_spade$id == idid,"age"])
  }
}

save(brazil_spade, file=here("data", "processed","Subnational distributions", "brazil"), replace)   



