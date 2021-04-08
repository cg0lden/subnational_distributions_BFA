# canada National 2015 data cleaning
# File created by Simone Passarelli 3/19/21
# Clean canada data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw canada data: there are three files

canada1 <- read_excel(here( "data", "raw", "canada", "CAN_CCHS_2015_DietData-part1.xlsx")) %>% clean_names()
names(canada1)

canada2 <-read_csv(here( "data", "raw", "canada", "CAN_CCHS_2015_DietData-part2.csv")) %>% clean_names()
names(canada2)

canada3 <-read_excel(here( "data", "raw", "canada", "CAN_CCHS_2015_DietData-part3.xlsx")) %>% clean_names()
names(canada3)

iddata <- read_csv(here( "data", "raw", "canada", "CAN_CCHS_2015_ParticipantData.csv")) %>% clean_names() %>% 
  select(id, smpl_weight) %>% rename(weight=smpl_weight)
names(iddata)

# Data are stored in two separate files for 2015 and 2016: combine them
canada <- rbind(canada1, canada2, canada3) 

# Round the age variable down to nearest year
canada$age <- floor(canada$age)

# see which variables are blank
summary(canada)


# rename variables and sum them 

canada_nut <- canada %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note,  n6, tfa, chol, adsugar, plantpro, animalpro, dairypro, water, vite, vitk, bcarot, iod, se)) %>% 
  select(!c(recall_d:ingr_amount_proc)) %>% 
  rename(mday=recall_n) %>% 
  group_by(id, mday, age, sex) %>% summarize(vitb12 = sum(vitb12),
                                             iron = sum(fe),
                                             zinc = sum(zn),
                                             vita = sum(vita),
                                             calc = sum(ca),
                                             energy=sum(energy),
                                             protein=sum(totalpro),
                                             carb=sum(carb),
                                             fiber=sum(fiber),
                                             fat=sum(totalfat),
                                             omega_3=sum(seafood_n3),
                                             sugar=sum(totalsugars),
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
                                             pota=sum(k)) %>% distinct() 


# canada_merge_2 <-  merge(canada_merge, canada_weights, by.all="id", all.x=T)

# Rename and format variables for spade
canada_spade <- canada_nut %>% 
  left_join(iddata, by=c("id")) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
canada_missings <- canada_spade[is.na(canada_spade$age), ] # shows you the missings
canada_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(canada_spade$id)
# for (idid in ids_data){
#   data.id <- canada_spade[canada_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     canada_spade[canada_spade$id == idid,"weight"] <- 
#       min(canada_spade[canada_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(canada_spade$id)
for (idid in ids_data){
  data.id <- canada_spade[canada_spade$id == idid, ]
  if(nrow(data.id) > 1){
    canada_spade[canada_spade$id == idid,"age"] <- 
      min(canada_spade[canada_spade$id == idid,"age"])
  }
}

save(canada_spade, file=here("data", "processed","Subnational distributions", "canada"), replace)   



