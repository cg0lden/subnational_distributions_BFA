# Estonia National 2014 data cleaning
# File created by Simone Passarelli 3/18/21
# Updated to remove supplements on 3/24/21
# Clean Estonia data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw estonia data 

estonia <- read_excel(here( "data", "raw", "estonia", "EST_RTU_2014_DietData.xlsx")) %>% clean_names()
names(estonia)


# Round the age variable down to nearest year
estonia$age <- floor(estonia$age)

# see which variables are blank
summary(estonia)

# To look at fish variables

estonia_fish <- estonia %>% select(ingr_descr_eng,  ingr_code) %>% filter(!(str_detect(ingr_code, "^S"))) %>% distinct()

write_csv(estonia_fish, here( "data", "raw", "Estonia", "estonia_ingredients.csv"))

# Filter out supplements



# rename variables and sum them 

estonia_nut <- estonia %>% filter(!(str_detect(ingr_code, "^S"))) %>% 
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


# estonia_merge_2 <-  merge(estonia_merge, estonia_weights, by.all="id", all.x=T)

# Rename and format variables for spade
estonia_spade <- estonia_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
estonia_missings <- estonia_spade[is.na(estonia_spade$age), ] # shows you the missings
estonia_missings   

#Replace any cases where the ages are different for the same individual

ids_data <- unique(estonia_spade$id)
for (idid in ids_data){
  data.id <- estonia_spade[estonia_spade$id == idid, ]
  if(nrow(data.id) > 1){
    estonia_spade[estonia_spade$id == idid,"age"] <- 
      min(estonia_spade[estonia_spade$id == idid,"age"])
  }
}

save(estonia_spade, file=here("data", "processed","Subnational distributions", "estonia"), replace)   



