# Clean Bangladesh data for spade
# Created December 15th, 2020 by Simone Passarelli
# Updated February 16th 2021 by SP
library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# read in raw food consumption data from csv. Keep all nutrients
bang <- read_csv(here( "data", "raw", "Bangladesh", "FAOGIFT", "consumption_user.csv")) %>% 
  clean_names() %>% 
  mutate(ingredient=tolower(ingredient))

# read in raw id data from csv
bang_id <- read_csv(here( "data", "raw", "Bangladesh", "FAOGIFT",  "subject_user.csv"))%>% 
  clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct() %>% 
  rename(id=subject , age=age_year)


# load in the omega_3 values of fish from Daniel/AFDC
bang_omega <- read_excel((here("data", "raw", "Bangladesh", "FAOGIFT", "Bangladesh_fish_types_DV.xlsx")), col_names=TRUE) %>% 
  rename( omega_3_100 = `omega_3 (EPA+DHA), grams` ) %>% mutate(ingredient=tolower(ingredient))

# merge the omega 3 values with the 
bang_nut <- bang %>% left_join(bang_omega, by="ingredient") %>%  #merge in the omega 3 values for fish
 mutate(omega_3_100=replace_na(omega_3_100, 0)) %>% #replace the NA values with 0
  mutate(omega_3_g = omega_3_100 *(food_amount_reported/100)) %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate_at(vars(energy:vita), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>% select(-c(water, 1:6, sat_fat)) %>% 
  summarize(energy = sum(energy),
            protein = sum(protein),
            carb = sum(carboh),
            fat = sum(fat), #no saturated/unsaturated fat sadly
            vitc=sum(vitc),
            thia=sum(thia),
            ribo=sum(ribo),
            vitb6=sum(vitb6),
            fola=sum(fola),
            b12 = sum(vitb12),
            iron = sum(iron),
            vita = sum(vita),
            calc = sum(calc),
            zinc = sum(zinc),
            omega_3 = sum(omega_3_g)) %>% distinct() 


# Merge in the the identifying information 
bangladesh_spade <- bang_nut %>%
  left_join(bang_id, by=c("id")) %>% 
  mutate(id=as.integer(id)) 



#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
ids_data <- unique(bangladesh_spade$id)
for (idid in ids_data){
  data.id <- bangladesh_spade[bangladesh_spade$id == idid, ]
  if(nrow(data.id) > 1){
    bangladesh_spade[bangladesh_spade$id == idid,"age"] <- 
      min(bangladesh_spade[bangladesh_spade$id == idid,"age"])
  }
}

save(bangladesh_spade, file=here("data", "processed", "Subnational distributions", "bangladesh"), replace)   



