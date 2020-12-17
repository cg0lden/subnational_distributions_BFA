# Clean Bangladesh data for spade
# Created December 15th, 2020 by Simone Passarelli
library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)


bang <- read_csv(here( "data", "raw", "Bangladesh", "consumption_user.csv")) %>% 
  clean_names() %>% 
  select(subject, survey_day, vita, calc, iron, vitb12, zinc,
         food_amount_reported, ingredient, code_ingredient, foodex2_ingr_descr) %>% 
  mutate(ingredient=tolower(ingredient))

# id data
bang_id <- read_csv(here( "data", "raw", "Bangladesh",  "subject_user.csv"))%>% 
  clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct() %>% 
  rename(id=subject , age=age_year)


# load in the omega_3 values of fish from Daniel/AFDC
bang_omega <- read_excel((here("data", "raw", "Bangladesh", "Bangladesh_fish_types_DV.xlsx")), col_names=TRUE) %>% 
  rename( omega_3_100 = `omega_3 (EPA+DHA), grams` ) %>% mutate(ingredient=tolower(ingredient))

# merge the omega 3 values with the 
bang_nut <- bang %>% left_join(bang_omega, by="ingredient") %>%  #merge in the omega 3 values for fish
 mutate(omega_3_100=replace_na(omega_3_100, 0)) %>% #replace the NA values with 0
  mutate(omega_3_g = omega_3_100 *(food_amount_reported/100)) %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate(red_meat = case_when((grepl("beef", ingredient)) ~ food_amount_reported,
                              (grepl("sausage", ingredient)) ~ food_amount_reported,
                              (grepl("frankfurter", ingredient)) ~ food_amount_reported,
                              (grepl("veal", ingredient)) ~ food_amount_reported,
                              (grepl("steak", ingredient)) ~ food_amount_reported,
                              (grepl("pork", ingredient)) ~ food_amount_reported,
                              (grepl("lamb", ingredient)) ~ food_amount_reported,
                              (grepl("mutton", ingredient)) ~ food_amount_reported,
                              (grepl("goat", ingredient)) ~ food_amount_reported,
                              (grepl("burger", ingredient)) ~ food_amount_reported,
                              (grepl("ham", ingredient)) ~ food_amount_reported,
                              (grepl("bacon", ingredient)) ~ food_amount_reported,
                              (grepl("salami", ingredient)) ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("chicken", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("intestines", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("duck", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("pigeon", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("stomach", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("liver", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("brain", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("offal", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="bouillon", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="beef fat  (cooked nuts)", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="pate", 0)) %>% 
  relocate(red_meat , omega_3_g, after=zinc) %>% 
  relocate(id, mday, ingredient, food_amount_reported,  before=red_meat) %>% 
  mutate_at(vars(red_meat:vitb12), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12),
            iron = sum(iron),
            vita = sum(vita),
            calc = sum(calc),
            zinc = sum(zinc),
            red_meat = sum(red_meat),
            omega_3 = sum(omega_3_g)) %>% distinct()


# Merge in the the identifying information 
bang_spade <- bang_nut %>%
  left_join(bang_id, by=c("id")) %>% 
  mutate(id=as.integer(id)) 



#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
ids_data <- unique(bang_spade$id)
for (idid in ids_data){
  data.id <- bang_spade[bang_spade$id == idid, ]
  if(nrow(data.id) > 1){
    bang_spade[bang_spade$id == idid,"age"] <- 
      min(bang_spade[bang_spade$id == idid,"age"])
  }
}

save(bang_spade, file=here("data", "processed", "bangladesh"), replace)   



