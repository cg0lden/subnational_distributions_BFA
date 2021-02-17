# Clean romania data for spade

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the rom data from the Stata file (coded with the food groups)

# Consumption data
rom <- read_csv(here( "data", "raw", "Romania", "consumption_user.csv")) %>% 
  clean_names() %>% select(subject, survey_day, vita_rae_mcg, seafood_n3_g, calc_mg, iron_mg, vitb12_mcg, zinc_mg, food_amount_reported, ingredient_eng, code_ingredient) #no zinc for rom

# id data
rom_id <- read_csv(here( "data", "raw", "Romania", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year)

summary(rom)
summary(rom_id)
table(rom$survey_day)

# Need to add snail nutrients

# Red meat=9, processed meat=10
#removed values where vita > 10000
rom_nut <-  rom %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate(ingredient_eng=tolower(ingredient_eng)) %>% 
  mutate(red_meat = case_when((startsWith(as.character(code_ingredient), "108.")) ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("chicken", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("duck", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("goose", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("poultry", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("turkey", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("rabbit", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("stomach", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("liver", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("blood", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("brain", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient_eng=="pate", 0)) %>% 
  relocate(red_meat, after=zinc_mg) %>% 
  relocate(id, mday, ingredient_eng, food_amount_reported, code_ingredient, before=red_meat) %>% 
  mutate_at(vars(red_meat:vitb12_mcg), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12_mcg),
            iron = sum(iron_mg),
            vita = sum(vita_rae_mcg),
            calc = sum(calc_mg),
            zinc = sum(zinc_mg),
            red_meat = sum(red_meat),
            omega_3 = sum(seafood_n3_g)) %>% distinct()

# Merge in identifiers, incl age/sex
rom_merge <- rom_id %>%
  rename( age=age_year, id=subject) %>%
  dplyr::select( age, sex,  id) %>% group_by(id) %>% distinct()


# rom_merge_2 <-  merge(rom_merge, rom_weights, by.all="id", all.x=T)

# Rename and format variables for spade
rom_spade <- rom_nut %>% 
  left_join(rom_merge, by=c("id")) %>%
  dplyr::select(id, age, sex, mday, b12, iron, vita, calc, zinc, red_meat,  omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
rom_missings <- rom_spade[is.na(rom_spade$age), ] # shows you the missings
rom_missings   

#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
 ids_data <- unique(rom_spade$id)
 for (idid in ids_data){
   data.id <- rom_spade[rom_spade$id == idid, ]
   if(nrow(data.id) > 1){
     rom_spade[rom_spade$id == idid,"age"] <- 
       min(rom_spade[rom_spade$id == idid,"age"])
   }
 }

save(rom_spade, file=here("data", "processed", "rom"), replace)   



