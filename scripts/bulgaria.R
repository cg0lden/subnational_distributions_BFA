# Clean Bulgaria data for spade
# Created December 10th, 2020 by Simone Passarelli
library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the csv files for both children (2007) and adults (2004)

# The subject number is not unique to the dataset; must process them separately
#2007=children only
bulg_c <- read_dta(here( "data", "raw", "Bulgaria", "bulg_consumption_merged_omega.dta")) %>% 
  clean_names() %>% filter(consumption_year==2007) %>% 
  select(subject, consumption_year, survey_day, vita_rae_mcg, omega_3_100, calc_mg, iron_mg, vitb12_mcg, zinc_mg, food_amount_reported, ingredient_eng, code_ingredient) 
# 
# # 2004=adults only
# bulg_a <- read_dta(here( "data", "raw", "Bulgaria", "bulg_consumption_merged_omega.dta")) %>% 
#   clean_names() %>% filter(consumption_year==2004) %>% 
#   select(subject, consumption_year, survey_day, vita_rae_mcg, omega_3_100, calc_mg, iron_mg, vitb12_mcg, zinc_mg, food_amount_reported, ingredient_eng, code_ingredient) 

#id data children
# id data
bulg_c_id <- read_csv(here( "data", "raw", "Bulgaria", "BGR_00441", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct() %>% 
  mutate(age=floor(age_year)) %>% rename(id=subject)

# id data adults
# bulg_a_id <- read_csv(here( "data", "raw", "Bulgaria", "BGR_00440", "subject_user.csv")) %>% 
#   clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct()

# Children: Merge in identifiers, incl age/sex


# # Adults: Merge in identifiers, incl age/sex
# bulg_a_merge <- bulg_a %>%
#   left_join(bulg_a_id, by=c("subject")) %>% 
#   mutate(subject=as.character(subject)) %>% 
#   mutate(consumption_year=as.character(consumption_year)) %>% 
#   mutate(id=paste0(subject, consumption_year)) 

# THE ADULT SURVEY DOES NOT HAVE A REPEATED 24 HOUR RECALL
# EXCLUDE ADULTS FOR NOW


# Combine child and adult dataframes
bulg_nut <- bulg_c %>% 
  mutate(omega_3_g = omega_3_100 *(food_amount_reported/100)) %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate(ingredient_eng=tolower(ingredient_eng)) %>% 
  mutate(red_meat = case_when((grepl("beef", ingredient_eng)) ~ food_amount_reported,
                              (grepl("sausage", ingredient_eng)) ~ food_amount_reported,
                              (grepl("frankfurter", ingredient_eng)) ~ food_amount_reported,
                              (grepl("veal", ingredient_eng)) ~ food_amount_reported,
                              (grepl("steak", ingredient_eng)) ~ food_amount_reported,
                              (grepl("pork", ingredient_eng)) ~ food_amount_reported,
                              (grepl("lamb", ingredient_eng)) ~ food_amount_reported,
                              (grepl("mutton", ingredient_eng)) ~ food_amount_reported,
                              (grepl("goat", ingredient_eng)) ~ food_amount_reported,
                              (grepl("burger", ingredient_eng)) ~ food_amount_reported,
                              (grepl("ham", ingredient_eng)) ~ food_amount_reported,
                              (grepl("bacon", ingredient_eng)) ~ food_amount_reported,
                              (grepl("salami", ingredient_eng)) ~ food_amount_reported,
                              (grepl("wurst", ingredient_eng)) ~ food_amount_reported,
                              (grepl("wiener", ingredient_eng)) ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("chicken", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("yogurt", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("duck", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("hen", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("goose", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("geese", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("poultry", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("turkey", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("rabbit", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("stomach", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("liver", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("blood", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("brain", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("offal", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("haggis", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("milk", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("cheese", ingredient_eng)), 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient_eng=="bread of wheat wholemeal graham", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient_eng=="bouillon", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient_eng=="pork fat", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient_eng=="pate", 0)) %>% 
  relocate(red_meat , omega_3_g, after=zinc_mg) %>% 
  relocate(id, mday, ingredient_eng, food_amount_reported,  before=red_meat) %>% 
  mutate_at(vars(red_meat:vitb12_mcg), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12_mcg),
            iron = sum(iron_mg),
            vita = sum(vita_rae_mcg),
            calc = sum(calc_mg),
            zinc = sum(zinc_mg),
            red_meat = sum(red_meat),
            omega_3 = sum(omega_3_g)) %>% distinct()


# Merge in the the identifying information 
bulg_spade <- bulg_nut %>%
  left_join(bulg_c_id, by=c("id")) %>% 
  mutate(id=as.integer(id)) %>% select(!age_year)



#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
ids_data <- unique(bulg_spade$id)
for (idid in ids_data){
  data.id <- bulg_spade[bulg_spade$id == idid, ]
  if(nrow(data.id) > 1){
    bulg_spade[bulg_spade$id == idid,"age"] <- 
      min(bulg_spade[bulg_spade$id == idid,"age"])
  }
}

save(bulg_spade, file=here("data", "processed", "bulg"), replace)   



