# Clean italy data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the italy data from the Stata file (coded with the food groups)

# Consumption data
italy <- read_dta(here( "data", "raw", "italy", "italy_omega.dta")) %>% 
  clean_names() %>% select(subject, survey_day, vita, omega_3, calc, iron, food_amount_reported, ingredient, code_ingredient, vitb12) #no zinc for italy

# id data
italy_id <- read_csv(here( "data", "raw", "italy", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year)

summary(italy)
table(italy$survey_day)

# Need to add snail nutrients

# Red meat=9, processed meat=10
#removed values where vita > 10000
italy_nut <-  italy %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate(red_meat = case_when(code_ingredient==50001 ~ food_amount_reported,
                              code_ingredient==50002 ~ food_amount_reported,
                              code_ingredient==50003 ~ food_amount_reported,
                              code_ingredient==50004 ~ food_amount_reported,
                              code_ingredient >= 50012 & code_ingredient <=50063 ~ food_amount_reported,
                              code_ingredient >= 50088 & code_ingredient <=50112 ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12),
            iron = sum(iron),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            omega_3 = sum(omega_3)) %>% distinct()

# Merge in identifiers, incl age/sex
italy_merge <- italy_id %>%
  rename( age=age_year, id=subject) %>%
  dplyr::select( age, sex,  id) %>% group_by(id) %>% distinct()


# italy_merge_2 <-  merge(italy_merge, italy_weights, by.all="id", all.x=T)

# Rename and format variables for spade
italy_spade <- italy_nut %>% 
  left_join(italy_merge, by=c("id")) %>%
  dplyr::select(id, age, sex, mday, b12, iron, vita, calc, red_meat,  omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
italy_missings <- italy_spade[is.na(italy_spade$age), ] # shows you the missings
italy_missings   

#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
# ids_data <- unique(italy_spade$id)
# for (idid in ids_data){
#   data.id <- italy_spade[italy_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     italy_spade[italy_spade$id == idid,"age"] <- 
#       min(italy_spade[italy_spade$id == idid,"age"])
#   }
# }

save(italy_spade, file=here("data", "processed", "italy"), replace)   



