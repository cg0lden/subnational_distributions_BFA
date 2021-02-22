# Clean italy data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the italy data from the Stata file (coded with the food groups)

# Consumption data
italy <- read_dta(here("data", "raw", "Italy", "italy_omega.dta")) %>% 
  clean_names() %>% select(-c(adm0_code, adm0_name, adm1_code, adm1_name, adm2_code,
                              adm2_name, round, consumption_day, consumption_month,
                              consumption_year, week_day, exception_day:food_amount_proc, water,
                              a_prot, v_prot, zinc, na, cu, adsugar:vitk, epa, dha, omega_3_100, merge, b12_100))
  
# id data
italy_id <- read_csv(here( "data", "raw", "italy", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year)

summary(italy)
table(italy$survey_day)

# Need to add snail nutrients

#removed values where vita > 10000
italy_nut <-  italy %>% 
  rename(mday = survey_day, id=subject) %>% 
  group_by(id, mday) %>%
  replace(is.na(.), 0) %>% 
  summarize_at(vars(energy:omega_3), sum) %>% distinct()


# Merge in identifiers, incl age/sex
italy_merge <- italy_id %>%
  rename( age=age_year, id=subject) %>%
  dplyr::select( age, sex,  id) %>% group_by(id) %>% distinct()

# Rename and format variables for spade
italy_spade <- italy_nut %>% 
  left_join(italy_merge, by=c("id")) %>%
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

save(italy_spade, file=here("data", "processed", "Subnational distributions", "italy"), replace)   



