# Clean italy data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the italy data from the Stata file (coded with the food groups)

# Consumption data
italy <- read_csv(here( "data", "raw", "italy", "consumption_user.csv")) %>% 
  clean_names() %>% select(subject, survey_day, vita, zinc, seafood_n3, calc, iron, food_amount_reported, ingredient)

# id data
italy_id <- read_csv(here( "data", "raw", "italy", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year)

summary(italy)
table(italy$survey_day)

# Need to add snail nutrients

# Red meat=9, processed meat=10

italy_nut <-  italy %>% 
  rename(mday = survey_day, id=subject, age=age_year) %>% 
  group_by(id, recall) %>%
  mutate(red_meat = case_when(ingredient=="Buffalo meat" ~ food_amount_reported,
                              fg24==9 ~ food_amount_reported,
                              ingredient=="Duck meat" ~ 0,
                              ingredient=="Golden applesnail, Channeled applesnail, boiled" ~ 0,
                              TRUE ~ 0)) %>% 
  mutate(processed_meat = case_when(fg24==10 ~ food_amount_reported, TRUE~0)) %>% 
  summarize(b12 = sum(b12),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            processed_meat = sum(processed_meat),
            omega_3 = sum(omega_3)) %>% distinct()

# Merge in identifiers, incl age/sex
italy_merge <- italy %>%
  rename( age=age_year, recall=survey_day, id=subject) %>%
  dplyr::select( age, sex, recall, id)


# italy_merge_2 <-  merge(italy_merge, italy_weights, by.all="id", all.x=T)

# Rename and format variables for spade
italy_spade <- italy_nut %>% 
  left_join(italy_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
italy_missings <- italy_spade[is.na(italy_spade$age), ] # shows you the missings
italy_missings   

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(italy_spade$id)
for (idid in ids_data){
  data.id <- italy_spade[italy_spade$id == idid, ]
  if(nrow(data.id) > 1){
    italy_spade[italy_spade$id == idid,"age"] <- 
      min(italy_spade[italy_spade$id == idid,"age"])
  }
}

save(italy_spade, file=here("data", "processed", "italy"), replace)   



