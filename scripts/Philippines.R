# Clean Philippines data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Phil data from the Stata file (coded with the food groups)

Phil <- read_dta(here( "data", "raw", "Philippines", "Phil.dta")) %>% clean_names()

summary(Phil)
table(Phil$survey_day)
# Need to add snail nutrients

# Red meat=9, processed meat=10

Phil_nut <-  Phil %>% 
  rename(recall = survey_day, id=subject, age=age_year) %>% 
  group_by(id, recall) %>%
  mutate(red_meat = case_when(fg24==9 ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  mutate(processed_meat = case_when(fg24==10 ~ food_amount_reported, 
                                    TRUE~0)) %>% 
  summarize(b12 = sum(b12),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            processed_meat = sum(processed_meat),
            omega_3 = sum(omega_3)) %>% distinct()

# Merge in identifiers, incl age/sex
Phil_merge <- Phil %>%
  rename( age=age_year, recall=survey_day, id=subject) %>%
  dplyr::select( age, sex, recall, id)

# Phil_merge_2 <-  merge(Phil_merge, Phil_weights, by.all="id", all.x=T)

# Rename and format variables for spade
Phil_spade <- Phil_nut %>% 
  left_join(Phil_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
Phil_missings <- Phil_spade[is.na(Phil_spade$age), ] # shows you the missings
Phil_missings   

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(Phil_spade$id)
for (idid in ids_data){
  data.id <- Phil_spade[Phil_spade$id == idid, ]
  if(nrow(data.id) > 1){
    Phil_spade[Phil_spade$id == idid,"age"] <- 
      min(Phil_spade[Phil_spade$id == idid,"age"])
  }
}

save(Phil_spade, file=here("data", "processed", "Philippines"), replace)   



