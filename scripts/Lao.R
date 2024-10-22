# Clean Lao data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Lao data from the Stata file (coded with the food groups)

Lao <- read_dta(here( "data", "raw", "Lao", "Lao.dta")) %>% clean_names()

summary(Lao)
table(Lao$survey_day)
# Need to add snail nutrients

# Red meat=9, processed meat=10

Lao_nut <-  Lao %>% 
  mutate(omega_3 = replace(omega_3, ingredient=="Golden applesnail, Channeled applesnail, boiled", 0.12)) %>%
  rename(recall = survey_day, id=subject, age=age_year) %>% 
  group_by(id, recall)
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
Lao_merge <- Lao %>%
  rename( age=age_year, recall=survey_day, id=subject) %>%
  dplyr::select( age, sex, recall, id)


# Lao_merge_2 <-  merge(Lao_merge, Lao_weights, by.all="id", all.x=T)

# Rename and format variables for spade
Lao_spade <- Lao_nut %>% 
  left_join(Lao_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
Lao_missings <- Lao_spade[is.na(Lao_spade$age), ] # shows you the missings
Lao_missings   

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(Lao_spade$id)
for (idid in ids_data){
  data.id <- Lao_spade[Lao_spade$id == idid, ]
  if(nrow(data.id) > 1){
    Lao_spade[Lao_spade$id == idid,"age"] <- 
      min(Lao_spade[Lao_spade$id == idid,"age"])
  }
}

save(Lao_spade, file=here("data", "processed", "Lao"), replace)   



