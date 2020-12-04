# Clean burkina data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the burkina data from the Stata file (coded with the food groups)

burkina <- read_dta(here( "data", "raw", "Burkina", "Burkina_omega.dta")) %>% clean_names()

summary(burkina)
table(burkina$survey_day)
# Need to add snail nutrients

# Red meat=9, processed meat=10

burkina_nut <-  burkina %>% 

  rename(recall = survey_day, id=subject, age=age_year) %>% 
  group_by(id, recall) %>%
  mutate(red_meat = case_when(ingredient=="Buffalo meat" ~ wgt_food,
                              fg24==9 ~ wgt_food,
                              ingredient=="Duck meat" ~ 0,
                              ingredient=="Golden applesnail, Channeled applesnail, boiled" ~ 0,
                              TRUE ~ 0)) %>% 
  mutate(processed_meat = case_when(fg24==10 ~ wgt_food, TRUE~0)) %>% 
  summarize(b12 = sum(b12),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            processed_meat = sum(processed_meat),
            omega_3 = sum(omega_3)) %>% distinct()

# Merge in identifiers, incl age/sex
burkina_merge <- burkina %>%
  rename( age=age_year, recall=survey_day, id=subject) %>%
  dplyr::select( age, sex, recall, id)


# burkina_merge_2 <-  merge(burkina_merge, burkina_weights, by.all="id", all.x=T)

# Rename and format variables for spade
burkina_spade <- burkina_nut %>% 
  left_join(burkina_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, omega_3) %>% 
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
burkina_missings <- burkina_spade[is.na(burkina_spade$age), ] # shows you the missings
burkina_missings   

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(burkina_spade$id)
for (idid in ids_data){
  data.id <- burkina_spade[burkina_spade$id == idid, ]
  if(nrow(data.id) > 1){
    burkina_spade[burkina_spade$id == idid,"age"] <- 
      min(burkina_spade[burkina_spade$id == idid,"age"])
  }
}

save(burkina_spade, file=here("data", "processed", "burkina"), replace)   



