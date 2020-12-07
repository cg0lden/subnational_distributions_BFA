# Clean burkina data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the burkina data from the Stata file (coded with the food groups)

burkina <- read_dta(here( "data", "raw", "Burkina", "Burkina_omega.dta"))  %>% 
  clean_names() %>% 
  select(id_subj, sample_weight, age_mother, age_child_4c, nb_r24h, sex, 
         wgt_food, calc, iron, zinc, vita, vitb12, omega_3, code_grp, id_mother, id_child)

summary(burkina)
table(burkina$nb_r24h)

# Make an indicator variable for mother vs child


# Need to add snail nutrients

# Red meat=9, processed meat=10

burkina_nut <-  burkina %>% 
  mutate(mother = case_when(id_mother != "" ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(id = case_when(mother == 1  ~ id_mother,
                        mother == 0  ~ id_child,
                        TRUE ~ NA_character_)) %>% 
  mutate(id = as.integer(id)) %>% 
  rename(mday = nb_r24h, b12=vitb12) %>% 
  group_by(id, mday) %>%
  mutate(red_meat = case_when(
    code_grp==13 ~ wgt_food, TRUE ~ 0)) %>% 
  summarize(b12 = sum(b12),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            omega_3 = sum(omega_3)) %>% distinct()

# Identifying info 
burkina_merge <-  burkina %>%  
  mutate(mother = case_when(id_mother != "" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(id = case_when(mother == 1  ~ id_mother,
                        mother == 0  ~ id_child,
                        TRUE ~ NA_character_)) %>% 
  mutate(age = case_when(mother == 1 ~ age_mother, 
                         mother == 0 ~ age_child_4c,
                         TRUE ~ NA_real_)) %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(id = as.integer(id)) %>% 
  select(id, age, sex) %>% distinct(id, .keep_all=TRUE)
  
# Rename and format variables for spade
burkina_spade <- burkina_nut %>% 
  left_join(burkina_merge, by=c("id")) %>%
  group_by(id, mday) %>% 
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat,  omega_3) %>% 
  distinct()

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

# Replace any cases where the sex is different for the same individual


ids_data <- unique(burkina_spade$id)
for (idid in ids_data){
  data.id <- burkina_spade[burkina_spade$id == idid, ]
  if(nrow(data.id) > 1){
    burkina_spade[burkina_spade$id == idid,"sex"] <- 
      min(burkina_spade[burkina_spade$id == idid,"sex"])
  }
}

save(burkina_spade, file=here("data", "processed", "burkina"), replace)   



