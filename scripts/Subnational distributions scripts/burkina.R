# Clean burkina data for spade

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the burkina data from the Stata file (coded with the food groups)

burkina <- read_dta(here( "data", "raw", "Burkina", "Burkina_omega.dta"))  %>% 
  clean_names() %>% 
  select(-c( province, airesant, village, id_hh, season, date_recall, subj, 
            lact, preg, age_mother_10c, age_child, appetit, malade, weekend, code_period,
            type_rcp, code_rcp, name_rcp, code_ingr, ingrdient, 
            outlier))
    

summary(burkina)
table(burkina$nb_r24h)
names(burkina)
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
  summarize(b12 = sum(b12),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            omega_3 = sum(omega_3),
            vitc=sum(vitc),
            thia=sum(thia),
            ribo=sum(ribo),
            niac=sum(niac),
            vitb6=sum(vitb6),
            fola=sum(fola),
            bcarot=sum(bcarot),
            energy=sum(energy),
            protein=sum(protein),
            fat=sum(fat),
            carb=sum(carboh)) %>% distinct()

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
  select(id, age, sex, sample_weight) %>% distinct(id, .keep_all=TRUE)
  
# Rename and format variables for spade
burkina_spade <- burkina_nut %>% 
  left_join(burkina_merge, by=c("id")) %>%
  group_by(id, mday) %>% 
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

save(burkina_spade, file=here("data", "processed","Subnational distributions", "burkina"), replace)   



