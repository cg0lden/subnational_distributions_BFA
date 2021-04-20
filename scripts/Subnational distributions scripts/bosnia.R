# bosnia data cleaning
# File created by Simone Passarelli 4/5/21
# Clean bosnia data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw bosnia data 
bosnia_weights <- read_sav(here( "data", "raw", "Bosnia", "weights.sav")) %>% clean_names() %>% 
  select(finalni_ponder, subject_id) %>% rename(id=subject_id) 




bosnia <- read_sas(here( "data", "raw", "Bosnia", "bhsurvey2017simone.sas7bdat")) %>% clean_names() %>% 
  select(!c(marital:water_2)) %>% select(!c(finalni_ponder:samehouse)) %>% 
  rename( vitb12=vitb12_ug, fola=fol_ug, vite=e_vit, vita=vita_mg, betacarot=cartb_ug,
         vitd=vitd_ug, carb=cho_g, fiber=fibt_g, sugar=sugar_g,
         cholest=chorl_mg, mufa=fams_g , pufa=fapu_g , satfat=fasat_g,
         fat=fat_g, calc=ca_mg, iron=fe_mg, iodine=id_mg, pota=k_mg, mg=mg_mg,
         na=na_mg, phos=p_mg, se=se_mg, zinc=zn_mg, alcohol=alc_g, energy=enerc_kcal,
         protein=prot_g, fola=fol_ug, niac=niaeq_mg, ribo=ribf_mg,
         thia=thia_mg, vitb6=vitb6_mg, vitc=vitc_mg, vitk=vitk_ug, dha=f22_6_g, id=subject_id,
         omega_6=omega6, vite=e_vit) %>% select(!c(omega3)) %>% #figure out what is included in omega3 before using it
  mutate(epa = replace_na(epa, 0)) %>% mutate(dha = replace_na(dha, 0))  %>% mutate(omega_3=(epa+dha)) %>% 
  mutate(sex=case_when(gender=="F" ~ 2,
                       gender=="M" ~ 1)) %>% select(!c(gender, epa, dha)) %>% 
  # Join in the corrected sample weights
  left_join(bosnia_weights) %>% rename(weights=finalni_ponder) %>% filter(weights>0) %>% 
  # Rename and format variables for spade
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  mutate(id=as.integer(id)) %>% 
  # Make an mday id
  mutate(mday= row_number()) %>% ungroup()


names(bosnia)
summary(bosnia)


# rename variables and sum them 

bosnia_spade <- bosnia %>% 
  mutate_all(~replace(., is.na(.), 0))  %>% mutate(id=as.integer(id)) %>% distinct() 

names(bosnia_spade)
summary(bosnia_spade)
save(bosnia_spade, file=here("data", "processed","Subnational distributions", "bosnia"), replace)   



