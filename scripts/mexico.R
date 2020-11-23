# Clean Mexico data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Mexico data from the Stata file (coded with the food groups)

mexico <- read_dta(here( "data", "raw", "Mexico", "Base ENSANUT 2016 entrega_11_11_2020_coded.dta")) %>% clean_names()


mexico_nut <-  mexico %>% 
  rename(recall = replica, id=folio, age=edad) %>% 
  group_by(id, recall) %>%
               mutate(red_meat = group18) %>% 
               mutate(processed_meat = group19) %>% 
         summarize(b12 = sum(vit_b12_con_1, vit_b12_add_con_1),
                          iron = sum(iron_con_1),
                          zinc = sum(zinc_con_1),
                          vita = sum(vit_a_rae_con_1),
                          calc = sum(calcium_con_1),
                          red_meat = sum(red_meat),
                          processed_meat = sum(processed_meat),
                          red_processed_meat = sum(red_meat, processed_meat),
                   omega_3 = sum(f22d6_con_1, f20d5_con_1))


# Merge in identifiers, incl age/sex
mexico_merge <- mexico %>%
  rename( age=edad, sex=sexo, recall=replica, id=folio) %>%
  dplyr::select( age, sex, recall, id) %>%
  distinct() 


# Rename and format variables for spade
mexico_spade <- mexico_nut %>% 
  left_join(mexico_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  mutate(id = cur_group_id())
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, red_processed_meat)

  
  # Check for missing or differen ages
  mexico_missings <- mexico_spade[is.na(mexico_spade$age), ] # shows you the missings
  mexico_missings   

#No missing ages
  
#Replace any cases where the ages are different for the same individual
  
  ids_data <- unique(mexico_spade$id)
  for (idid in ids_data){
    data.id <- mexico_spade[mexico_spade$id == idid, ]
    if(nrow(data.id) > 1){
      mexico_spade[mexico_spade$id == idid,"age"] <- 
        min(mexico_spade[mexico_spade$id == idid,"age"])
    }
  }

save(mexico_spade, file=here("data", "processed", "mexico"), replace)   



