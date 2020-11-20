
library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Mexico data from the Stata file (coded with the food groups)

mexico <- read_dta(here( "data", "raw", "Mexico", "Base ENSANUT 2016 entrega_11_11_2020_coded.dta")) %>% clean_names()


mexico_nut <-  mexico %>% 
  rename(recall = replica) %>% 
  group_by(folio, recall) %>%
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
