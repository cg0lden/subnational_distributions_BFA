library(tidyverse)
library(haven)
library(here)
library(janitor)

# Open Data from Yanping with nutrients calculated
china <- read_sas(here("data", "raw", "China", "simone_3days.sas7bdat")) %>% 
  clean_names() %>% rename(id=i_dind)

china_sex <- read_sas(here("data", "raw", "China", "mast_pub_12.sas7bdat")) %>% 
  clean_names() %>% rename(id=idind, sex=gender) %>% select(id, sex) 

china_age <- read_sas(here("data", "raw", "China", "surveys_pub_12.sas7bdat")) %>% 
  clean_names() %>% rename(id=idind) %>% filter(wave==2009) %>% select(age, id)

# Merge in identifiers

china_spade <- china %>%
  left_join(china_age, by=c("id")) %>% 
  left_join(china_sex, by=c("id")) %>% 
  rename(mday=day, red_meat=f3redmeat, vita=f3d_vit_a, calc=f3d_ca, omega_3=f3depadha, zinc=f3d_zn, iron=f3d_fe) %>% 
  distinct()


summary(china_spade)

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(china_spade$id)
for (idid in ids_data){
  data.id <- china_spade[china_spade$id == idid, ]
  if(nrow(data.id) > 1){
    china_spade[china_spade$id == idid,"age"] <- 
      min(china_spade[china_spade$id == idid,"age"])
  }
}

save(china_spade, file=here("data", "processed", "China"), replace)   




# to clean through the nutrition data

# The first survey is the household inventory: we only want individual level
# china_nut1 <- read_sas(here("data", "raw", "China", "nutr1_00.sas7bdat")) %>% 
  # clean_names() %>% filter(wave==2009)

china_nut2 <- read_sas(here("data", "raw", "China", "nutr2_00.sas7bdat")) %>% 
  clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)

# food codes are in this file and file 2
china_nut3 <- read_sas(here("data", "raw", "China", "nutr3_00.sas7bdat")) %>% 
  clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)
# Open identifying data
# china_id <- read_sas("/Users/Simone/Downloads/Master_ID_201908/rst_12.sas7bdat") %>% 
  # clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)
