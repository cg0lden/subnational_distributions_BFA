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

china_b12 <- read_dta(here("data", "raw", "China", "Food code_China_B12_formatted.dta")) %>% 
                          clean_names()

# Clean the China B12 data--so many zeroes

china_b12_clean <- china_b12 %>% rename(ingredient=englishname2002) %>% 
  filter(!is.na(code)) %>% 
  mutate(b12 = replace(b12, foodcategory1=="Cereals and cereals products" | 
      foodcategory1=="Dried legumes and legumes products" |
      foodcategory1=="Tubers, starches and products" |
      foodcategory1=="Vegetables and vegetables products" |
      foodcategory1=="Fungi and algae" | 
      foodcategory1=="Fruits and fruit products" |
      foodcategory1=="Fruit and fruit products" | 
        foodcategory1=="Liquor and alcoholic beverages" |
        foodcategory2=="Herb" | foodcategory1=="Condiments" | foodcategory2=="Fruit juice and drink" |
        foodcategory2=="Carbonated drink" | foodcategory2=="Tea and tea drink" | foodcategory2=="Sugars" | 
        foodcategory1=="Fats and oil" | foodcategory2=="Confectionery" | 
        foodcategory1=="Nuts and seeds" , 0)) %>% 
  mutate(b12= replace(b12, code %in% 153001:153004 , 0)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate" | ingredient=="Chocolate, filled with liquor", 0.8)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate, standard wafer" , 0.14))


china_b12_test <- china_b12_clean %>% filter(is.na(b12))



b12=replace(b12, ))
  


# Merge in identifiers

china_spade <- china %>%
  left_join(china_age, by=c("id")) %>% 
  left_join(china_sex, by=c("id")) %>% 
  rename(mday=day, red_meat=f3redmeat, vita=f3d_vit_a, calc=f3d_ca, omega_3=f3depadha, zinc=f3d_zn, iron=f3d_fe) %>% 
  group_by(id) %>% 
  mutate(id = cur_group_id()) %>%
  distinct() %>% ungroup()


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

save(china_spade, file=here("data", "processed", "china"), replace)   




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
