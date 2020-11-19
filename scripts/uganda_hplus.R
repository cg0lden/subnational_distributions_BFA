# Prep data for SPADE calculations
# Created by Simone Passarelli on 11/10/20
# Uganda Harvest Plus dataset

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Zambia data from the Stata file

uganda_hplus <- read_dta(here("data", "raw", "Uganda HPlus fg24.dta")) %>% clean_names()

# Limit to variables needed for analysis
uganda_h_nut <- uganda_hplus %>%
  select(!(id)) %>%
  rename(id= ind_id, recall=recall_d, weight_food = weight_f) %>%
  group_by(id, recall) %>%
  mutate(red_meat= case_when(descr=="BEEF,HIGH FAT,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,HIGHFAT,FRESH,ROASTED" ~ weight_food,
                             descr=="BEEF,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,LEAN,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,LEAN,FRESH,ROASTED" ~ weight_food,
                             descr=="GOAT,HIGH FAT,FRESH,BOILED" ~ weight_food,
                             descr=="GOAT,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="GOAT,LEAN,FRESH,BOILED" ~ weight_food,
                             descr=="PORK,HIGH FAT,FRESH,BOILED" ~ weight_food,
                             descr=="PORK,HIGH FAT,FRESH,FRIED" ~ weight_food,
                             descr=="PORK,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,LIVER,BOILED" ~ weight_food,
                             descr=="BEEF,KIDNEY,BOILED" ~ weight_food,
                             descr=="BEEF,LUNG,BOILED" ~ weight_food,
                             descr=="BEEF,TRIPE,BOILED" ~ weight_food,
                             descr=="BEEF,TROTTER (EMOLOKONY),BOILED" ~ weight_food,
                             descr=="GOAT/LAMB,LIVER,BOILED" ~ weight_food,
                             TRUE ~ 0 ) ) %>%
  summarize(b12 = sum(vit_b12),
            iron = sum(iron_mg),
            zinc = sum(zinc_mg),
            vita = sum(vit_a_mc),
            calc = sum(calcium),
            red_meat = sum(red_meat))
 
   # omega_3 = sum (omega_3)

# no processed meat listed in dataset
# FIXIT: add omega 3 content once you have fish

# Merge in identifiers, incl age/sex
uganda_h_merge<-uganda_hplus %>%
  select(!(id)) %>%
  rename(id= ind_id, recall=recall_d) %>%
  select(ageyears, gender, id, recall) %>%
  distinct() # drop duplicates (since there is one observation per food item)

# Rename and format variables for spade
uganda_h_nut_spade <- uganda_h_nut %>% left_join(uganda_h_merge, by=c("id", "recall")) %>%
  rename(age=ageyears, sex=gender) %>%
  mutate(mday = recall) %>%
  ungroup() %>%
  select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat)

uganda_h <- uganda_h_nut_spade %>% group_by(id) %>%
  mutate(id = cur_group_id())

# Look at how many recalls we have by day
table(table(uganda_h$id))
# 1   2 
# 401 180 

# 5 1-4 years
#6 5-9 years
#7 10-14 years
#8 15-19 years
#9 20-24 years
#10 25-29 years
#11 30-34 years
#12 35-39 years
#13 40-44 years
#14 45-49 years
#15 50-54 years
#16 55-59 years
#17 60-64 years
#18 65-69 years
#19 70-74 years
#20 75-79 years
#21 80 plus

summary(uganda_h)

# Look at the observations with age missing
uganda_h_missings <- uganda_h[is.na(uganda_h$age), ] # shows you the missings
uganda_h_missings 
# There are 32 missing ages: fill these in where possible with second day of obs
# Most missing age values can't be recovered because only one recall or missing both
# Define the impute mean function

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# To repair missing id's
uganda_h <- uganda_h %>% 
  group_by(id) %>%
  mutate(age= case_when(age <18 ~ NA_real_ , TRUE~age)) %>% 
  # replace 0's for age as NA so can impute mean when we have a second entry
  # this is also the case for some low numbers, eg age 2, so do the same
  mutate(age2 = impute.mean(age)) %>% #impute missing ages with the mean of age for second entry
  mutate(age = age2) %>%
  select(!age2) %>% 
  filter(!is.na(age)) %>% #dropped the two observations that with missing age
  # in the case where there are different ages for same individual, take the max age
  filter(age>=18) %>% #only keep ages >= 18 because only adults in this sample
  ungroup %>%
  mutate(age = as.integer(age))

# The next scripts sets the age the the first age (min(age)) when age is different

ids_data <- unique(uganda_h$id)
for (idid in ids_data){
  data.id <- uganda_h[uganda_h$id == idid, ]
  if(nrow(data.id) > 1){
    uganda_h[uganda_h$id == idid,"age"] <- 
      min(uganda_h[uganda_h$id == idid,"age"])
  }
}

is.integer(uganda_h$age)
is.integer(uganda_h$id)

summary(uganda_h) #no more NA values
table(table(uganda_h$sex))

# It seems like there is one observation listed as man instead of woman
uganda_h_wom <- uganda_h %>%
mutate(sex=2)


# Save separate datasets for men and women
save(uganda_h, file=here("data", "processed", "uganda_h"), replace) 




