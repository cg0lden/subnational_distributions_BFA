# Zambia dataset cleaning and spade operations

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Zambia data from the Stata file

zambia <- read_dta(here( "data", "raw", "Zambia", "Zambia fg24_omega.dta")) %>% clean_names()

# Calculate the quantity of red meat consumed
# According to GBD, red meat is "Any intake (in grams per day) of red meat including beef, pork, lamb, and
# goat but excluding poultry, fish, eggs, and all processed meats"


# Processed meat is classified as the following in the GBD: 
# Any intake (in grams per day) of meat preserved by smoking, curing,
# salting, or addition of chemical preservatives

summary(zambia)
# Limit to variables needed for analysis
zambia_nut <- zambia %>%
  group_by(hid, pid, recall) %>%
  mutate(red_meat= case_when(descr=="BEEF,HIGH FAT,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,KIDNEY,BOILED" ~ weight_food,
                             descr=="BEEF,LEAN,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,LEAN,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,LIVER,BOILED" ~ weight_food,
                             descr=="BEEF,LUNG,BOILED" ~ weight_food,
                             descr=="BEEF,LUNG,FRIED" ~ weight_food,
                             descr=="BEEF,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="BEEF,TRIPE,BOILED" ~ weight_food,
                             descr=="BEEF,TRIPE,BOILED" ~ weight_food,
                             descr=="GAME MEAT,BUFFALO,DRIED,BOILED" ~ weight_food,
                             descr=="GAME MEAT,BUFFALO,DRIED,BOILED" ~ weight_food,
                             descr=="GOAT,LEAN,FRESH,BOILED" ~ weight_food,
                             descr=="GOAT,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="GOAT/LAMB,LIVER,BOILED" ~ weight_food,
                             descr=="GOAT/LAMB,LUNG,FRESH,BOILED" ~ weight_food,
                             descr=="GOAT/LAMB,OFFALS,FRESH,MIXED,BOILED" ~ weight_food,
                             descr=="PORK, LEAN, FRESH, COOKED" ~ weight_food,
                             descr=="PORK,HIGH FAT,FRESH,FRIED" ~ weight_food,
                             descr=="PORK,LEAN,W/BONE,FRESH,BOILED" ~ weight_food,
                             descr=="PORK,LIVER,FRESH, RAW,BOILED" ~ weight_food,
                             descr=="PORK,MEDIUM FAT,FRESH,BOILED" ~ weight_food,
                             descr=="RAT,DRIED,BOILED" ~ weight_food,
                             descr=="RAT,FRESH,ROASTED" ~ weight_food,
                             TRUE ~ 0)) %>%
  mutate(processed_meat = case_when(gdescr=="MEATS, POULTRY, AND INSECTS" & str_detect(descr, "SAUSAGE") ~ weight_food,
                                    gdescr=="MEATS, POULTRY, AND INSECTS" & str_detect(descr, "SMOKED") ~ weight_food,
                                    TRUE ~ 0)) %>%
  summarize(b12 = sum(vit_b12_mcg),
            iron = sum(iron_mg),
            zinc = sum(zinc_mg),
            vita = sum(vit_a_mcg_rae),
            calc = sum(calcium_mg),
            red_meat = sum(red_meat),
            processed_meat = sum(processed_meat),
            red_processed_meat = sum(red_meat, processed_meat),
            omega_3 = sum(omega_3))



# Merge in identifiers, incl age/sex
zambia_merge <- zambia %>%
  dplyr::select( agey, sex, recall, hid, pid) %>%
    distinct() 
# drop duplicates (since there is one observation per food item)

# drop duplicates (since there is one observation per food item)
  
# Rename and format variables for spade
zambia_spade <- zambia_nut %>% left_join(zambia_merge, by=c("hid", "pid", "recall")) %>%
  rename(age=agey) %>%
  mutate(hid=as.character(hid),
         pid=as.character(pid)) %>%
  mutate(id=paste0(hid, pid)) %>%
  mutate(mday = recall) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita, calc, red_meat, processed_meat, red_processed_meat, omega_3)

zambia_spade$id <- as.integer(zambia_spade$id)

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

# No need to make separate datasets for men and women because only women in this dataset
# Make a count variable for id

zambia_wom <- zambia_spade %>% group_by(id) %>%
  mutate(id = cur_group_id())

# Some people are missing age
# The first error I get is
#Error in if (sum(round(dt[, "age"], 0) != dt[, "age"]) > 0) stop("f_check_data: variable age contains non unit numbers") : 
#		missing value where TRUE/FALSE needed

# And if you view zambia_wom you will see some missings in age

# Let us see how we can repair this
zambia_missings <- zambia_wom[is.na(zambia_wom$age), ] # shows you the missings
zambia_missings 


#           id age sex mday   b12
#35  103430263  NA   2    1  0.00
#155 105120253  NA   2    1 12.04
#231 106311073  NA   2    1  0.00
#255 107210733  NA   2    1  0.00
#270 107430143  NA   2    1  0.00
#406 202610183  NA   2    1  0.00
#454 203320513  NA   2    1  0.00
#571 206340523  NA   2    1  0.25
#662 208630333  NA   2    1  0.00

# Let us see if we have other records with age present for the same id
for (idid in zambia_missings[, "id"]){
  print(zambia_wom[zambia_wom[, "id"] == idid, ])
}

#          id age sex mday b12
#35 103430263  NA   2    1   0
#36 103430263  27   2    2   0
#           id age sex mday   b12
#155 105120253  NA   2    1 12.04
#           id age sex mday  b12
#231 106311073  NA   2    1 0.00
#232 106311073  25   2    2 0.06
#           id age sex mday b12
#255 107210733  NA   2    1   0
#256 107210733  26   2    2   0
#           id age sex mday b12
#270 107430143  NA   2    1   0
#271 107430143  20   2    2   0
#           id age sex mday b12
#406 202610183  NA   2    1   0
#407 202610183  30   2    2   0
#           id age sex mday b12
#454 203320513  NA   2    1   0
#455 203320513  22   2    2   0
#           id age sex mday   b12
#571 206340523  NA   2    1  0.25
#572 206340523  24   2    2 62.14
#           id age sex mday b12
#662 208630333  NA   2    1   0
is.numeric(zambia_wom$age)
is.numeric(zambia_wom$id)

# Define the impute mean function
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# To repair missing ages
zambia_wom <- zambia_wom %>% group_by(id) %>% 
  mutate(age= case_when(age <18 ~ NA_real_ , TRUE~age)) %>% 
  # replace 0's for age as NA so can impute mean when we have a second entry
  # this is also the case for some low numbers, eg age 2, so do the same
  mutate(age2 = impute.mean(age)) %>% #impute missing ages with the mean of age for second entry
  mutate(age = age2) %>%
  select(!age2) %>% 
  filter(!is.na(age)) %>% #dropped the two observations that with missing age
  # in the case where there are different ages for same individual, take the max age
  filter(age>=18) %>% #only keep ages >= 18 because only adults in this sample
  ungroup

range(zambia_wom$age)
# [1]  18 70


# Now you get the next error
#f_check_data: the next columns in the data contain different values per individual
#
#[1] "age"
#Error in f_check_data(f.spade, frml_list = frml_list, data = data, name.data.orig = dt.name,  : 
#		Correct your call to spade

# This means that some persons have two different ages. This may be caused
# by the fact that the records contain hte actual age during the 24hours recall

# The next scripts sets the age the the first age (min(age))

ids_data <- unique(zambia_wom$id)
for (idid in ids_data){
  data.id <- zambia_wom[zambia_wom$id == idid, ]
  if(nrow(data.id) > 1){
    zambia_wom[zambia_wom$id == idid,"age"] <- 
      min(zambia_wom[zambia_wom$id == idid,"age"])
  }
}

# Now that we have fixed these errors, save the processed data

save(zambia_wom, file=here("data", "processed", "zambia_wom"), replace) 
