# Zambia dataset cleaning and spade operations

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(SPADE.RIVMNwCore)

# Load the Zambia data from the Stata file

zambia <- read_dta(here("data", "Zambia fg24.dta")) %>% clean_names()


# Limit to variables needed for analysis
zambia_nut <- zambia %>%
  group_by(hid, pid, recall) %>%
  summarize(sum_b12 = sum(vit_b12_mcg),
            sum_iron = sum(iron_mg),
            sum_zinc = sum(zinc_mg),
            sum_vita = sum(vit_a_mcg_rae)
  )


# Merge in identifiers, incl age/sex
zambia_merge <- zambia %>%
  dplyr::select( agey, sex, recall, hid, pid) %>%
    distinct() 
# drop duplicates (since there is one observation per food item)
  
# Rename and format variables for spade
zambia_spade <- zambia_nut %>% left_join(zambia_merge, by=c("hid", "pid", "recall")) %>%
  rename(age=agey, b12=sum_b12, iron = sum_iron, zinc = sum_zinc, vita=sum_vita) %>%
  mutate(hid=as.character(hid),
         pid=as.character(pid)) %>%
  mutate(id=paste0(hid, pid)) %>%
  mutate(mday = recall) %>%
  ungroup() %>%
  dplyr::select(id, age, sex, mday, b12, iron, zinc, vita)

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

# Make separate datasets for men and women
zambia_wom <- zambia_spade %>%
  filter(sex==2)

# Save the dataset and run the spade program in a separate file WITHOUT tidyverse
# For women
# f.spade(frml.ia=b12~fp(age),
#         frml.if=b12~fp(age),
#         data=zambia_wom,
#         seed=10,
#         min.age=18,
#         max.age=69,
#         sex.lab="female"
# )
# output.name = "b12_zambia",
# spade.output.path = here("Results", "Zambia_women")
#age.classes=c(1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 
              # 45-49, 50-54, 55-59, 60-64, 65-70),

