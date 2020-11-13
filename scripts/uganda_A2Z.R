# Prep dataset for SPADE
# Uganda A27 dataset

library(tidyverse)
library(haven)
library(here)

# Load the Zambia data from the Stata file

uganda_a <- read_dta(here("data", "raw", "Uganda-A2Z fg24.dta"))


# Limit to variables needed for analysis
uganda_a_nut <- uganda_a %>%
  group_by(hid, pid, Recall) %>%
  summarize(sum_b12 = sum(VIT_B12_MCG),
            sum_iron = sum(IRON_MG),
            sum_zinc = sum(ZINC_MG),
            sum_vita = sum(VIT_A_MCG_RAE)
  )

# Merge in identifiers, incl age/sex
uganda_a_merge<-uganda_a %>%
  select(agey, sex, hid, pid, Recall) %>%
  distinct() # drop duplicates (since there is one observation per food item)

# Rename and format variables for spade
uganda_a_nut_spade <- uganda_a_nut %>% left_join(uganda_a_merge, by=c("hid", "pid", "Recall")) %>%
  rename(age=agey, b12=sum_b12, sum_iron = iron, sum_zinc = zinc, sum_vita=vita) %>%
  mutate(hid=as.character(hid),
         pid=as.character(pid)) %>%
  mutate(id=paste0(hid, pid)) %>%
  mutate(mday = Recall) %>%
  ungroup() %>%
  select(id, age, sex, mday, b12, iron, zinc, vita)

uganda_a_nut_spade$id <- as.integer(uganda_a_nut_spade$id)

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
f.spade_wom <- uganda_a_nut_spade %>%
  filter(sex==2)

f.spade_men <- uganda_a_nut_spade %>%
  filter(sex==1)

# Running Spade

# For women
f.spade(frml.ia=b12~fp(age),
        frml.if=b12~fp(age),
        data=f.spade_wom,
        seed=10,
        min.age=1,
        max.age=69,
        sex.lab="female",
        output.name = "b12_uganda_a",
        spade.output.path = here("results", "uganda_a_women"),
        age.classes=c(1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-70)
)

