# Zambia dataset cleaning and spade operations

library(tidyverse)
library(haven)
library(here)
library(SPADE.RIVMNwCore)

# Load the Zambia data from the Stata file

zambia <- read_dta(here("data", "Zambia fg24.dta"))


# Limit to variables needed for analysis
zambia_b12 <- zambia %>%
  group_by(hid, pid, Recall) %>%
  summarize(sum_b12 = sum(VIT_B12_MCG))

# Merge in identifiers, incl age/sex
zambia_b12_merge<-zambia %>%
  select(agey, sex, hid, pid, Recall) %>%
  distinct() # drop duplicates (since there is one observation per food item)
  
# Rename and format variables for spade
zambia_b12_spade <- zambia_b12 %>% left_join(zambia_b12_merge, by=c("hid", "pid", "Recall")) %>%
  rename(age=agey, b12=sum_b12) %>%
  mutate(hid=as.character(hid),
         pid=as.character(pid)) %>%
  mutate(id=paste0(hid, pid)) %>%
  mutate(mday = Recall) %>%
  ungroup() %>%
  select(id, age, sex, mday, b12)

zambia_b12_spade$id <- as.integer(zambia_b12_spade$id)

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
f.spade_wom <- zambia_b12_spade %>%
  filter(sex==2)

f.spade_men <- zambia_b12_spade %>%
  filter(sex==1)

# Running Spade

# For women
f.spade(frml.ia=b12~fp(age),
        frml.if=b12~fp(age),
        data=f.spade_wom,
        seed=10,
        min.age=1,
        max.age=69,
        sex.lab="female"
)
# output.name = "b12_zambia",
# spade.output.path = here("Results", "Zambia_women")
#age.classes=c(1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 
              45-49, 50-54, 55-59, 60-64, 65-70),


view(f.spade)

sum(b)
