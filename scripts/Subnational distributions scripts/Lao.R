# Clean Lao data for 

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Lao data from the Stata file (coded with the food groups)

Lao <- read_csv(here( "data", "raw", "Lao", "consumption_user.csv")) %>% clean_names() %>% 
  mutate(na=as.numeric(x57)) %>% 
  select(-c(adm0_code:round, consumption_day:food_amount_reported, water, a_prot, v_prot,
            sat_fat, zinc, vitb6, fola, vitb12, bcarot, mg:vitk))

# Read in dta file with omega and b12
Lao_2 <- read_dta(here( "data", "raw", "Lao", "Lao.dta")) %>% clean_names() %>% 
  select(survey_day, subject, b12, omega_3, ingredient, age_year, sex, zinc) %>% 
  rename(recall = survey_day, id=subject, age=age_year) %>% 
  mutate(omega_3 = replace(omega_3, ingredient=="Golden applesnail, Channeled applesnail, boiled", 0.12)) 

Lao_age_sex <- Lao_2 %>% select(age, id, sex) %>% distinct()
# Summarize vitb12 and omega3 variables
Lao_b12_omega_zinc <- Lao_2 %>%  group_by(id, recall) %>%   
  summarize(vitb12 = sum(b12),
            zinc=sum(zinc),
            omega_3 = sum(omega_3)) %>% distinct() %>% 
  left_join(Lao_age_sex, by="id")

# left off here on 2/23/21 
#Have to replace all missings were zeroes, have to merge in the LAO b12 and omega data 

summary(Lao_b12_omega_zinc)
table(Lao$survey_day)
# Need to add snail nutrients

# Red meat=9, processed meat=10

Lao_nut <-  Lao %>% 
  rename(recall = survey_day, id=subject) %>% 
  group_by(id, recall) %>% 
  summarize(energy=sum(energy),
            fat = sum(fat),
            protein=sum(protein),
            carb=sum(carboh),
            fiber=sum(fibtg),
            vitc=sum(vitc),
            thia=sum(thia),
            ribo=sum(ribo),
            niac=sum(niac),
            iron = sum(iron),
            vita = sum(vita),
            calc = sum(calc),
            na=sum(na)) %>% distinct()

# Merge in all of the aggregated b12 and omega data
Lao_3 <- Lao_nut %>% left_join(Lao_b12_omega_zinc, by=c("recall", "id")) %>% 
  mutate_all(~replace(., is.na(.), 0))



summary(Lao_3)
# Lao_merge_2 <-  merge(Lao_merge, Lao_weights, by.all="id", all.x=T)

# Rename and format variables for spade
Lao_spade <- Lao_3 %>% 
  mutate(mday = recall) %>%
  group_by(id) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
Lao_missings <- Lao_spade[is.na(Lao_spade$age), ] # shows you the missings
Lao_missings   

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(Lao_spade$id)
for (idid in ids_data){
  data.id <- Lao_spade[Lao_spade$id == idid, ]
  if(nrow(data.id) > 1){
    Lao_spade[Lao_spade$id == idid,"age"] <- 
      min(Lao_spade[Lao_spade$id == idid,"age"])
  }
}

save(Lao_spade, file=here("data", "processed", "Subnational distributions", "Lao"), replace)   



