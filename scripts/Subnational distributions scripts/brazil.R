# Brazil National 2014 data cleaning
# File created by Simone Passarelli 3/19/21
# Clean brazil data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw brazil data 

brazil <- read_csv(here( "data", "raw", "brazil", "POF2017_2018_Harvard.csv")) %>% clean_names()
names(brazil)

brazil_groups <- brazil %>% select(food_group, food_description) %>% filter(food_group=="Suplementos") %>% distinct()

# see which variables are blank
summary(brazil)

# To look at fish variables
brazil_fish <- brazil %>% select(group_code:food_description) %>% distinct()

write_csv(brazil_fish, here( "data", "raw", "Brazil", "brazil_ingredients.csv"))

# Check for supplements:
ingredients <- brazil %>% select(food_group, cod_item, food_description) %>% distinct()


# Merge in epa+dha file
brazil_omega <- read_excel(here("data", "raw", "brazil", "brazil_ingredients_trans_dha_epa.xlsx")) %>% 
  select("EPA+DHA", "cod_item") %>% rename("omega_3_100"="EPA+DHA") 


# rename variables and sum them 

brazil_nut <- brazil %>% mutate_all(~replace(., is.na(.), 0)) %>% 
  left_join(brazil_omega, by="cod_item") %>% 
  mutate(omega_3=(qtd_final*omega_3_100)/100) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
 filter(food_description !="VITAMINAS, MINERAIS E OUTROS") %>% 
  select(!c(strata_pof, group_code, food_group, cod_item, food_description, method_preparation, meal_occasion, hour, qtd_final, energia_kj, atypical_day)) %>% 
  rename(mday=day, id=id_num, age=age_years, weight=sample_weight_pof) %>% 
  group_by(id, mday, age, sex, weight) %>% summarize(energy=sum(energia_kcal),
                                             protein=sum(ptn),
                                             carb=sum(chotot),
                                             fiber=sum(fibra),
                                             fat=sum(lip),
                                             cholest=sum(colest),
                                             satfat=sum(agsat),
                                             mufa=sum(agmono),
                                             pufa=sum(agpoli),
                                             tfat=sum(agtrans),
                                             vitb12 = sum(cobalamina),
                                             iron = sum(ferro),
                                             zinc = sum(zinco),
                                             vita = sum(vita_rae),
                                             calc = sum(calcio),
                                             vite=sum(vite),
                                             thia=sum(tiamina),
                                             ribo=sum(riboflavina),
                                             niac=sum(niacina),
                                             vitb6=sum(piridoxamina),
                                             fola=sum(folato),
                                             vitd=sum(vitd),
                                             vitc=sum(vitc),
                                             phos=sum(fosforo),
                                             mg=sum(magnesio),
                                             na=sum(sodio),
                                             cu=sum(cobre),
                                             pota=sum(potassio),
                                             omega_3=sum(omega_3)) %>% distinct() 


# brazil_merge_2 <-  merge(brazil_merge, brazil_weights, by.all="id", all.x=T)

# Rename and format variables for spade
brazil_spade <- brazil_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
brazil_missings <- brazil_spade[is.na(brazil_spade$age), ] # shows you the missings
brazil_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(brazil_spade$id)
# for (idid in ids_data){
#   data.id <- brazil_spade[brazil_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     brazil_spade[brazil_spade$id == idid,"weight"] <- 
#       min(brazil_spade[brazil_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(brazil_spade$id)
for (idid in ids_data){
  data.id <- brazil_spade[brazil_spade$id == idid, ]
  if(nrow(data.id) > 1){
    brazil_spade[brazil_spade$id == idid,"age"] <- 
      min(brazil_spade[brazil_spade$id == idid,"age"])
  }
}

save(brazil_spade, file=here("data", "processed","Subnational distributions", "brazil"), replace)   



