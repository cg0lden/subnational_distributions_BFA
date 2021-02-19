# Clean Bulgaria data for spade
# Created December 10th, 2020 by Simone Passarelli
library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the csv files for both children (2007) and adults (2004)

# The subject number is not unique to the dataset; must process them separately
#2007=children only
bulg_c <- read_dta(here( "data", "raw", "Bulgaria", "bulg_consumption_merged_omega.dta")) %>% 
  clean_names() %>% filter(consumption_year==2007) %>% 
  select(-c(round, season, respondent, consumption_day, consumption_month, week_day, exception_day, consumption_time_hour,
            consumption_time_minutes, meal, place, food_type, recipe_code, recipe_descr, recipe_descr_eng, foodex2_recipe_code, 
            amount_recipe, ingredient, foodex2_ingr_code, foodex2_ingr_descr,  foodex2_recipe_descr, food_amount_unproc, food_amount_cons, water_g, v_prot_g, retol_mcg, merge))
    # 
# # 2004=adults only
# bulg_a <- read_dta(here( "data", "raw", "Bulgaria", "bulg_consumption_merged_omega.dta")) %>% 
#   clean_names() %>% filter(consumption_year==2004) %>% 
#   select(subject, consumption_year, survey_day, vita_rae_mcg, omega_3_100, calc_mg, iron_mg, vitb12_mcg, zinc_mg, food_amount_reported, ingredient_eng, code_ingredient) 

#id data children
# id data
bulg_c_id <- read_csv(here( "data", "raw", "Bulgaria", "BGR_00441", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct() %>% 
  mutate(age=floor(age_year)) %>% rename(id=subject)


# Combine child and adult dataframes
bulg_nut <- bulg_c %>% 
  mutate(omega_3_g = omega_3_100 *(food_amount_reported/100)) %>% 
  rename(mday = survey_day, id=subject) %>% 
  select(-omega_3_100) %>% 
  mutate(ingredient_eng=tolower(ingredient_eng)) %>%  
  mutate_at(vars(energy_kcal:omega_3_g), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12_mcg),
            iron = sum(iron_mg),
            vita = sum(vita_rae_mcg),
            calc = sum(calc_mg),
            zinc = sum(zinc_mg),
            omega_3 = sum(omega_3_g),
            energy=sum(energy_kcal),
            protein=sum(protein_g),
            carb=sum(carboh_g),
            fiber=sum(fibtg_g),
            fat=sum(fat_g),
            satfat=sum(sat_fat_g),
            mufa=sum(mufa_g),
            pufa=sum(pufa_g),
            vitc=sum(vitc_mg),
            thia=sum(thia_mg),
            ribo=sum(ribo_mg),
            niac=sum(niac_mg),
            betacarot=sum(bcarot_mcg),
            vitd=sum(vitd_mcg),
            vite=sum(vite_mg),
            alcohol=sum(alcohol_g),
            adsugar=sum(adsugar_g),
            chol=sum(chol_mg),
            mg=sum(mg_mg),
            phos=sum(phos_mg),
            pota=sum(pota_mg),
            na=sum(na_mg),
            cu=sum(cu_mg),
            iod=sum(iod_mg),
            se=sum(se_mg)) %>% distinct()


# Merge in the the identifying information 
bulg_spade <- bulg_nut %>%
  left_join(bulg_c_id, by=c("id")) %>% 
  mutate(id=as.integer(id)) %>% select(!age_year)



#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
ids_data <- unique(bulg_spade$id)
for (idid in ids_data){
  data.id <- bulg_spade[bulg_spade$id == idid, ]
  if(nrow(data.id) > 1){
    bulg_spade[bulg_spade$id == idid,"age"] <- 
      min(bulg_spade[bulg_spade$id == idid,"age"])
  }
}

save(bulg_spade, file=here("data", "processed", "Subnational distributions", "bulg"), replace)   



