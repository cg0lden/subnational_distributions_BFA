# Clean romania data for spade

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the rom data from the Stata file (coded with the food groups)
names(rom)
summary(rom)
# Consumption data
rom <- read_csv(here( "data", "raw", "Romania", "consumption_user.csv")) %>% 
  clean_names() %>% select(-c(round, season, respondent, consumption_day:food_amount_reported, a_prot_g, v_prot_g, retol_mcg, eat_seq, water_g)) 
  

# id data
rom_id <- read_csv(here( "data", "raw", "Romania", "subject_user.csv")) %>% 
  clean_names() %>% select(subject, sex, age_year)

summary(rom)
summary(rom_id)
table(rom$survey_day)

rom_nut <-  rom %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate_at(vars(energy_kcal:vitk_mg), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(vitb12 = sum(vitb12_mcg),
            iron = sum(iron_mg),
            vita = sum(vita_rae_mcg),
            calc = sum(calc_mg),
            zinc = sum(zinc_mg),
            omega_3 = sum(seafood_n3_g),
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
            vitb6=sum(vitb6_mg),
            betacarot=sum(bcarot_mcg),
            vitd=sum(vitd_mcg),
            vite=sum(vite_mg),
            omega6=sum(n6_g),
            foldfe_mcg=sum(foldfe_mcg),
            plantomega3=sum(plant_n3_g),
            tfat=sum(tfa_g),
            chol=sum(chol_mg),
            mg=sum(mg_mg),
            phos=sum(phos_mg),
            pota=sum(pota_mg),
            na=sum(na_mg),
            cu=sum(cu_mg),
            se=sum(se_mg),
            vitk=sum(vitk_mg)
            )%>% distinct()

# Merge in identifiers, incl age/sex
rom_merge <- rom_id %>%
  rename( age=age_year, id=subject) %>%
  dplyr::select( age, sex,  id) %>% group_by(id) %>% distinct()


# rom_merge_2 <-  merge(rom_merge, rom_weights, by.all="id", all.x=T)

# Rename and format variables for spade
rom_spade <- rom_nut %>% 
  left_join(rom_merge, by=c("id")) %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or different ages
rom_missings <- rom_spade[is.na(rom_spade$age), ] # shows you the missings
rom_missings   

#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
 ids_data <- unique(rom_spade$id)
 for (idid in ids_data){
   data.id <- rom_spade[rom_spade$id == idid, ]
   if(nrow(data.id) > 1){
     rom_spade[rom_spade$id == idid,"age"] <- 
       min(rom_spade[rom_spade$id == idid,"age"])
   }
 }

save(rom_spade, file=here("data", "processed", "rom"), replace)   



