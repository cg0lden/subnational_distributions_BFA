# Clean portugal data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the portugal data from the Stata file (coded with the food groups)

portugal <- read_dta(here( "data", "raw", "portugal", "Base ENSANUT 2016 entrega_11_11_2020_coded.dta")) %>% clean_names()
names(portugal)

portugal_nut <-  portugal %>% 
  rename(recall = replica, id=folio, age=edad) %>% 
  group_by(id, recall) %>%
  summarize(
    energy=sum(energ_con_1),
    carb=sum(carbohydrt_con_1),
    fat=sum(lipid_tot_con_1),
    protein=sum(protein_con_1),
    fiber=sum(fiber_td_con_1),
    alcohol=sum(alcohol_con_1),
    sugar=sum(sugar_tot_con_1),
    iron = sum(iron_con_1),
    mg=sum(magnesium_con_1),
    phos=sum(phosphorus_con_1),
    pota=sum(potassium_con_1),
    na=sum(sodium_con_1),
    zinc = sum(zinc_con_1),
    cu=sum(copper_con_1),
    mang=sum(manganese_con_1),
    se=sum(selenium_con_1),
    vitc=sum(vit_c_con_1),
    thia=sum(thiamin_con_1),
    ribo=sum(riboflavin_con_1),
    niac=sum(niacin_con_1),
    vitb6=sum(vit_b6_con_1),
    fola=sum(folate_tot_con_1),
    chol=sum(choline_tot_con_1),
    b12 = sum(vit_b12_con_1, vit_b12_add_con_1),
    vita = sum(vit_a_rae_con_1),
    calc = sum(calcium_con_1),
    vitk=sum(vit_k_con_1),
    vite=sum(vit_e_con_1),
    vitd=sum(vit_d_mcg_con_1),
    betacarot=sum(beta_carot_con_1),
    satfat=sum(fa_sat_con_1),
    mufa=sum(fa_mono_con_1),
    pufa=sum(fa_poly_con_1),
    omega_3 = sum(f22d6_con_1, f20d5_con_1)) %>% distinct()

# Merge in identifiers, incl age/sex
portugal_merge <- portugal %>%
  rename( age=edad, sex=sexo, recall=replica, id=folio, weight=ponde_f) %>%
  dplyr::select( age, sex, recall, id, weight)


# portugal_merge_2 <-  merge(portugal_merge, portugal_weights, by.all="id", all.x=T)

# Rename and format variables for spade
portugal_spade <- portugal_nut %>% 
  left_join(portugal_merge, by=c("id", "recall")) %>%
  mutate(mday = recall) %>%
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
portugal_missings <- portugal_spade[is.na(portugal_spade$age), ] # shows you the missings
portugal_missings   

#No missing ages

#Replace weights with weight from day 1
ids_data <- unique(portugal_spade$id)
for (idid in ids_data){
  data.id <- portugal_spade[portugal_spade$id == idid, ]
  if(nrow(data.id) > 1){
    portugal_spade[portugal_spade$id == idid,"weight"] <- 
      min(portugal_spade[portugal_spade$id == idid,"weight"])
  }
}

#Replace any cases where the ages are different for the same individual

ids_data <- unique(portugal_spade$id)
for (idid in ids_data){
  data.id <- portugal_spade[portugal_spade$id == idid, ]
  if(nrow(data.id) > 1){
    portugal_spade[portugal_spade$id == idid,"age"] <- 
      min(portugal_spade[portugal_spade$id == idid,"age"])
  }
}

save(portugal_spade, file=here("data", "processed","Subnational distributions", "portugal"), replace)   



