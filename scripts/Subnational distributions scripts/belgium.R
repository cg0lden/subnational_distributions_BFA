# belgium (2014-2015) National data cleaning
# File created by Simone Passarelli 3/17/21
# Clean belgium data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Load the raw belgium data 

belgium <- read_excel(here( "data", "raw","Subnational distributions paper data", "Belgium", "allnutrients.xlsx")) %>% clean_names()
names(belgium)

# Round the age variable down to nearest year
belgium$age <- floor(belgium$age)

# see which variables are blank
summary(belgium)

# rename variables and sum them 

belgium_spade <- belgium %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  rename(mday=int_num, weights=wfin_epic, calc=ca_mg, cu=cu_mg, cholest=chorl_mg, carb=cho_g_en,
         kcal=enerc_kcal, omega_3=epa_dha_g_en, la=f18_2cn6_g, omega_6=fapun6_g,
         mufa=famscis_g, pufa=fapu_g, satfat=fasat_g, tfat=fatrn_g,
         fat=fat_g, iron=fe_mg, fiber=fibt_g, fola=fol_ug, iodine=id_ug, 
         pota=k_mg, mg=mg_mg, na=na_mg, sugar=sugar_g, protein=prot_g, phos=p_mg,
         ribo=ribf_mg, se=se_ug, vita=vita_ug, vitb12=vitb12_ug,
         vitb6=vitb6_mg, vitc=vitc_mg, vitd=vitd_ug, vite=vite_mg, vitk=vitk_ug, zinc=zn_mg) %>% 
  select(!c(f20_5cn3_g, f20_5cn3_g_en, starch_g, starch_g_en, sugar_g_en, fapu_g_en,
            famscis_g_en, fapun3_g,fapun3_g_en, fibt_g_en, fapun6_g_en, f18_2cn6_g_en,
            fasat_g_en, fatrn_g_en, fat_g_en, prot_g_en)) %>% 
  mutate(id=as.integer(id)) %>% 
  group_by(id, mday, age, sex, weights) %>%
  summarize(calc = sum(calc),
            cholest=sum(cholest),
            carb=sum(carb),
            cu=sum(cu),
            energy=sum(kcal),
            omega_3=sum(omega_3),
            la=sum(la),
            mufa=sum(mufa),
            omega_6=sum(omega_6),
            pufa=sum(pufa),
            satfat=sum(satfat),
            tfat=sum(tfat),
            fat=sum(fat),
            iron = sum(iron),
            fiber=sum(fiber),
            fola=sum(fola),
            iodine=sum(iodine),
            pota=sum(pota),
            mg=sum(mg),
            na=sum(na),
            protein=sum(protein),
            phos=sum(phos),
            ribo=sum(ribo),
            se=sum(se),
            sugar=sum(sugar),
            vita = sum(vita),
            vitb12 = sum(vitb12),
            vitb6=sum(vitb6),
            vitc=sum(vitc),
            vitd=sum(vitd),
            vite=sum(vite),
            vitk=sum(vitk),
            zinc = sum(zinc)) %>% distinct() 

save(belgium_spade, file=here("data", "processed","Subnational distributions", "belgium"), replace)   



