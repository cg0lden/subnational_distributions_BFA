# netherlands  data cleaning
# File created by Simone Passarelli 3/29/21
# Clean netherlands data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)


# These are the supplement data
# nevotable <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "nevotable_fcs2012_2016.sas7bdat")) %>% 
#   clean_names() 
#   
# nestable <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "nestable_fcs2012_2016.sas7bdat")) %>% 
#   clean_names() 

# read in the identifying information

iddata1 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "participant.sas7bdat")) %>% 
  clean_names() %>% select(p_id, sex, age)

iddata2 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children","SAS", "participant.sas7bdat")) %>% 
  clean_names()%>% select(p_id, sex, age) %>% mutate(sex=as.numeric(sex))

iddata3 <- read_sas(here( "data", "raw", "Netherlands", "FCS2011_Elderly","SAS", "participant.sas7bdat")) %>% 
  clean_names()%>% select(p_id, sex, age) %>% mutate(sex=as.numeric(sex))


# Load the raw netherlands data 

# Adults, 2012-2016
netherlands1 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% select(!c(cd_id, e_nkj, e_nmj, prot_veg, prot_ani, org_acid, syn_fol, ufa_cis, b1_mj, b3_mj,fibre_mj,
                              foleq, fe_haem, fe_nonhaem, modisac, polysac,vitk1, water, epa, dha, ret )) %>% 
  rename(omega_3=marine, potas=kalium, carb=carbo, phos=phosphorus, kcal=enkcal, retinol=rae) %>% 
 select(!c(e_nprot:e_norg_acid))  %>% mutate(type="adult") %>% left_join(iddata1)

# Children 2006
netherlands2 <-read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children", "SAS", "consumption_food_nut.sas7bdat")) %>% 
  clean_names() %>% select(!c(fco: nevo_name)) %>% select(!c(water, enkj, cd_id, polycho, foleq, modicho, unsfa, retino_leq)) %>%  
  rename(mufa=eov, pufa=mov, potas=k, carb=cho, folate=folium, phos=fo, kcal=enkcal, retinol=rae, m_day=mday, fibre=fiber) %>% 
  mutate(b_carotene=NA_real_, la=NA_real_, ala=NA_real_, omega_3=NA_real_, vitk=NA_real_, iodine=NA_real_, 
         na=NA_real_, vit_b3=NA_real_, type="child") %>% left_join(iddata2)

# 2011 elderly
netherlands3 <-read_sas(here( "data", "raw", "Netherlands", "FCS2011_Elderly", "SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% 
  select(!c(e_nprot: e_norg_acid)) %>% select(!c(fatty_a, ufa_cis, epa, dha, org_acid, fibre_mj, fe_haem, fe_nonhaem))%>% 
  select(!c(cd_id, enkcal_excl, e_nmj, e_nmj_excl, prot_veg, prot_ani, modisac, polysac, syn_fol, water, foleq, ret, re)) %>% 
  rename(omega_3=marine, phos=ph, kcal=enkcal, carb=carbo, retinol=rae) %>%  
  mutate(vitk=NA_real_, folate=NA_real_, vit_b3=NA_real_, type="elderly")%>% left_join(iddata3)



netherlands1 <- netherlands1[ , order(names(netherlands1))]
netherlands2 <- netherlands2[ , order(names(netherlands2))]
netherlands3 <-netherlands3[ , order(names(netherlands3))]

LEFT OFF HERE: HAVE TO FILTER OUT THE 619 ZEROES FROM NETHERLANDS2

summary(netherlands1)
summary(netherlands2)
summary(netherlands3)

names(netherlands1)
names(netherlands2)
names(netherlands3)

#Combine the elderly, adult, and child datasets
netherlands <- rbind(netherlands1, netherlands2, netherlands3) %>% mutate(m_day=as.numeric(m_day))



# see which variables are blank
summary(netherlands)

# To look at fish variables
# 
# netherlands_fish <- netherlands %>% select(ingr_descr_eng, foodex2_ingr_code, ingr_code) %>% distinct()
# 
# write_csv(netherlands_fish, here( "data", "raw", "netherlands", "netherlands_ingredients.csv"))

# Merge in aquatic food values
# netherlands_omega <- read_excel(here( "data", "raw", "netherlands", "netherlands_ingredients_dha_epa_DV.xlsx")) %>% 
#   select("EPA+DHA", "ingr_descr_eng", "ingr_code") %>% rename("omega_3_100"="EPA+DHA") 


#Exclude food supplements 

# Look at ingredients
food_names <- netherlands %>% distinct(ingr_descr_eng, ingr_code) 

# rename variables and sum them 

netherlands_nut <- netherlands %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(!c(vitk, note, cu, iod, n6, plant_n3, tfa, adsugar, plantpro, animalpro, dairypro, water)) %>% 
  select(!c(recall_d:ingr_amount_proc)) %>% 
  rename(mday=recall_n) %>% 
  group_by(id, mday, age, sex) %>%
  summarize(vitb12 = sum(vitb12),
            iron = sum(fe),
            zinc = sum(zn),
            vita = sum(vita),
            calc = sum(ca),
            vite=sum(vite),
            energy=sum(energy),
            protein=sum(totalpro),
            carb=sum(carb),
            fiber=sum(fiber),
            fat=sum(totalfat),
            satfat=sum(sfa),
            mufa=sum(mufa),
            pufa=sum(pufa),
            thia=sum(vitb1),
            ribo=sum(vitb2),
            niac=sum(vitb3),
            vitb6=sum(vitb6),
            fola=sum(fol),
            vitd=sum(vitd),
            vitc=sum(vitc),
            phos=sum(ph),
            mg=sum(mg),
            na=sum(na),
            pota=sum(k),
            chol=sum(chol),
            se=sum(se),
            betacarot=sum(bcarot),
            retinol=sum(retinol),
            sucrose=sum(totalsucrose),
            alcohol=sum(alcohol),
            omega_3=sum(seafood_n3)) %>% distinct() 


# netherlands_merge_2 <-  merge(netherlands_merge, netherlands_weights, by.all="id", all.x=T)

# Rename and format variables for spade
netherlands_spade <- netherlands_nut %>% 
  group_by(id) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
netherlands_missings <- netherlands_spade[is.na(netherlands_spade$age), ] # shows you the missings
netherlands_missings   

#No missing ages
# 
# #Replace weights with weight from day 1
# ids_data <- unique(netherlands_spade$id)
# for (idid in ids_data){
#   data.id <- netherlands_spade[netherlands_spade$id == idid, ]
#   if(nrow(data.id) > 1){
#     netherlands_spade[netherlands_spade$id == idid,"weight"] <- 
#       min(netherlands_spade[netherlands_spade$id == idid,"weight"])
#   }
# }

#Replace any cases where the ages are different for the same individual

ids_data <- unique(netherlands_spade$id)
for (idid in ids_data){
  data.id <- netherlands_spade[netherlands_spade$id == idid, ]
  if(nrow(data.id) > 1){
    netherlands_spade[netherlands_spade$id == idid,"age"] <- 
      min(netherlands_spade[netherlands_spade$id == idid,"age"])
  }
}

save(netherlands_spade, file=here("data", "processed","Subnational distributions", "netherlands"), replace)   



