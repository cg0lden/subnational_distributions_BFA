# netherlands  data cleaning
# File created by Simone Passarelli 3/29/21
# Clean netherlands data for subnational distributions

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# These are the nutrient composition data for adults; see if you can use it for the omega-3 values in children's data
nevotable <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "nevotable_fcs2012_2016.sas7bdat")) 
#   clean_names() 
#   
# nestable <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "nestable_fcs2012_2016.sas7bdat")) %>% 
#   clean_names() 

# read in the identifying information
# 4/6/21 added weights
iddata1 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "participant.sas7bdat")) %>% 
  clean_names()%>% select(p_id, sex, age, w_demog_season) %>% rename(weights=w_demog_season)

iddata2 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children","SAS", "participant.sas7bdat")) %>% 
  clean_names()%>% select(p_id, sex, age, w_demog_season) %>% mutate(sex=as.numeric(sex)) %>% rename(weights=w_demog_season)

iddata3 <- read_sas(here( "data", "raw", "Netherlands", "FCS2011_Elderly","SAS", "participant.sas7bdat")) %>% 
  clean_names() %>% select(p_id, sex, age, w_id_age_sex_municip_season_wknd) %>% mutate(sex=as.numeric(sex)) %>% 
  rename(weights=w_id_age_sex_municip_season_wknd)

# Load the raw netherlands data 

# Adults, 2012-2016
netherlands1 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% select(!c(cd_id, e_nkj, e_nmj, prot_veg, prot_ani, org_acid, syn_fol, ufa_cis, b1_mj, b3_mj,fibre_mj,
                              foleq, fe_haem, fe_nonhaem, modisac, polysac,vitk1, water, epa, dha, ret )) %>% 
  rename(omega_3=marine, potas=kalium, carb=carbo, phos=phosphorus, kcal=enkcal, retinol=rae) %>% 
 select(!c(e_nprot:e_norg_acid))  %>% mutate(type="adult") %>% left_join(iddata1) %>% rename(mday=m_day)

# Children 2006
#Merge in food composition table from adults
fish <- read_sas(here( "data", "raw", "Netherlands", "DNFCS2012_2016","SAS", "nevotable_fcs2012_2016.sas7bdat")) %>% 
  clean_names() %>% select(c(nevocode, prod_desc, prod_oms, epa, dha)) %>% 
  mutate(omega_3_100=epa+dha) %>% rename(nevo_code=nevocode)

# load in individual food level data
child_omega_3 <- read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children", "SAS", "consumption_food_nut.sas7bdat")) %>% 
  clean_names() %>% select(p_id, nevo_code, mday, nevocoef, cons_qty) %>% 
  left_join(fish, by="nevo_code") %>% select(!c(epa, dha)) %>% 
  mutate(omega_3=(omega_3_100*cons_qty*nevocoef)/100) %>% 
  mutate(omega_3 = replace_na(omega_3, 0)) %>%   # now add up the omega_3 amount so I can later merge into daily consumption file
group_by(p_id, mday) %>% summarize(omega_3=sum(omega_3)) %>% distinct()

netherlands2 <-read_sas(here( "data", "raw", "Netherlands", "DNFCS_young_children", "SAS", "consumption_food_nut.sas7bdat")) %>% 
  clean_names() %>% select(!c(fco: nevo_name)) %>% select(!c(water, enkj, cd_id, polycho, foleq, modicho, unsfa, retino_leq)) %>%  
  rename(mufa=eov, pufa=mov, potas=k, carb=cho, folate=folium, phos=fo, kcal=enkcal, retinol=rae, m_day=mday, fibre=fiber) %>% 
  mutate(b_carotene=NA_real_, la=NA_real_, ala=NA_real_, vitk=NA_real_, iodine=NA_real_, 
         na=NA_real_, vit_b3=NA_real_, type="child") %>% left_join(iddata2) %>% drop_na(ca) %>% 
  rename(mday=m_day) %>% 
  group_by(p_id, mday, age, type, sex, weights) %>% 
  summarize(kcal=sum(kcal),
            prot=sum(prot),
            fat=sum(fat),
            sfa=sum(sfa),
            tfa=sum(tfa),
            carb=sum(carb),
            alcohol=sum(alcohol),
            fibre=sum(fibre),
            ca=sum(ca),
            fe=sum(fe),
            retinol=sum(retinol),
            vit_b1=sum(vit_b1),
            vit_b2=sum(vit_b2),
            vit_b6=sum(vit_b6),
            folate=sum(folate),
            vit_b12=sum(vit_b12),
            vit_c=sum(vit_c),
            vit_d=sum(vit_d),
            vit_e=sum(vit_e),
            mufa=sum(mufa),
            pufa=sum(pufa),
            cholest=sum(cholest),
            phos=sum(phos),
            mg=sum(mg),
            zn=sum(zn),
            se=sum(se),
            cu=sum(cu),
            potas=sum(potas),
            b_carotene=sum(b_carotene),
            la=sum(la),
            ala=sum(ala),
            vitk=sum(vitk),
            iodine=sum(iodine),
            na=sum(na),
            vit_b3=sum(vit_b3)) %>% 
  #merge in the omega-3 data
  left_join(child_omega_3, by=c("p_id", "mday")) %>% mutate(mday=as.numeric(mday))

# 2011 elderly
netherlands3 <-read_sas(here( "data", "raw", "Netherlands", "FCS2011_Elderly", "SAS", "consumption_food_nut_per_day.sas7bdat")) %>% 
  clean_names() %>% 
  select(!c(e_nprot: e_norg_acid)) %>% select(!c(fatty_a, ufa_cis, epa, dha, org_acid, fibre_mj, fe_haem, fe_nonhaem))%>% 
  select(!c(cd_id, enkcal_excl, e_nmj, e_nmj_excl, prot_veg, prot_ani, modisac, polysac, syn_fol, water, foleq, ret, re)) %>% 
  rename(omega_3=marine, phos=ph, kcal=enkcal, carb=carbo, retinol=rae) %>%  
  mutate(vitk=NA_real_, folate=NA_real_, vit_b3=NA_real_, type="elderly")%>% left_join(iddata3) %>% 
  rename(mday=m_day) %>% mutate(mday=as.numeric(mday))



netherlands1 <- netherlands1[ , order(names(netherlands1))]
netherlands2 <- netherlands2[ , order(names(netherlands2))]
netherlands3 <-netherlands3[ , order(names(netherlands3))]

summary(netherlands1)
summary(netherlands2)
summary(netherlands3)

names(netherlands1)
names(netherlands2)
names(netherlands3)

#Combine the elderly, adult, and child datasets
netherlands <- rbind(netherlands1, netherlands2, netherlands3) 

# Trouble matching names

identical(names(netherlands1), names(netherlands2) )

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

# rename variables and sum them 

names(netherlands)

netherlands_nut <- netherlands %>% 
  # mutate_all(~replace(., is.na(.), 0)) %>% 
  rename(vita = retinol,
         fola=folate,
            calc = ca,
         iron=fe,
            vite=vit_e,
            energy=kcal,
            protein=prot,
            fiber=fibre,
            thia=vit_b1,
            ribo=vit_b2,
            niac=vit_b3,
            vitb6=vit_b6,
            fola=folate,
            vitd=vit_d,
            vitc=vit_c,
            pota=potas,
         vitb12=vit_b12,
         iod=iodine,
            betacarot=b_carotene,
         zinc=zn,
         id=p_id)  


# netherlands_merge_2 <-  merge(netherlands_merge, netherlands_weights, by.all="id", all.x=T)

# Need to update the id numbers because they are redundant

summary(netherlands1$p_id)
summary(netherlands2$p_id)
summary(netherlands3$p_id)

# on 4/20/21: I updated the id's to be grouped by dataset type so that they are unique
# Rename and format variables for spade
netherlands_spade <- netherlands_nut %>% 
  group_by(id, type) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id=as.integer(id)) %>% distinct()


# Check for missing or differen ages
netherlands_missings <- netherlands_spade[is.na(netherlands_spade$age), ] # shows you the missings
netherlands_missings   

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



