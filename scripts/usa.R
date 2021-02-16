# NHANES data cleaning for SPADE
# Created by Simone Passarelli on 11/23/2020

library(tidyverse)
library(haven)
library(here)
library(janitor)
# Clean day 1 data and sync names. Keep necessary vars only
nhanes_day1 <- read_xpt(here("data", "raw", "United States", "DR1IFF_J.XPT")) %>% 
  clean_names() %>% mutate(mday=1) %>% 
  rename(id=seqn, code=dr1ifdcd, amount=dr1igrms, vita=dr1ivara, calc=dr1icalc, 
  b12a=dr1ivb12, b12b=dr1ib12a, zinc=dr1izinc, epa=dr1ip205, dha=dr1ip226, iron=dr1iiron, weight1=wtdrd1, weight2=wtdr2d)  %>% 
  select(id, mday, code, amount, vita, b12a, b12b, zinc, iron, calc, epa, dha, weight1, weight2)

#Clean day 2 data and sync names. Keep necessary vars only
nhanes_day2 <- read_xpt(here("data", "raw", "United States", "DR2IFF_J.XPT")) %>% 
  clean_names() %>% mutate(mday=2) %>% 
  rename(id=seqn, code=dr2ifdcd,  amount=dr2igrms, vita=dr2ivara, calc=dr2icalc, 
         b12a=dr2ivb12, b12b=dr2ib12a, zinc=dr2izinc, epa=dr2ip205, dha=dr2ip226, iron=dr2iiron, weight1=wtdrd1, weight2=wtdr2d) %>% 
  select(id, mday, code, amount, vita, b12a, b12b, zinc, iron, calc, epa, dha, weight1, weight2)

#append day 1 and day 2 data
nhanes <- rbind(nhanes_day1, nhanes_day2)

# Merge in and combine food group codes from GDQS when they are available

nhanes_gdqs_meat <- read_dta(here("data", "raw", "NHANES", "FPED_DR_1516.dta")) %>% 
  select(DR1IFDCD, DESCRIPTION, DR1I_PF_MEAT, DR1I_PF_CUREDMEAT) %>% 
  clean_names() %>%
  rename(code=dr1ifdcd) %>% 
  filter(code >=20000000 & code < 30000000 , dr1i_pf_meat > 0 | dr1i_pf_curedmeat>0 ) %>% 
  mutate(red = case_when(dr1i_pf_meat > 0  ~ 1, 
                         TRUE ~ 0)) %>% 
  mutate(processed = case_when(dr1i_pf_curedmeat > 0 ~ 1 , TRUE ~ 0)) %>% 
  select(code, red, processed) %>% 
  distinct()

#Load demographic data for age and sex variables
usa_merge <- read_xpt(here("data", "raw", "NHANES", "DEMO_J.XPT")) %>% 
         clean_names() %>% 
  rename( age=ridageyr, sex=riagendr, id=seqn) %>%
  dplyr::select( age, sex, id) %>%
  distinct() 

nhanes1 <-  merge(nhanes, nhanes_gdqs_meat, by.all="code", all.x=T)

# see how many meat values weren't classified by the gdqs codes
meat_test <- nhanes1 %>% filter(code >=20000000 & code < 30000000) %>% filter(processed==0 & red==0)
# None, they were all classified


usa_nut <- nhanes1 %>% 
  mutate(red_meat = case_when(red >0 ~ amount, 
         TRUE ~ 0),
         processed_meat = case_when(processed >0 ~ amount, TRUE ~ 0)) %>%
  group_by(id, mday) %>%
  summarize(b12 = sum(b12a, b12b),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            red_meat = sum(red_meat),
            processed_meat = sum(processed_meat),
            omega_3 = sum(epa, dha),
            weight1 = weight1,
            weight2=weight2) %>% distinct() 

  
  # Rename and format variables for spade
  usa_spade <- usa_nut %>% 
    left_join(usa_merge, by=c("id")) %>%
    group_by(id, mday) %>%
    distinct()   %>%
  ungroup() %>% 
    dplyr::select(id, weight1, weight2, age, sex, mday, b12, iron, zinc, vita, calc, omega_3, red_meat, processed_meat) %>% 
    mutate(id=as.integer(id))
  
  
  # Check for missing or differen ages
  usa_missings <- usa_spade[is.na(usa_spade$age), ] # shows you the missings
  usa_missings   
  
  #No missing ages
  
  #Replace any cases where the ages are different for the same individual
  
  ids_data <- unique(usa_spade$id)
  for (idid in ids_data){
    data.id <- usa_spade[usa_spade$id == idid, ]
    if(nrow(data.id) > 1){
      usa_spade[usa_spade$id == idid,"age"] <- 
        min(usa_spade[usa_spade$id == idid,"age"])
    }
  }
  
# Update sampling weights
# We want to include both the day 1 and the day 2 data, so we should use the day 1
  
# Change id type to be numeric

  
  save(usa_spade, file=here("data", "processed", "usa"), replace)   
  
  