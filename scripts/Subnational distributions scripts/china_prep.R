# China dataset cleaning and spade operations
# Created by Simone Passarelli 11_18_20

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)



china_1 <- read_sas(here("data", "raw" , "China", "nutr1_00.sas7bdat")) 
china_2 <- read_sas(here("data", "raw" , "China", "nutr2_00.sas7bdat"))
china_3 <- read_sas(here("data", "raw" , "China", "nutr3_00.sas7bdat"))

# Read in sas files downloaded from https://www.cpc.unc.edu/projects/china/data/datasets
china <- read_sas(here("data", "raw" , "China", "nutr3_00.sas7bdat")) %>% 
  clean_names() %>% 
  rename(code = foodcode) %>%
  filter(any(!is.na(code))) %>% 
  mutate(code = as.numeric(code)) %>% 
  filter(wave == 2011) #the 2011 data used the 2002-2004 FCT

# Read in macronutrient file
china_macro <- read_sas(here("data", "raw" , "China", "c12diet.sas7bdat")) %>% 
  clean_names() 

# Read in China food codes translated from Ling
food_codes <- readxl::read_xlsx(here("data", "raw" , "China", "Food code_China.xlsx")) %>% 
  clean_names() %>%
  rename(code = code_2002) %>%
  filter(any(!is.na(code))) %>% 
  mutate(code = gsub("-", "", code)) %>% 
  mutate(code = as.numeric(code))

# Load in the b_12 data
china_b12 <- load(here("data", "raw", "China", "china_b12_values"))

  clean_names() %>%  mutate(code = gsub("-", "", code)) %>% 
  mutate(code = as.numeric(code))


# Make a file with the translated data and nutrients

china_merge <- left_join(china, food_codes, by="code") %>% 
  left_join(china_macro) %>% left_join(china_b12, by="code") 
  
  
  
  save(china_merge, file=here("data", "raw", "China", "china_nutrients"), replace)   



