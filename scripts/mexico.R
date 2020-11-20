
library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the Mexico data from the Stata file (coded with the food groups)

zambia <- read_dta(here( "data", "raw", "Mexico", "Base ENSANUT 2016 entrega_11_11_2020_coded.dta")) %>% clean_names()
