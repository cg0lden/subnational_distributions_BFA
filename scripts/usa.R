# NHANES data cleaning for SPADE
# Created by Simone Passarelli on 11/23/2020

library(tidyverse)
library(haven)
library(here)
library(janitor)

nhanes_day1 <- read_xpt("/Users/Simone/Downloads/DR2IFF_I.XPT")