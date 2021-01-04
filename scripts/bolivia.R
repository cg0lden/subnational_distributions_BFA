# Clean Bolivia data for spade
# Created December 15th, 2020 by Simone Passarelli
library(tidyverse)
library(haven)
library(here)
library(janitor)

# Load the csv files for

bolivia <- read_csv(here( "data", "raw", "Bolivia", "consumption_user.csv"))%>% 
  clean_names() %>% 
  select(subject, survey_day, vita, calc, iron, vitb12, zinc,
         food_amount_reported, ingredient, code_ingredient, foodex2_ingr_descr) 

# id data
bolivia_id <- read_csv(here( "data", "raw", "Bolivia",  "subject_user.csv"))%>% 
  clean_names() %>% select(subject, sex, age_year) %>% group_by(subject) %>% distinct() %>% 
  rename(id=subject , age=age_year)

# To add omega-3 content for the four types of fish reported
bolivia <- bolivia %>% mutate(ingredient=tolower(ingredient)) %>% 
  mutate(omega_3_100 = case_when(ingredient == "fish, tuna, light, canned in oil, drained solids" ~ (0.027 + 0.101),
                                                  ingredient == "fish, sardine, atlantic, canned in tomato sauce, drained solids with bone" |
                                   ingredient == "fish, sardine, atlantic, canned in oil, drained solids with bone" ~ (0.473 + 0.509),
                                 ingredient == "fish, carp, cooked, dry heat" ~ (0.372 + 0.178),
                                 ingredient == "fish, tilapia, cooked, dry heat" ~ (0.005 + 0.13), TRUE ~ 0 ))


# Combine child and adult dataframes
bolivia_nut <- bolivia %>% 
  mutate(omega_3_g = omega_3_100 *(food_amount_reported/100)) %>% 
  rename(mday = survey_day, id=subject) %>% 
  mutate(red_meat = case_when((grepl("beef", ingredient)) ~ food_amount_reported,
                              (grepl("sausage", ingredient)) ~ food_amount_reported,
                              (grepl("frankfurter", ingredient)) ~ food_amount_reported,
                              (grepl("veal", ingredient)) ~ food_amount_reported,
                              (grepl("steak", ingredient)) ~ food_amount_reported,
                              (grepl("pork", ingredient)) ~ food_amount_reported,
                              (grepl("lamb", ingredient)) ~ food_amount_reported,
                              (grepl("mutton", ingredient)) ~ food_amount_reported,
                              (grepl("goat", ingredient)) ~ food_amount_reported,
                              (grepl("burger", ingredient)) ~ food_amount_reported,
                              (grepl("ham", ingredient)) ~ food_amount_reported,
                              (grepl("bacon", ingredient)) ~ food_amount_reported,
                              (grepl("salami", ingredient)) ~ food_amount_reported,
                              (grepl("wurst", ingredient)) ~ food_amount_reported,
                              (grepl("wiener", ingredient)) ~ food_amount_reported,
                              TRUE ~ 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("chicken", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("tripe", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("yogurt", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("duck", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("hen", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("goose", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("geese", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("poultry", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("turkey", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("rabbit", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("stomach", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("liver", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("blood", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("brain", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("offal", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("haggis", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("milk", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, (grepl("cheese", ingredient)), 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="bouillon", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="pork fat", 0)) %>% 
  mutate(red_meat = replace(red_meat, ingredient=="pate", 0)) %>% 
  relocate(red_meat , omega_3_g, after=zinc) %>% 
  relocate(id, mday, ingredient, food_amount_reported,  before=red_meat) %>% 
  mutate_at(vars(red_meat:vitb12), ~replace(., is.na(.), 0)) %>% 
  group_by(id, mday) %>%
  summarize(b12 = sum(vitb12),
            iron = sum(iron),
            vita = sum(vita),
            calc = sum(calc),
            zinc = sum(zinc),
            red_meat = sum(red_meat),
            omega_3 = sum(omega_3_g)) %>% distinct()


# Merge in the the identifying information 
bolivia_spade <- bolivia_nut %>%
  left_join(bolivia_id, by=c("id")) %>% 
  mutate(id=as.integer(id)) 



#No missing ages
# 
# #Replace any cases where the ages are different for the same individual
# 
ids_data <- unique(bolivia_spade$id)
for (idid in ids_data){
  data.id <- bolivia_spade[bolivia_spade$id == idid, ]
  if(nrow(data.id) > 1){
    bolivia_spade[bolivia_spade$id == idid,"age"] <- 
      min(bolivia_spade[bolivia_spade$id == idid,"age"])
  }
}

save(bolivia_spade, file=here("data", "processed", "bolivia"), replace)   



