library(tidyverse)
library(haven)
library(here)
library(janitor)
library(readxl)

# Merge in the translated food names to the food code document

# Merge in the translated food codes from YanPing

china_translated <- read_dta(here("data", "raw", "China", "Food code_China_B12_formatted.dta")) %>% 
  clean_names() %>%  filter(!is.na(code)) %>%  rename(foodcode=code) 

# Then, read in the foods reported 
china_foods <- read_sas(here("data", "raw", "China", "nutr3_00.sas7bdat")) %>% 
  clean_names()  %>%  filter(wave==2009)

# Read in b12 values for fish
china_fish_b12 <- read_excel(here("data", "raw", "China", "China_B12 content fish.xlsx")) %>% 
  clean_names() %>% rename(foodcode=code) 

# Read in the B12 values for other fish that were filled in
# Don't need to because this information is combined in the translated food name document

# china_b12 <- read_dta(here("data", "raw", "China", "Food code_China_B12_formatted.dta")) %>% 
#                            clean_names() %>%  rename(foodcode=code)

# Merge china foods with translated list
# and replace the b12 values from the fish into the b12 column with entries from CG student colleague

china_foods_translated <- china_foods  %>% rename(id=i_dind) %>% 
  left_join(china_translated, by=c("foodcode")) %>% left_join(china_fish_b12, by=c("foodcode")) %>% 
  select(!c("best_match_species_in_average", "part", "ingredient")) %>% rename(ingredient=englishname2002) %>% 
  mutate(b12=ifelse(china_foods_translated$vitamin_b12_mcg_100g >=0 & !is.na(china_foods_translated$vitamin_b12_mcg_100g),
        vitamin_b12_mcg_100g, b12))

# Open Data from Yanping with nutrients calculated
china <- read_sas(here("data", "raw", "China", "simone_3days.sas7bdat")) %>% 
  clean_names() %>% rename(id=i_dind)

china_sex <- read_sas(here("data", "raw", "China", "mast_pub_12.sas7bdat")) %>% 
  clean_names() %>% rename(id=idind, sex=gender) %>% select(id, sex) 

china_age <- read_sas(here("data", "raw", "China", "surveys_pub_12.sas7bdat")) %>% 
  clean_names() %>% rename(id=idind) %>% filter(wave==2009) %>% select(age, id) 

# Merge b12 food codes with the consumption data
#
# Clean the China B12 data--so many zeroes

china_b12_clean <- china_foods_translated %>% rename(code=foodcode) %>% 
  filter(!is.na(code)) %>% 
  mutate(b12 = replace(b12, foodcategory1=="Cereals and cereals products" | 
      foodcategory1=="Dried legumes and legumes products" |
      foodcategory1=="Tubers, starches and products" |
      foodcategory1=="Vegetables and vegetables products" |
      foodcategory1=="Fungi and algae" | 
      foodcategory1=="Fruits and fruit products" |
      foodcategory1=="Fruit and fruit products" | 
        foodcategory1=="Liquor and alcoholic beverages" |
        foodcategory2=="Herb" | foodcategory1=="Condiments" | foodcategory2=="Fruit juice and drink" |
        foodcategory2=="Carbonated drink" | foodcategory2=="Tea and tea drink" | foodcategory2=="Sugars" | 
        foodcategory1=="Fats and oil" | foodcategory2=="Confectionery" | 
        foodcategory1=="Nuts and seeds" , 0)) %>% 
  mutate(b12= replace(b12, code %in% 153001:153004 , 0)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate" | ingredient=="Chocolate, filled with liquor", 0.8)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate, standard wafer" , 0.14)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, lean and fat" | ingredient=="Pork, fatty" , 0.53)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, rump" | ingredient=="Pork, rump, *Duchangda*, raw, lean and fat" |
                       ingredient=="Pork, rump, *Liangza*, raw, lean and fat", 0.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, hind hock" | ingredient=="Pork, rib chop" |
          ingredient=="Pork, head skin" | ingredient=="Pork, chop, with rib", 0.54 )) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, rib belly" | ingredient=="Pork, flank", 0.84)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, tenderloin" | ingredient=="Pork, tendon" |
                        ingredient=="Pork, tenderloin, raw, lean and fat", 0.52)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, chop, without rib" | 
                        ingredient=="Pork chop, without rib, *Duchangda*, raw, lean and fat" |
                        ingredient=="Pork chop, without rib, *Liangza*, raw, lean and fat" |
                        ingredient=="Pork, eye muscle or loin, *Liangza*, raw, lean" |
                        ingredient=="Pork, eye muscle or loin, *Duchangda*, raw, lean", 0.53)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, fore hock" | ingredient=="Pork, hock", 0.73)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, lean" , 0.57)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, leg" , 0.63)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, neck" , 0.79)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, large intestine" | ingredient=="Pork, small intestine", 0.1)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, ear" , 0.07)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, foot" | ingredient=="Pork, foot, cooked" |
                        ingredient=="Pork, fore foot, cooked", 0.41)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, picnic butt, *Liangza*, raw, lean and fat" |
                         ingredient=="Pork, picnic butt, *Duchangda*, raw, lean and fat" , 0.91)) %>%
  mutate(b12= replace(b12, (grepl("sparerib", ingredient)), 1.01)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, kidney", ingredient)), 8.49)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, stomach", ingredient)), 0.3)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, lung", ingredient)), 2.75)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, liver", ingredient)), 26)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, brain", ingredient)), 2.19)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, blood", ingredient)), 1)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, heart", ingredient)), 3.79)) %>% 
  mutate(b12= replace(b12, (grepl("Pork, tongue", ingredient)), 2.84)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, cooked with soy sauce", 0.62)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, diced meat, with chili and peanuts, canned" |
                        ingredient=="Pork, shredded tenderloin and glutinous rice, canned", 0.45)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, liver, stewed with spices and soy sauce", 18.67)) %>% 
  mutate(b12= replace(b12, ingredient=="Mixed pork offal, spiced and cooked with soy sauce", 2.76)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, cured meat, salted and smoked" | 
                        ingredient=="Pork, cured meat, salted", 0.29)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, *Barbeque*", 0.79)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, bacon", 0.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, spleen", 3.01)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, *Luncheon meat*", 0.36)) %>% 
  mutate(b12= replace(b12, (grepl("Pork floss", ingredient)) | (grepl("Pork, floss", ingredient)) , 0.83)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, lean and fat, barbecued, cooked with salt and sugar" |
                        ingredient=="Pork, tenderloin, barbecued and smoked, cooked with salt and sugar", 0.36)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, steamed with salt and ginger" , 0.62)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, lean and fat, stewed, flavored" |
                        ingredient=="Pork, hock, stewed with salt, soy sauce and sugar", 0.43)) %>% 
  mutate(b12= replace(b12, (grepl("Pork sausage", ingredient)) | ingredient=="Pork, *hotdog*, sausage, cooked", 0.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork sausage, air-dried" , 1.18)) %>% 
  mutate(b12= replace(b12, (grepl("Pork ham", ingredient)) |  (grepl("Pork, ham", ingredient)), 0.36))  %>% 
  mutate(b12= replace(b12, ingredient=="Pork, face, braised with salt, soy sauce and sugar" , 0.69)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, jerky, lean, coked with salt, soy sauce and sugar, dried" , 0.99)) %>% 
  mutate(b12= replace(b12, ingredient=="Pork, crispy, cooked with salt and sugar" , 0.64)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef" , 2.0)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, short rib, lean and fat" , 2.56)) %>% 
  mutate(b12= replace(b12, (grepl("Beef, hind", ingredient)) | (grepl("Beef, fore", ingredient)) , 3.33))  %>% 
  mutate(b12= replace(b12, ingredient=="Beef, tenderloin" |   (grepl("Beef, tendon", ingredient)) , 2.6)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, lean" , 1.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, rump, raw, lean" , 2.15)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, highrib, raw, lean and fat" , 1.55)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, fillet tenderloin, raw, lean" , 3.53)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, shoulder, raw, lean and fat" , 3.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, brisket, raw, lean and fat" | 
                        ingredient=="Beef, belly steak, raw, lean and fat", 2.43)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, boneless knuckle or thick flank, raw, lean" , 0.91)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, boneless topside, raw, lean" , 3.78)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, shank, raw, lean" , 3.33)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, lung" , 3.81)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, liver" , 59.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, large intestine" |
                        ingredient=="Beef, penis, soaked in water", 1.99) ) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, brain" , 9.51)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, tongue" , 3.79)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, stewed, lean and fat, canned" , 0.5)) %>% 
  mutate(b12= replace(b12, (grepl("Beef, tripe", ingredient)) , 1.39)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, kidney" , 27.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, heart" , 8.55)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, lean and fat, cooked with salt, etc." |
                        ingredient=="Beef, lean and fat, spiced, cooked with salt, soy sauce, etc." |
                        ingredient=="Beef, lean and fat, spiced, cooked with soy sauce", 2.45)) %>% 
  mutate(b12= replace(b12, (grepl("Beef, dried", ingredient)) | (grepl("Beef floss", ingredient)) |
                         (grepl("Beef, jerky", ingredient)), 1.59)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, shank, cooked with salt, soy sauce, etc." , 3.79)) %>% 
  mutate(b12= replace(b12, ingredient=="Beef, curried, fried" , 0.54)) %>% 
  mutate(b12 = replace(b12, ingredient=="Mutton, highrib / meat of the back of the body, lean and fat, raw", 2.09)) %>% 
  mutate(b12 = replace(b12, ingredient=="Mutton, shoulder, lean and fat, raw", 1.98)) %>% 
  mutate(b12 = replace(b12, ingredient=="Mutton, fore leg, lean, raw" | ingredient=="Mutton, hind leg" |
                       ingredient=="Mutton, fore leg" |  ingredient=="Mutton, hind leg, lean, raw" |
                         ingredient=="Tendon, raw" | ingredient=="Tendon, soaked in water", 2.5)) %>% 
  mutate(b12= replace(b12, foodcategory2=="Ass" , 3.16)) %>%  
    mutate(b12 = replace(b12, ingredient=="Donkey meat, lean" | ingredient=="Donkey penis" |
                          ingredient=="Camel paw" | ingredient=="Camel hoof" |
                           ingredient=="Horse meat", 3)) %>% 
  mutate(b12= replace(b12, ingredient=="Dog meat" , 0.6)) %>%  
  mutate(b12= replace(b12, ingredient=="Venison, clubs deer, raw", 6.32)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, spiced, barbecued with salt, oil, etc." | 
                        ingredient=="Mutton, cooked" | ingredient=="Mutton, boiled, eaten with fingers" | 
                        ingredient=="Mutton, salted and smoked", 2.53)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, slice, raw" | ingredient=="Mutton, lean and fat" |
                        ingredient=="Mutton, *Pale green* sheep", 1.47)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, liver", 59)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, frozen", 1.73)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, lung", 3.93)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, tripe" | ingredient=="Mutton, large intestine", 1.09)) %>% 
mutate(b12= replace(b12, ingredient=="Mutton, scrag", 2.51)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, tenderloin", 1.87)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, flank", 1.31)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, heart", 8.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, kidney", 50.37)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, blood", 1.0)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, dried", 0.99)) %>% 
  mutate(b12= replace(b12, (grepl("Mutton, shashlik", ingredient)), 3.03)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, lean", 2.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Horse meat, stewed", 2.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Horse heart", 8.55)) %>% 
  mutate(b12= replace(b12, ingredient=="Rabbit meat" | ingredient=="hare meat", 7.16)) %>% 
  mutate(b12= replace(b12, ingredient=="Goat meat, frozen", 1.13)) %>% 
  mutate(b12= replace(b12, ingredient=="Goat meat, spiced, cooked with soy sauce", 1.16)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, brain", 9.99)) %>% 
  mutate(b12= replace(b12, ingredient=="Mutton, tongue" | ingredient=="Duck, tongue", 7.2)) %>% 
  mutate(b12= replace(b12, ingredient=="Ice cream, strawberry flavor" | 
           ingredient=="Ice cream,coffee flavor" | ingredient=="Ice cream, butter added" |
           ingredient=="Ice cream, *Jinbaihe*" | ingredient=="Ice cream, cone" | 
             ingredient=="Ice cream brick, with milk, cream, sugar and gelatin" | 
             ingredient=="Ice cream, with milk, cream, sugar and gelatin", 0.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Ice cream bar, *Bailebao*" |
  ingredient=="Ice cream bar, *Hongdouyuan*, adzuki bean powder added" | 
    ingredient=="Ice cream bar" | ingredient=="*Two stick* ice crem bar" | 
    ingredient=="*Baby head* ice crem bar" | ingredient=="*Purple* ice crem bar, chocolate coated" | 
    ingredient=="Popsicle, milk and sugar", 0.05)) %>% 
  mutate(b12= replace(b12, ingredient=="*Sandwich* ice cream", 0.32)) %>% 
  mutate(b12= replace(b12, (grepl("Milk drink, vitamin A, D enriched", ingredient)), 3.25)) %>% 
mutate(b12= replace(b12, ingredient=="*Sandwich* ice cream", 0.39)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, whole" | ingredient=="Chicken, free range, whole" |
                        ingredient=="Chicken, young hen, whole" | 
                        ingredient=="Chicken, broiler, artificially fed, fatty, whole" |
                        ingredient=="Chicken, *Huaqing*, whole" |
                        ingredient=="Black boned chicken, domestic, whole", 0.31)) %>%
  mutate(b12= replace(b12, ingredient=="Chicken, breast", 0.34)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, wing", 0.25)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, leg", 0.56)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, claw", 0.47)) %>% 
  mutate(b12= replace(b12, ingredient=="Cake, with egg and baking powder, baked", 0.25)) %>% 
  mutate(b12= replace(b12, ingredient=="Butter residue, *milk solids*", 0.01)) %>% 
  mutate(b12= replace(b12, (grepl("*Mooncake*", ingredient)), 0.08)) %>%  
  mutate(b12= replace(b12, ingredient=="*Mooncake*, filled with salted egg yolk and lotus seed paste", 0.34)) %>% 
  mutate(b12= replace(b12, ingredient=="Butter residue, *milk solids*", 0.01)) %>% 
  mutate(b12= replace(b12, ingredient=="Fried dough twisted, crispy" | ingredient=="Honey dough twist", 0.1)) %>% 
  mutate(b12= replace(b12, ingredient=="*Beijing* white duck, blood" | 
                        ingredient=="Duck, blood, drake" | ingredient=="Chicken, blood", 0.1)) %>% 
  mutate(b12= replace(b12, ingredient=="Quail egg, whole", 1.58)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk, German cow" | ingredient=="Milk, pasteurized" | 
                        ingredient=="Milk, American cow" | ingredient=="Milk, enriched with vitamins A and D" |
                        (grepl("Milk, whole", ingredient)), 0.54)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, egg" | ingredient=="Egg, Tibetan hen, whole, raw", 0.88)) %>% 
  mutate(b12= replace(b12, ingredient=="*Fruit flavor* milk, artificially fruit flavored, pasteurized" | 
                        ingredient=="*Fruit flavor* milk, artificially fruit flavored, pasteurized", 0.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt drink, sweetened" | 
                        ingredient=="Yogurt drink, with actobacillus, sweetened", 0.39)) %>% 
  mutate(b12= replace(b12, ingredient=="Cocoa malted beverage, in powder, milk and cocoa added" |
                        ingredient=="Malted milk crystal, mixture of cocoa, malt and milk", 0.35)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk candy" | ingredient=="Toffee", 0.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, roasted, seasoned, whole" |
                        ingredient=="Chicken, roasted, whole", 0.27)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, braised, seasoned, whole" | 
                        ingredient=="Chicken, spring, braised with salt, beer, etc., whole" |
                        ingredient=="Chicken, stewed with soy sauce and sugar, whole", 0.45)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, mixed with starch and other spices. frozen in pieces, raw", 0.36)) %>% 
  mutate(b12= replace(b12,  (grepl("Duck (Peking duck), roasted", ingredient)) |
                        ingredient=="Duck (Peking duck), roasted, whole, LAOTANG" |
                        ingredient=="Duck (Peking duck), roasted, whole, QUANJUDE", 0.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate flavored soy milk, bottled", 0.7)) %>% 
  mutate(b12= replace(b12, ingredient=="Egg, duck, whole, salt preserved, boiled" | 
                        ingredient=="Duck, egg, whole, salt preserved", 4.03)) %>% 
  mutate(b12= replace(b12, ingredient=="Egg, goose, whole, boiled", 3.81)) %>% 
  mutate(b12= replace(b12, ingredient=="Egg, hen, fried, shell removed", 0.97)) %>% 
  mutate(b12= replace(b12, ingredient=="Egg, sea duck, whole, raw", 5.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Egg, hen, whole, boiled" | 
                        ingredient=="Egg, hen, poached, shell removed", 0.71)) %>% 
  mutate(b12= replace(b12, ingredient=="Cake, with fruit", 0.01)) %>% 
  mutate(b12= replace(b12, ingredient=="Frog" | ingredient=="Snake" | ingredient=="Water snake", 0.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Trionyx turtle" , 1)) %>% 
  mutate(b12= replace(b12, ingredient=="Silkworm chrysalis" , 1.3)) %>% 
  mutate(b12= replace(b12, ingredient=="*Pine flower* chicken, egg, preserved in lime" , 0.529)) %>% 
  mutate(b12= replace(b12, ingredient=="*Pine flower* duck egg, whole, preserved in powdered line" , 2.14)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, egg powder, whole" , 2.96)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, egg yolk" | ingredient=="Chicken, egg yolk, black boned chicken", 1.95)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, egg, whole, free range chicken" |
                        ingredient=="Chicken, egg, whole, white shell" | 
                        ingredient=="Chicken, egg, brown shell, whole" | 
                        ingredient=="Egg, hen, hatched, whole, raw", 0.89)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, egg white" , 0.09)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, egg whole" , 5.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, egg white" , .546)) %>% #no estimate for duck egg white so used ratio from chicken egg white to whole egg
  mutate(b12= replace(b12, ingredient=="Goose egg, whole" , 5.1)) %>% 
  mutate(b12= replace(b12, ingredient=="*Chicken-leg* crispy cookie, with lard and sugar, baked" |
                        ingredient=="*Phoenix tail* cookie, with lard and sugar, baked" |
                        ingredient=="Cookie, with brown and white sugar and honey" |
                        ingredient=="Sesame seeds cookie, baked" |
                        ingredient=="Sesame walnut cookies" |
                        ingredient=="Walnut cookie, with walnuts and vegetable oil, baked" |
                        ingredient=="Black sesame seed cookie, baked", 0.03)) %>% 
  mutate(b12= replace(b12, ingredient=="*Chicken-leg* crispy cookie, with lard and sugar, baked" , 0.03)) %>% 
  mutate(b12= replace(b12, ingredient=="*Firm shelled cake*, with lard and sugar, baked" |
                        ingredient=="Cake, with egg and baking powder, steamed" | 
                        ingredient=="Cake, with egg-whites and baking powder, baked" | 
                        ingredient=="Cake, with egg, butter and baking powder, baked" |
                        ingredient=="Fluffy cake with egg fork and sugar, baked" |
                        ingredient=="*Royal style* cake, with egg and baking powder, baked" |
                        ingredient=="*New year* cake, glutinous rice flour, steamed" |
                        ingredient=="*Scholar’s cake*, fluffy cake, with jujube paste, baked" |
                        ingredient=="Fried cake, with red bean paste and sugar, deep fried" |
                        ingredient=="Mungbean cake, with sugar and sesame oil, baked" |
                        ingredient=="Egg crisp, sachima", 0.17)) %>% 
  mutate(b12= replace(b12, ingredient=="Crispy sandwich" |
                        ingredient=="Crispy cookie, filled with sugar, Osmanthus flower, baked" |
                        ingredient=="*Wafer sandwich*, poris cocos and honey in between", 0.09)) %>% 
  mutate(b12= replace(b12, ingredient=="Steamed bun, filled with ground pork and broth" |
                        ingredient=="Dumpling, wheat flour, filled with pork, Chinese chive, oil and salt added, frozen, raw" |
                        ingredient=="Dumpling, wheat flour, filled with pork, celery, oil and salt added, frozen, raw" |
                        ingredient=="Dumpling, wheat flour, filled with pork, Chinese cabbage, oil and salt added, frozen, raw" |
                        ingredient=="Dumpling, wheat flour, filled with shrimp, wood ear fungus, egg or pork, oil and salt added, frozen, raw" |
                        ingredient=="Dumpling, wheat flour, filled with pork, fennel, oil and salt added, frozen, raw" , 0.22)) %>% 
  mutate(b12= replace(b12, ingredient=="Dumpling, glutinous-rice flour case, filled with black sesame, peanut and sugar, etc., frozen, raw" |
                        ingredient=="Dumpling, wheat flour, filled with vegetables, wood ear fungus, egg. oil and salt added, frozen, raw" |
                        ingredient=="*Spring roll*, vegetarian, wheat flour skin, filled with cabbage, carrot and mushroom, etc., frozen, raw", 0.03)) %>% 
  mutate(b12= replace(b12, ingredient=="*Spring roll*, wrapped with pork & vegetables, deep fried" |
                        ingredient=="Egg roll, crispy" |
                        ingredient=="*Three-delicious-taste roll*, with egg, pork, and glutinous rice", 0.14)) %>% 
  mutate(b12= replace(b12, ingredient=="Rice flour doughnut, deep fried" , 0.08)) %>% 
  mutate(b12= replace(b12, ingredient=="*Starch sausage*, with garlic, fried" , 0.98)) %>% 
  mutate(b12= replace(b12, ingredient=="*Animal* cracker, for children" , 0.05)) %>% 
  mutate(b12= replace(b12, ingredient=="*Calcium-milk* cracker, fortified with calcium and milk" , 0.08)) %>% 
  mutate(b12= replace(b12, ingredient=="*Horn* bread, French style, hollow, filled with sweet cream" , 0.12)) %>% 
  mutate(b12= replace(b12, ingredient=="*Morning tea* biscuit, wheat flour, oil, sugar, milk powder, salt added" |
                        ingredient=="Biscuit" | ingredient=="*Soda* biscuit" | ingredient=="Milk biscuit" |
                       ingredient=="Biscuit, salted",  0.19) ) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken and shrimp paste, for instant rice noodle with seafood and chicken soup flavor" , 0.02)) %>% 
  mutate(b12= replace(b12, ingredient=="Butter bead, *Lesimei*" | ingredient=="Butter bread", 0.27)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken wing, fried, hot, KENTUCKY" , 0.35)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate pie" , 0.12)) %>% 
  mutate(b12= replace(b12, ingredient=="Coconut bread, ring, sprinkled with shredded coconut meat" , 0.06)) %>% 
  mutate(b12= replace(b12, ingredient=="Fruit bread, with preserved fruits" , 0.15)) %>% 
  mutate(b12= replace(b12, ingredient=="Hamburger, chicken, KENTUCKY" , 0.87)) %>% 
  mutate(b12= replace(b12, ingredient=="Instant stir-fried wheat noodle, with baked beef flavor, condiments, vegetable and beef paste added" |
                        ingredient=="Instant wheat noodle, with cod fish flavor, condiments, cod paste, vegetable and oil", 0.25)) %>% 
  mutate(b12= replace(b12, ingredient=="Roll pie, French style" , 0.11)) %>% 
  mutate(b12= replace(b12, ingredient=="Salmon grain, mixed with wheat flour, soybean powder and sugar" , 2.22)) %>% 
  mutate(b12= replace(b12, ingredient=="Sandwich, with egg and cheese" , 0.51)) %>% 
  mutate(b12= replace(b12, ingredient=="Steamed bun, filled with shrimp, wood ear fungus, egg or pork, oil and salt added, frozen, raw" |
                        ingredient=="Steamed bun, filled with pork, oil and salt added, frozen, raw" , 0.17 )) %>% 
  mutate(b12= replace(b12, ingredient=="Soda cracker" , 0.09)) %>% 
  mutate(b12= replace(b12, ingredient=="Caviar" , 20)) %>% 
  mutate(b12= replace(b12, ingredient=="Cod, spread with butter, baked" , 2.18)) %>% 
  mutate(b12= replace(b12, ingredient=="Cuttlefish meat ball, mashed, frozen, raw" | 
                        ingredient=="Fish ball, mashed with starch, salt and sugar, frozen, raw" | 
                        ingredient=="Pork and shrimp meat ball, mashed, cooked then frozen", 0.67)) %>% 
  mutate(b12= replace(b12, ingredient=="Cuttlefish roe, marine" , 10)) %>% 
  mutate(b12= replace(b12, ingredient=="Cuttlefish, marine" | ingredient=="Japanese cuttlefish, marine" , 3)) %>% 
  mutate(b12= replace(b12, ingredient=="Japanese sea cucumber, fresh, marine" |
                        ingredient=="Japanese sea cucumber, marine, soaked in water", 3.68)) %>% 
  mutate(b12= replace(b12, ingredient=="Jellyfish body, marine" | ingredient=="Jellyfish head, marine" , 0.28)) %>% 
  mutate(b12= replace(b12, ingredient=="Japanese lion fish, drum fish, marine", 1.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Hairtail or belt fish, marine"  , 2.34)) %>% 
  mutate(b12= replace(b12, ingredient=="Lizardfish, marine"  , 4.68)) %>% 
  mutate(b12= replace(b12, ingredient=="Mixed fish, sliced, cooked, dried", 10)) %>% 
  mutate(b12= replace(b12, ingredient=="Octopus", 20)) %>% 
  mutate(b12= replace(b12, ingredient=="Japanese cuttlefish, marine, dried", 4.67)) %>% 
  mutate(b12= replace(b12, ingredient=="Mandarin fish, freshwater" | ingredient=="Spotted drum, white, marine", 2)) %>% 
  mutate(b12= replace(b12, ingredient=="Squid, marine, dried", 4.67)) %>% 
  mutate(b12= replace(b12, ingredient=="Squid, marine, soaked in water", 1.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Swordfish, fried, canned", 1.82)) %>% 
  mutate(b12= replace(b12, ingredient=="White fish fillet, mixed with bread crumbs, starch and salt, frozen, raw", 1.71)) %>% 
  mutate(b12= replace(b12, ingredient=="Yellow spotted maigre, marine", 2.5)) %>% 
  mutate(b12= replace(b12, ingredient=="Infant formula milk powder", 0.2)) %>% 
  mutate(b12= replace(b12, ingredient=="Infant formula milk powder, simulated breast milk", 0.2)) %>% 
  mutate(b12= replace(b12, (grepl("Beer", ingredient)) | (grepl("beer", ingredient)) , 0.02)) %>% 
  mutate(b12= replace(b12, ingredient=="Pacific cod, marine", 1.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Formula milk powder, KOCCI, for middle-aged and elderly people" |
                        ingredient=="Formula milk powder, SANLU, for middle-aged and elderly people", 1.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Pacific cod, marine", 1.98)) %>% 
  mutate(b12= replace(b12, ingredient=="Goat milk", 0.07)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk, partly skimmed", 0.55)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk, recombined, enriched with zinc and calcium, PARMALAT", 1.10)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk, strawberry flavored, whole, sweetened", .5)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk, chocolate flavored, whole, sweetened, SANYUAN", .47)) %>% 
  mutate(b12= replace(b12, (grepl("Whole milk powder", ingredient)) | 
                        ingredient=="Whole Milk powder," | 
                        (grepl("Whole Milk powder", ingredient)), 1.8)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt", .56)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt, high protein", .75)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt, medium fat", .56)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt, skimmed", .61)) %>% 
  mutate(b12= replace(b12, ingredient=="Yogurt, whole milk, with sugar" | 
                        ingredient=="*Fruit* yogurt, whole milk, with fruits", .33)) %>% 
  mutate(b12= replace(b12, ingredient=="*Beijing* white duck, roasted, whole", .3)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken soup broth, cooked in clay pot", .02)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken soup, meat, cooked in clay pot", 1)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, *Kentucky*, fried", 0.38)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, braised, whole", 0.45)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, gizzard" | 
                        ingredient=="Goose, gizzard", 1.21)) %>% 
  mutate(b12= replace(b12, ingredient=="Chicken, heart", 7.29)) %>% 
  mutate(b12= replace(b12, (grepl("Chicken, liver", ingredient)) , 16.58)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, boiled in salted water, whole" |
                        ingredient=="Duck, cooked with soy sauce and spices, whole" |
                        ingredient=="Duck, stewed in soy sauce, canned", 0.3)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, breast", 0.76)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, gizzard", 3.61)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, intestine" | ingredient=="Duck, pancreas", 1)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, liver", 54)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, web", 0.47)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, whole" | ingredient=="Duck, drake, whole", 0.25)) %>% 
  mutate(b12= replace(b12, ingredient=="Duck, wing", 0.25)) %>% 
  mutate(b12= replace(b12, ingredient=="Goose, liver", 54)) %>% 
  mutate(b12= replace(b12, ingredient=="Goose, whole", 0.34)) %>% 
  mutate(b12= replace(b12, ingredient=="Pigeon, whole", 0.4)) %>% 
  mutate(b12= replace(b12, ingredient=="Quail, whole", 0.43)) %>% 
  mutate(b12= replace(b12, ingredient=="Turkey, leg", 0.39)) %>% 
  mutate(b12= replace(b12, ingredient=="Turkey, liver", 19.73)) %>% 
  mutate(b12= replace(b12, ingredient=="Peanut brittle", 0.01)) %>% 
  mutate(b12= replace(b12, ingredient=="Milk chocolate" | ingredient=="Chocolate, filled with air" , 0.8)) %>% 
  mutate(b12= replace(b12, ingredient=="Peanut brittle", 0.01)) %>% 
  mutate(b12= replace(b12, ingredient=="Chocolate, with nuts", 0.33))
 

# TO view the remaining observations with missing b12 entries
  china_b12_test <-  china_b12_clean %>% filter(foodcategory1 !="Vegetables and vegetables products" & 
                                                       foodcategory1 != "Dried legumes and legumes products" &
                                                       foodcategory1!="Tubers, starches and products" &
                                                       foodcategory1 != "Cereals and cereals products" &
                                                       foodcategory1 != "Fungi and algae" &
                                                       foodcategory1 != "Fruit and fruit products" & 
                                                  foodcategory1 != "Fruits and fruit products" &
                                                  foodcategory1 != "Nuts and seeds" &
                                                  foodcategory2!="Animal fat" & foodcategory1 !="Condiments" &
                                                  foodcategory1 !="Beverages"  &
                                                  foodcategory1 !="Edible medicinal herbs and others" &
                                                  foodcategory1 !="Ethnic food and cakes" & 
                                                  foodcategory1 != "Ethnic foods and cakes" &
                                                  foodcategory1 !="Fats and oil" &
                                                  foodcategory1 != "Fats and oils" &
                                                  foodcategory1 != "Liquor and alcoholic beverages" &
                                                  foodcategory1 != "Fast foods") %>% 
  group_by(foodcategory1, ingredient) %>% slice(1) %>% filter(b12 ==0 | is.na(b12) )




# write a function to replace the values
# My function doesnt work :( and I don't know why

# repb12 <- function(food, x) {
#   china_b12_clean %>% 
#     mutate(b12= replace(b12, ingredient==food , x))
# }

china_b12_test <- china_b12_clean %>% filter(is.na(b12))


# Merge in identifiers

china_spade <- china %>%
  left_join(china_age, by=c("id")) %>% 
  left_join(china_sex, by=c("id")) %>% 
  rename(mday=day, red_meat=f3redmeat, vita=f3d_vit_a, calc=f3d_ca, omega_3=f3depadha, zinc=f3d_zn, iron=f3d_fe) %>% 
  group_by(id) %>% 
  mutate(id = cur_group_id()) %>%
  distinct() %>% ungroup()


summary(china_spade)

#No missing ages

#Replace any cases where the ages are different for the same individual

ids_data <- unique(china_spade$id)
for (idid in ids_data){
  data.id <- china_spade[china_spade$id == idid, ]
  if(nrow(data.id) > 1){
    china_spade[china_spade$id == idid,"age"] <- 
      min(china_spade[china_spade$id == idid,"age"])
  }
}

save(china_spade, file=here("data", "processed", "china"), replace)   




# to clean through the nutrition data

# The first survey is the household inventory: we only want individual level
# china_nut1 <- read_sas(here("data", "raw", "China", "nutr1_00.sas7bdat")) %>% 
  # clean_names() %>% filter(wave==2009)

china_nut2 <- read_sas(here("data", "raw", "China", "nutr2_00.sas7bdat")) %>% 
  clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)

# food codes are in this file and file 2
china_nut3 <- read_sas(here("data", "raw", "China", "nutr3_00.sas7bdat")) %>% 
  clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)
# Open identifying data
# china_id <- read_sas("/Users/Simone/Downloads/Master_ID_201908/rst_12.sas7bdat") %>% 
  # clean_names() %>% filter(wave==2009) %>% rename(id=i_dind)
