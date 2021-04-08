# NHANES data cleaning for SPADE
# Created by Simone Passarelli on 3/24/21
# Testing the distributions with older data: NHANES 2017-2018

library(tidyverse)
library(haven)
library(here)
library(janitor)
# Clean day 1 data and sync names. Keep necessary vars only
nhanes_day1 <- read_xpt(here("data", "raw", "United States", "older", "2017_2018" , "DR1TOT_J.XPT")) %>% 
  clean_names() %>% mutate(mday=1) %>% 
  rename(id=seqn, vita=dr1tvara, calc=dr1tcalc, 
         b12a=dr1tvb12, b12b=dr1tb12a, zinc=dr1tzinc, epa=dr1tp205, dha=dr1tp226, iron=dr1tiron, weight1=wtdrd1, weight2=wtdr2d,
         energy=dr1tkcal, protein=dr1tprot, carb=dr1tcarb, sugar=dr1tsugr, fiber=dr1tfibe, fat=dr1ttfat,
         satfat=dr1tsfat, mufa=dr1tmfat, pufa=dr1tpfat, cholest=dr1tchol, vitef=dr1tatoc, viteadd=dr1tatoa,
         retinol=dr1tret, alphacarot=dr1tacar, betacarot=dr1tbcar, betacrypt=dr1tcryp, 
         lyco=dr1tlyco, lutzea=dr1tlz, thia=dr1tvb1, ribo=dr1tvb2, niac=dr1tniac, vitb6=dr1tvb6,
         fola=dr1tfola, chol=dr1tchl, vitc=dr1tvc, vitd=dr1tvc, vitk=dr1tvk, mg=dr1tmagn,
         phos=dr1tphos, cu=dr1tcopp, na=dr1tsodi, pota=dr1tpota, se=dr1tsele, caff=dr1tcaff,
         theo=dr1ttheo, alcohol=dr1talco) %>% select(!c(dr1tmois:dr1tp204 )) %>% select(!(weight2:dr1tnumf)) %>% 
  select(!(dr1_300:drd370v)) %>% 
  select(!c("dr1tvd", "dr1tfa",  "dr1tff", "dr1tfdfe" , "dr1tp225"))


names(nhanes_day1)

#Clean day 2 data and sync names. Keep necessary vars only
nhanes_day2 <- read_xpt(here("data", "raw", "United States", "older", "2017_2018" , "DR2TOT_J.XPT")) %>% 
  clean_names() %>% mutate(mday=2) %>% 
  rename(id=seqn, vita=dr2tvara, calc=dr2tcalc, 
         b12a=dr2tvb12, b12b=dr2tb12a, zinc=dr2tzinc, epa=dr2tp205, dha=dr2tp226, iron=dr2tiron, weight1=wtdrd1, weight2=wtdr2d,
         energy=dr2tkcal, protein=dr2tprot, carb=dr2tcarb, sugar=dr2tsugr, fiber=dr2tfibe, fat=dr2ttfat,
         satfat=dr2tsfat, mufa=dr2tmfat, pufa=dr2tpfat, cholest=dr2tchol, vitef=dr2tatoc, viteadd=dr2tatoa,
         retinol=dr2tret, alphacarot=dr2tacar, betacarot=dr2tbcar, betacrypt=dr2tcryp, 
         lyco=dr2tlyco, lutzea=dr2tlz, thia=dr2tvb1, ribo=dr2tvb2, niac=dr2tniac, vitb6=dr2tvb6,
         fola=dr2tfola, chol=dr2tchl, vitc=dr2tvc, vitd=dr2tvc, vitk=dr2tvk, mg=dr2tmagn,
         phos=dr2tphos, cu=dr2tcopp, na=dr2tsodi, pota=dr2tpota, se=dr2tsele, caff=dr2tcaff,
         theo=dr2ttheo, alcohol=dr2talco) %>% select(!c(dr2tmois:dr2tp204 )) %>% select(!(weight2:dr2tnumf)) %>% 
  select(!c("dr2tvd", "dr2tfa", "dr2sty", "dr2sky", "dr2tff", "dr2tfdfe","dr2_300",
            "dr2_320z", "dr2_330z", "dr2bwatz", "dr2tws", "dr2tp225" ))

names(nhanes_day1)

names(nhanes_day2)

#append day 1 and day 2 data
nhanes <- rbind(nhanes_day1, nhanes_day2)


#Load demographic data for age and sex variables
usa_merge <- read_xpt(here("data", "raw", "United States", "older", "2017_2018", "DEMO_J.XPT")) %>% 
  clean_names() %>% 
  rename( age=ridageyr, sex=riagendr, id=seqn) %>%
  dplyr::select( age, sex, id) %>%
  distinct() 

# None, they were all classified


usa_nut <- nhanes %>% 
  group_by(id, mday, weight1) %>%
  summarize(vitb12 = sum(b12a, b12b),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            omega_3 = sum(epa, dha),
            vite=sum(vitef, viteadd),
            energy=sum(energy),
            protein=sum(protein),
            carb=sum(carb),
            sugar=sum(sugar),
            fiber=sum(fiber),
            fat=sum(fat),
            satfat=sum(satfat),
            mufa=sum(mufa),
            pufa=sum(pufa),
            retinol=sum(retinol),
            alphacarot=sum(alphacarot),
            betacarot=sum(betacarot),
            betacrypt=sum(betacrypt),
            lyco=sum(lyco),
            lutzea=sum(lutzea),
            thia=sum(thia),
            ribo=sum(ribo),
            niac=sum(niac),
            vitb6=sum(vitb6),
            fola=sum(fola),
            chol=sum(chol),
            vitd=sum(vitd),
            vitk=sum(vitk),
            phos=sum(phos),
            mg=sum(mg),
            cu=sum(cu),
            na=sum(na),
            pota=sum(pota),
            se=sum(se),
            caff=sum(caff),
            theo=sum(theo),
            alcohol=sum(alcohol)) %>% distinct() 



# Rename and format variables for spade
usa_spade <- usa_nut %>% 
  left_join(usa_merge, by=c("id")) %>%
  group_by(id, mday) %>%
  distinct()   %>%
  ungroup() %>% 
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


save(usa_spade, file=here("data", "processed","Subnational distributions", "usa_2017_2018"), replace)   

