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
  b12a=dr1ivb12, b12b=dr1ib12a, zinc=dr1izinc, epa=dr1ip205, dha=dr1ip226, iron=dr1iiron, weight1=wtdrd1, weight2=wtdr2d,
energy=dr1ikcal, protein=dr1iprot, carb=dr1icarb, sugar=dr1isugr, fiber=dr1ifibe, fat=dr1itfat,
satfat=dr1isfat, mufa=dr1imfat, pufa=dr1ipfat, cholest=dr1ichol, vitef=dr1iatoc, viteadd=dr1iatoa,
retinol=dr1iret, alphacarot=dr1iacar, betacarot=dr1ibcar, betacrypt=dr1icryp, 
lyco=dr1ilyco, lutzea=dr1ilz, thia=dr1ivb1, ribo=dr1ivb2, niac=dr1iniac, vitb6=dr1ivb6,
fola=dr1ifola, chol=dr1ichl, vitc=dr1ivc, vitd=dr1ivc, vitk=dr1ivk, mg=dr1imagn,
phos=dr1iphos, cu=dr1icopp, na=dr1isodi, pota=dr1ipota, se=dr1isele, caff=dr1icaff,
theo=dr1itheo, alcohol=dr1ialco) %>% 
  select(!(dr1iline:code)) %>% select(!c(dr1imois:dr1ip204, amount, dr1ifa, dr1ifdfe, dr1ivd, dr1iff, dr1ip225 ))
  

#Clean day 2 data and sync names. Keep necessary vars only
nhanes_day2 <- read_xpt(here("data", "raw", "United States", "DR2IFF_J.XPT")) %>% 
  clean_names() %>% mutate(mday=2) %>% 
  rename(id=seqn, code=dr2ifdcd, amount=dr2igrms, vita=dr2ivara, calc=dr2icalc, 
         b12a=dr2ivb12, b12b=dr2ib12a, zinc=dr2izinc, epa=dr2ip205, dha=dr2ip226, iron=dr2iiron, weight1=wtdrd1, weight2=wtdr2d,
         energy=dr2ikcal, protein=dr2iprot, carb=dr2icarb, sugar=dr2isugr, fiber=dr2ifibe, fat=dr2itfat,
         satfat=dr2isfat, mufa=dr2imfat, pufa=dr2ipfat, cholest=dr2ichol, vitef=dr2iatoc, viteadd=dr2iatoa,
         retinol=dr2iret, alphacarot=dr2iacar, betacarot=dr2ibcar, betacrypt=dr2icryp, 
         lyco=dr2ilyco, lutzea=dr2ilz, thia=dr2ivb1, ribo=dr2ivb2, niac=dr2iniac, vitb6=dr2ivb6,
         fola=dr2ifola, chol=dr2ichl, vitc=dr2ivc, vitd=dr2ivc, vitk=dr2ivk, mg=dr2imagn,
         phos=dr2iphos, cu=dr2icopp, na=dr2isodi, pota=dr2ipota, se=dr2isele, caff=dr2icaff,
         theo=dr2itheo, alcohol=dr2ialco) %>% 
  select(!(dr2iline:code)) %>% select(!c(dr2imois:dr2ip204, amount, dr2ifa:dr2ifdfe, dr2ivd, dr2ip225 ))

#append day 1 and day 2 data
nhanes <- rbind(nhanes_day1, nhanes_day2)


#Load demographic data for age and sex variables
usa_merge <- read_xpt(here("data", "raw", "United States", "DEMO_J.XPT")) %>% 
         clean_names() %>% 
  rename( age=ridageyr, sex=riagendr, id=seqn) %>%
  dplyr::select( age, sex, id) %>%
  distinct() 

# None, they were all classified


usa_nut <- nhanes %>% 
  group_by(id, mday) %>%
  summarize(vitb12 = sum(b12a, b12b),
            iron = sum(iron),
            zinc = sum(zinc),
            vita = sum(vita),
            calc = sum(calc),
            omega_3 = sum(epa, dha),
            vite=sum(vitef, viteadd),
            weight1 = weight1,
            weight2=weight2,
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

  
  save(usa_spade, file=here("data", "processed","Subnational distributions", "usa"), replace)   
  
  