*created by Simone Passarelli 11_24_2020

*Philippines

import delimited "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Philippines/consumption_user.csv", clear

keep code_ingredient ingredient foodex2_ingr_code foodex2_ingr_descr food_amount_unproc food_amount_reported energy protein carboh fat calc iron zinc vitc thia ribo niac vitb12 vita seafood_n3

order zinc vitb12 seafood_n3, after(vita)

duplicates drop ingredient foodex2_ingr_descr, force

sort ingredient

*To calculate the amount per 100 grams

foreach var of varlist energy protein carboh fat calc iron vitc thia ribo niac vita {
	gen `var'_100 = (100*`var')/food_amount_unproc
	label var `var'_100 "`var' per 100 grams"
	drop `var'
}

preserve
drop if strpos(ingredient, "FISH") | strpos(ingredient, "MOLLUSKS")

export excel "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Philippines/food_list_P.xls", replace  firstrow(var)

restore 

keep if strpos(ingredient, "FISH") | strpos(ingredient, "MOLLUSKS")

export excel "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Philippines/fish_list_P.xls", replace firstrow(var)

*Lao

import delimited "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Lao/consumption_user.csv", clear


keep code_ingredient ingredient foodex2_ingr_code foodex2_ingr_descr food_amount_unproc food_amount_reported energy protein carboh fat calc iron zinc vitc thia ribo niac vitb12 vita seafood_n3

order zinc vitb12 seafood_n3, after(vita)

duplicates drop ingredient foodex2_ingr_descr, force

sort ingredient

*To calculate the amount per 100 grams

foreach var of varlist energy protein carboh fat calc iron vitc thia ribo niac vita {
	gen `var'_100 = (100*`var')/food_amount_reported
	label var `var'_100 "`var' per 100 grams"
	drop `var'
}


export excel "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Lao/food_list_Lao.xls", replace firstrow(var)

keep if strpos(ingredient, "fish") | strpos(ingredient, "fish") | strpos(ingredient, "mackerel") ///
| strpos(ingredient, "squid") | strpos(ingredient, "Shrimp") | strpos(ingredient, "fish") ///
| strpos(ingredient, "featherback") | strpos(ingredient, "tilapia") ///
| strpos(ingredient, "silver barb") | strpos(ingredient, "perch") ///
| strpos(ingredient, "Fish") |  strpos(ingredient, "Snail") | strpos(ingredient, "Squid") | strpos(ingredient, "Eel") 
| strpos(ingredient, "carp") | strpos(ingredient, "mullet") 


export excel "/Users/Simone/Dropbox/Github/subnational_distributions/data/raw/Lao/fish_list_Lao.xls", replace firstrow(var)
