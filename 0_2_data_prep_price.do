********************************************************************************
*				program: prepare price variables							   *			
*				written by: Ziwei Ye										   *
* 				date: 01/17/2021											   *
*				update:	03/05/2021								   	   	       *
********************************************************************************

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"

global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"
global ds "$root\source"

cd "$do"

global herb "glyp comp"
global admin "scrd statefips"

*------------------------choose basket product---------------------------------*
*-----glyphosate-----*
use "$dd\consp_ai_98.dta", clear

keep if year>2009
keep if activeingredient=="GLYPHOSATE"

merge m:1 year using "$dt\def_10.dta",nogenerate
replace expenditures=expenditures/def_factor

collapse (sum) productamtused expenditures (mean) lbsai, by(product)

rename lbsai lbsai_flag

joinby product using "$dt\ai.dta" //merge with active ingredient data
drop if pesttype=="Growth Regulator"
gen flag1=1 if lbsai_flag!=lbsai
bysort product: egen flag2=max(flag1)
drop if activeingredient!="GLYPHOSATE"

drop if flag2==1
egen amtsum=total(productamtused)
egen expsum=total(expenditures)
gen amtperc=productamtused/amtsum
gen expperc=expenditures/expsum
gsort -expperc


//select the basket products who account for more than 1% and not pre-mixed.
replace expperc=round(expperc, 0.001) 
keep if expperc>=0.05
gen indexpr=_n
keep product indexpr
save "$dt\indexpr_glyp.dta", replace


*-----the composite-----*
*individual chemical
use "$dd\consp_ai_98.dta", clear
keep if year>2009
gen ai=productamtused*lbsai

merge m:1 year using "$dt\def_10.dta",nogenerate
replace expenditures=expenditures/def_factor


local chem "ATRAZINE METOLACHLOR-S ACETOCHLOR"
foreach i of local chem{
	preserve
	keep if activeingredient=="`i'"
	
	collapse (sum) ai productamtused expenditures (mean) lbsai, by(product)
	rename lbsai lbsai_flag
	joinby product using "$dt\ai.dta" //merge with active ingredient data
	gen flag1=1 if activeingredient!="ATRAZINE"&activeingredient!="METOLACHLOR-S"&activeingredient!="ACETOCHLOR"
	bysort product: egen flag2=max(flag1)
	drop if activeingredient!="`i'"
	gsort -ai //descending order

	egen aisum=total(ai)
	egen amtsum=total(productamtused)
	gen aiperc=ai/aisum
	gen amtperc=productamtused/amtsum
	gen qprice_na=expenditures/productamtused
	gen aprice_na=expenditures/ai
	
	replace flag2=0 if flag2!=1
	ttest qprice_na, by(flag2)
	
	keep product productamtused expenditures flag2
	replace expenditures=round(expenditures,1)
	save "$dt\mix_`i'.dta", replace
	restore
}

*composite herbicide product
use "$dt\mix_ATRAZINE.dta", clear
rename flag2 newflag1
merge 1:1 product productamtused expenditures using "$dt\mix_METOLACHLOR-S.dta",
drop _merge 
rename flag2 newflag2
merge 1:1 product productamtused expenditures using "$dt\mix_ACETOCHLOR.dta",
drop _merge 
rename flag2 newflag3
egen flag2=rowmax(newflag1 newflag2 newflag3)

drop if flag==1
gen Atrazine=1 if newflag1==0
gen MetolachlorS=1 if newflag2==0
gen Acetochlor=1 if newflag3==0
 
egen amtsum=total(productamtused)
gen amtperc=productamtused/amtsum
egen expsum=total(expenditures)
gen expperc=expenditures/expsum
gen qprice_comp=expenditures/productamtused


//store the product names if it is not pre-mixed and accounts for more than 5% expenditure among the non mixed products 
replace expperc=round(expperc, 0.001) 
keep if expperc>0.05
gen indexpr=_n
keep product indexpr
save "$dt\indexpr_comp.dta", replace


*----------------------------price index: prepare----------------------------*
***total area, for approximating quantities
use "$dd\consp_ai_98.dta", clear
keep if year>2009
duplicates drop year idnum seedtrait tillagetype seedcompany sequence basearea,force //the set of variables that identify unique plots
foreach a of global admin{
	preserve
	collapse (sum) basearea, by(year `a')
	save "$dt\area_`a'.dta", replace
	restore
}

collapse (sum) basearea, by(year)
save "$dt\area_na.dta", replace


*-------price and quantity of basket products-------*

***CRD/state level amount-weighted average price, for each basket product defined in indexpr
		
foreach h of global herb{
	foreach a of global admin{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep product ai productamtused expenditures activeingredient `a' year

		merge m:1 product using "$dt\indexpr_`h'.dta", nogenerate keep(matched) //keep only the observations of basket products from indexpr_* file
		collapse (sum) expenditure productamtused, by(year `a' indexpr)
		merge m:1 year `a' using "$dt\area_`a'.dta", keep(matched) nogenerate
		merge m:1 year using "$dt\def_10.dta",nogenerate

		gen qprice_`a'=(expenditure/productamtused)/def_factor
		gen rate_`a'=productamtused/basearea
		rename expenditure exp
		rename productamtused amt
		drop basearea
		reshape wide exp amt qprice_`a' rate_`a', i(year `a') j(indexpr)
		
		preserve
		drop qprice* rate*
		save "$dt\exp_amt_`h'_`a'_indexpr.dta", replace
		restore
		preserve
		drop exp* amt*
		save "$dt\qprice_rate_`h'_`a'_indexpr.dta", replace
		restore
	}
}
		

	
***national level amount-weighted average price, for each basket product defined in indexpr
foreach h of global herb{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep product ai productamtused expenditures activeingredient year

		merge m:1 product using "$dt\indexpr_`h'.dta", nogenerate keep(matched) //keep only the observations of basket products from indexpr_* file

		collapse (sum) expenditure productamtused, by(year indexpr)
		merge m:1 year using "$dt\area_na.dta", keep(matched) nogenerate
		merge m:1 year using "$dt\def_10.dta",nogenerate

		gen qprice_na=(expenditure/productamtused)/def_factor
		gen rate_na=productamtused/basearea

		rename expenditure exp
		rename productamtused amt
		drop basearea
		reshape wide exp amt qprice_na rate_na, i(year) j(indexpr)

		preserve
		drop qprice* rate*
		save "$dt\exp_amt_`h'_na_indexpr.dta", replace
		restore
		preserve
		drop exp* amt* 
		save "$dt\qprice_rate_`h'_na_indexpr.dta", replace
		restore
}

***match with adjacent states and calculate average price for each product
foreach h of global herb{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep year statefips
		duplicates drop year statefips, force //complete set of county-year in our sample

		joinby statefips using "$dt\adj_statefips"

		rename statefips main_statefips
		rename adj_statefips statefips
		merge m:1 year statefips using "$dt\exp_amt_`h'_statefips_indexpr.dta", keep(matched master) nogenerate
		merge m:1 year statefips using "$dt\area_statefips.dta"

		rename statefips adj_statefips
		collapse (sum) exp* amt* basearea*, by (year main_statefips)
		merge m:1 year using "$dt\def_10.dta",nogenerate

		reshape long exp amt, i(year main_statefips) j(indexpr)
		gen qprice_statefips_adj=(exp/amt)/def_factor
		gen rate_statefips_adj=amt/basearea
		replace rate_statefips_adj=. if rate_statefips_adj==0
		
		reshape wide exp amt qprice_statefips_adj rate_statefips_adj, i(year main_statefips) j(indexpr)
		
		rename main_statefips statefips
		keep year statefips qprice* rate*	
		
		save "$dt\qprice_rate_`h'_statefips_adj.dta", replace

}

		
***replace missing county-level prices with adjacent county average, state average, adjacent state average, and national average
***replace missing county-level quantities with county area * per area quantity of adjacent county average, state average, adjacent state average, and national average
foreach h of global herb{
use "$dd\consp_ai_98.dta", clear
keep if year>2009
keep year scrd statefips
duplicates drop year scrd, force //complete set of county-year in our sample

merge 1:1 year scrd using "$dt\qprice_rate_`h'_scrd_indexpr.dta", nogenerate keep(master matched)
merge m:1 year statefips using "$dt\qprice_rate_`h'_statefips_indexpr.dta", nogenerate keep(master matched)
merge m:1 year statefips using "$dt\qprice_rate_`h'_statefips_adj.dta", nogenerate keep(master matched)
merge m:1 year using "$dt\qprice_rate_`h'_na_indexpr.dta", nogenerate keep(master matched)
merge 1:1 year scrd using "$dt\area_scrd.dta", nogenerate keep(master matched)

reshape long qprice_scrd  qprice_statefips qprice_statefips_adj qprice_na rate_scrd rate_statefips rate_statefips_adj rate_na,i(year scrd) j(indexpr)

gen qprice=qprice_scrd
replace qprice=qprice_statefips if qprice==.
replace qprice=qprice_statefips_adj if qprice==.
replace qprice=qprice_na if qprice==.

gen amt=basearea*rate_scrd
replace amt=basearea*rate_statefips if amt==.
replace amt=basearea*rate_statefips_adj if amt==.
replace amt=basearea*rate_na if amt==.

drop rate_* qprice_* basearea

reshape wide qprice amt, i(year scrd) j(indexpr)

keep year scrd amt* qprice*

save "$dt\fisher_prep_`h'.dta",replace

nmissing _all

}


*----------------------------price index: construct----------------------------*
***base=average of the entire period, for each CRD
foreach h of global herb{
	use "$dt\fisher_prep_`h'.dta", clear 
	reshape long amt qprice, i(year scrd) j(indexpr)

	preserve
	collapse (mean) amt , by(indexpr scrd) 
	save "$dt\fisher_`h'_baseCRDmean_amt.dta", replace
	restore

	collapse (mean) qprice [weight=amt] , by(indexpr scrd) 
	save "$dt\fisher_`h'_baseCRDmean_qprice.dta", replace

	merge 1:1 indexpr scrd using "$dt\fisher_`h'_baseCRDmean_amt.dta", nogenerate

	reshape wide amt qprice, i(scrd) j(indexpr)
	for var amt* qprice*: rename X X_base
	save "$dt\fisher_`h'_baseCRDmean.dta", replace
}


	
***calculate price index using CRD sample mean as the base.
*glyphosate
use "$dt\fisher_prep_glyp.dta", clear  
merge m:1 scrd using "$dt\fisher_glyp_baseCRDmean.dta", nogenerate 

gen Pindex=100*(qprice1*amt1+qprice2*amt2+qprice3*amt3+qprice4*amt4+qprice5*amt5)/(qprice1_base*amt1+qprice2_base*amt2+qprice3_base*amt3+qprice4_base*amt4+qprice5_base*amt5)
gen Lindex=100*(qprice1*amt1_base+qprice2*amt2_base+qprice3*amt3_base+qprice4*amt4_base+qprice5*amt5_base)/(qprice1_base*amt1_base+qprice2_base*amt2_base+qprice3_base*amt3_base+qprice4_base*amt4_base+qprice5_base*amt5_base)
gen Findex=sqrt(Pindex*Lindex)

keep year scrd *index
for var *index: rename X X_glyp

save "$dt\fisher_glyp_CRDmean_deflated.dta", replace

*composite
use "$dt\fisher_prep_comp.dta", clear  
merge m:1 scrd using "$dt\fisher_comp_baseCRDmean.dta", nogenerate keep (matched) //drop the observations for which Fisher is not obtainable.

gen Pindex=100*(qprice1*amt1+qprice2*amt2+qprice3*amt3+qprice4*amt4+qprice5*amt5+qprice6*amt6)/(qprice1_base*amt1+qprice2_base*amt2+qprice3_base*amt3+qprice4_base*amt4+qprice5_base*amt5+qprice6_base*amt6)
gen Lindex=100*(qprice1*amt1_base+qprice2*amt2_base+qprice3*amt3_base+qprice4*amt4_base+qprice5*amt5_base+qprice6*amt6_base)/(qprice1_base*amt1_base+qprice2_base*amt2_base+qprice3_base*amt3_base+qprice4_base*amt4_base+qprice5_base*amt5_base+qprice6_base*amt6_base)
gen Findex=sqrt(Pindex*Lindex)

keep year scrd *index
for var *index: rename X X_comp

save "$dt\fisher_comp_CRDmean_deflated.dta", replace


















