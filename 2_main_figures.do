********************************************************************************
*				program: prepare estimation data							   *			
*				written by: Ziwei Ye										   *
* 				date: 10/13/2020											   *
*				update:											   	   	       *
********************************************************************************

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"

global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"


********************************************************************************
*							Fig. 1											   *
********************************************************************************

*---------------------------fig1a----------------------------------------------*
use "$dd\consp_ai_98.dta", clear //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient 

gen ai= productamtused* lbsai

*national level
local chem "GLYPHOSATE ATRAZINE ACETOCHLOR METOLACHLOR-S"
	foreach i of local chem {
		preserve
		keep if activeingredient=="`i'"
			
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) ai, by(year)
		
		rename ai ai_`abbr'
		
		save "$dt\temp_`abbr'.dta",replace
		restore
	} 


preserve
drop if activeingredient=="GLYPHOSATE"|activeingredient=="GLYPHOSATE"|activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
collapse (sum) ai, by(year)
rename ai ai_others
save "$dt\temp_others.dta",replace
restore

use "$dd\estim_plotid_98.dta",clear
collapse (sum) basearea, by (year)


foreach i of local chem{
	local abbr=lower(substr("`i'",1,4))
	merge 1:1 year using "$dt\temp_`abbr'.dta",nogenerate
}
merge 1:1 year using "$dt\temp_others.dta",nogenerate

//create rate_* for R.
foreach i of local chem{
	local abbr=lower(substr("`i'",1,4))
	gen rate_`abbr'=ai_`abbr'/basearea
}
gen rate_others=ai_others/basearea


keep rate_* year

reshape long rate_, i(year) j(variable,string)
rename rate_ value

sort variable year

export delimited using "$dc\Rfigure\fig1a.csv", datafmt replace



*---------------------------fig1b----------------------------------------------*
use "$dd\consp_ai_98.dta", clear //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient 

gen ai= productamtused* lbsai

preserve
keep if activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
collapse (sum) ai, by(year)
rename ai ai_comp
save "$dt\temp_comp.dta",replace

restore

use "$dd\estim_plotid_98.dta",clear
gen gtarea=gt*basearea
gen convtarea=convt*basearea
collapse (sum) basearea (sum) gtarea convtarea , by(year)
merge 1:1 year using "$dt\temp_glyp.dta", nogenerate
merge 1:1 year using "$dt\temp_comp.dta", nogenerate

gen shr_gt=gtarea/basearea
gen shr_convt=convt/basearea
gen rate_glyp=1.12*ai_glyp/basearea //multiply 1.12 to convert from lbs/ac to kg/ha
gen rate_comp=1.12*ai_comp/basearea

keep year shr_* rate_*
export delimited using "$dc\Rfigure\fig1b.csv", datafmt replace

*---------------------------fig1c----------------------------------------------*
use "$dt\wcount_use", clear 
collapse (sum) wcount_cum_*, by(year)
export delimited using "$dc\Rfigure\fig1c.csv", datafmt replace

*---------------------------fig1d----------------------------------------------*
use "$dt\consp_na_glyp", clear
merge 1:1 year using "$dt\consp_na_comp", nogenerate
merge 1:1 year using "$dt\def_98.dta", nogenerate

foreach v of varlist qprice_*_na{
	local abbr=substr("`v'",8,4)
	gen lnp`abbr'=ln(`v'/def_factor)
}

gen lnp=lnglyp-lnpcomp
keep year lnp* lnp

export delimited using "$dc\Rfigure\fig1d.csv", datafmt replace


********************************************************************************
*							Fig. 2											   *
********************************************************************************
***get the final dataset for estimation

use "$dd\estim_idnum_10", clear

*cost share
local chem "glyp comp"
foreach i of local chem{
	replace exp_`i'_idnum=0 if exp_`i'_idnum==.
}

gen exp_total_idnum=exp_glyp_idnum+exp_comp_idnum

foreach i of local chem{
	gen s_`i'=exp_`i'_idnum/exp_total_idnum
}

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.

*lnp
gen lnp=ln(qprice_glyp_countyfips/qprice_comp_countyfips)
replace lnp=ln(qprice_glyp_scrd/qprice_comp_scrd) if lnp==.
replace lnp=ln(qprice_glyp_statefips/qprice_comp_statefips) if lnp==.
replace lnp=ln(qprice_glyp_na/qprice_comp_na) if lnp==.


*wcount difference
gen wcount_cum_d=wcount_cum_gly-wcount_cum_comp
gen wcount_add_d=wcount_add_gly-wcount_add_comp 

*year dummies
tab year, gen(yr)
gen T=year-2009

***choose alternative measures 
gen till=convt
gen wcount=wcount_cum_d
gen pfuel=pfuel_paddl1/def_factor
gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

***clean
local basevar "lnp gt till wcount pgt bt pfuel hel8"
nmissing s_gly `basevar'
egen miss=rowmiss(s_gly `basevar')
drop if miss

***create gt and till percentage dummies
sum gt
gen gtmean=gt>r(mean)

sum till
gen tillmean=till>r(mean)

sum wcount
gen wcountmean=wcount>r(mean)

gen gtdum=gt>0
gen tilldum=till>0

***export
keep s_gly lnp gt* till* wcount* year statefips 
export delimited using "$dc\Rfigure\fig2.csv", datafmt replace






