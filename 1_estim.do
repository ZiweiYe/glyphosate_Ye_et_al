********************************************************************************
*				program: estimation											   *			
*				written by: Ziwei Ye										   *
* 				date: 10/29/2020											   *
*				update:											   	   	       *
********************************************************************************
clear
set more off 

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"
global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"

********************************************************************************
*							prepare dataset for estimation					   *
********************************************************************************
use "$dd\estim_idnum_10", clear

***generate vars
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

*---------------------------final dataset for estimation-----------------------*

********************************************************************************
*							estimation: fracglm								   *
********************************************************************************
***prep
*tobs
bys id: gen tobs=_N //number of time periods for each id
tab tobs



*---------------------------model 1: endo gt and till--------------------------*

local z1var "lnp wcount"
local z2var "pgt bt pfuel hel8"
local y1 "gt"
local y2 "till"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr1-yr6 {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y1' `z2var' `z1var' ///
	lam*_*bar lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m1y1
test `z2var'
local f1=r(F)
predict v1hat, resid


reg `y2' `z2var' `z1var' ///
	lam*_*bar lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m2y2
test `z2var'
local f2=r(F)
predict v2hat, resid


//step 2
fhetprob s_glyp `z1var' `y1' `y2' v1hat v2hat ///
	lam*_* lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)
test v1hat v2hat

cd "$do"
outreg2 using second, word append ///
	keep (`z1var' `y1' `y2' v1hat v2hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	addstat("F-statistic (GT)", `f1', ///
			"F-statistic (Till)", `f2', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) dec(3) 

drop v1hat v2hat lam* *bar





*---------------------------model 2: endo gt --------------------------*

local z1var "lnp wcount till"
local z2var "pgt bt"
local y1 "gt"


forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr1-yr6 {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y1' `z2var' `z1var' ///
	lam*_*bar lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m2y1
test `z2var'
local f1=r(F)
predict v1hat, resid

	//overidentification test
	ivregress 2sls s_glyp `z1var'  ///
	lam*_* lam2-lam7 ib(last).year i.statefips#c.T i.statefips (`y1'=`z2var'), ///
	robust
	estat overid
	local score1=r(score)
	local pscore1=r(p_score)

//step 2
fhetprob s_glyp `z1var' `y1' v1hat ///
	lam*_* lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)

cd "$do"
outreg2 using second, word append ///
	keep (`z1var' `y1' v1hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	addstat("F-statistic (GT)", `f1', ///
			"score chi-squared (GT)", `score1', ///
			"p-value for score (GT)", `pscore1', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) dec(3) 




drop v1hat lam* *bar


*---------------------------model 3: endo till --------------------------*

local z1var "lnp wcount gt"
local z2var "pfuel hel8"
local y2 "till"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr1-yr6 {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y2' `z2var' `z1var' ///
	lam*_*bar lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m3y2
test `z2var'
local f2=r(F)
predict v2hat, resid

	//overidentification test
	ivregress 2sls s_glyp `z1var'  ///
	lam*_* lam2-lam7 ib(last).year i.statefips i.statefips#c.T i.statefips (`y2'=`z2var'), ///
	robust
	estat overid
	local score2=r(score)
	local pscore2=r(p_score)

//step 2
fhetprob s_glyp `z1var' `y2' v2hat ///
	lam*_* lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)

cd "$do"
outreg2 using second, word append ///
	keep (`z1var' `y2' v2hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	addstat("F-statistic (Till)", `f2', ///
			"score chi-squared (Till)", `score2', ///
			"p-value for score (Till)", `pscore2', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) dec(3) 

drop v2hat lam* *bar

*--------------------------export first-stage results--------------------------*
esttab m2y1 m2y2 m3y1 m4y2 using first.rtf, ///
	keep(lnp wcount gt till pgt bt pfuel hel8) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("model 2: gt" "model 2: till" "model 3: gt" "model 4: till") ///
	title ("Table A1. First-stage regression results") ///
	b(%9.4f) ///
	replace

