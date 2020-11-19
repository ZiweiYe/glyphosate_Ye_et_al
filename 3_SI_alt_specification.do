********************************************************************************
*				program: estimation of alternative model specifications		   *			
*				written by: Ziwei Ye										   *
* 				date: 11/04/2020											   *
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
	gen lns_`i'=ln(s_`i')
}

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 29,721 obs.


*lnp

gen lnp=ln(qprice_glyp_countyfips/qprice_comp_countyfips)
replace lnp=ln(qprice_glyp_scrd/qprice_comp_scrd) if lnp==.
replace lnp=ln(qprice_glyp_statefips/qprice_comp_statefips) if lnp==.
replace lnp=ln(qprice_glyp_na/qprice_comp_na) if lnp==.

gen p=exp(lnp)

*p1 and p2
local chem "glyp comp"
foreach i of local chem{
	gen p_`i'=qprice_`i'_countyfips
	replace p_`i'=qprice_`i'_countyfips
	replace p_`i'=qprice_`i'_statefips
	replace p_`i'=qprice_`i'_na
}



*wcount difference
gen wcount_cum_d=wcount_cum_gly-wcount_cum_comp
gen wcount_add_d=wcount_add_gly-wcount_add_comp 

*year dummies
tab year, gen(yr)
gen T=year-2009

*size
gen size=basearea/1000

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
*							estimation: without controls					   *
********************************************************************************
***generalized Box-Cox
cap drop *hat eos
cap program drop nlbc
program nlbc, rclass
	version 15
	syntax varlist(min=3 max=3) if
	local lns1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	//local gtvar: word 3 of `varlist'
	
	return local eq "`lns1'=ln({b1=0.1}*(`p1'/`p2')^{lam}+0.5*{b2=0.1}*(`p1'/`p2')^(0.5*{lam}))-ln({b1=0.1}*(`p1'/`p2')^{lam}+{b2=0.1}*(`p1'/`p2')^(0.5^{lam})+1)"
end

nl bc: lns_gly p_glyp p_comp, nolog

predict lns1hat, yhat
gen s1hat=exp(lns1hat)
gen s2hat=1-s1hat

//eos
margins, dyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos



***Generalized Leontief
cap drop *hat eos
cap program drop nlgl
program nlgl, rclass
	version 15
	syntax varlist(min=3 max=3) if
	local lns1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	//local gtvar: word 3 of `varlist'
	
	return local eq "`lns1'=ln((`p1'/`p2')+{b1=0.1}*(`p1'/`p2')^0.5)-ln((`p1'/`p2')+2*{b1=0.1}*(`p1'/`p2')^0.5+{b2=0.1})"
end

nl gl: lns_gly p_glyp p_comp, nolog

predict lns1hat, yhat
gen s1hat=exp(lns1hat)
gen s2hat=1-s1hat

//eos
margins, dyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos


***GSRQ
cap drop *hat eos
cap program drop nlgsrq
program nlgsrq, rclass
	version 15
	syntax varlist(min=3 max=3) if
	local lns1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	
	return local eq "`lns1'=ln({b11=0.1}*(`p1'/`p2')^2+0.5*{b12=0.1}*(`p1'/`p2'))-ln({b11=0.1}*(`p1'/`p2')^2+{b12=0.1}*(`p1'/`p2')+{b22=0.1})"
end

nl gsrq: lns_gly p_glyp p_comp, nolog

predict lns1hat, yhat
sum lns1hat
gen s2hat=1-exp(lns1hat)

//eos
margins, dyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos



***Translog
cap drop *hat eos
cap program drop nltrans
program nltrans, rclass
	version 15
	syntax varlist(min=3 max=3) if
	local s1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	//local gtvar: word 3 of `varlist'
	
	return local eq "`s1'={b0=0.5}+{b1=0.1}*ln(`p1'/`p2')"
end

nl trans: s_gly p_glyp p_comp, nolog

predict s1hat, yhat
gen s2hat=1-s1hat

//eos
margins, eyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos

drop *hat eos

reg s_gly lnp

predict s1hat, xb
gen s2hat=1-s1hat

//eos
margins, eydx(lnp) post
margins, coeflegend

gen eos=1-(1/s2hat)*_b[lnp]
sum eos

********************************************************************************
*							estimation: with controls						   *
********************************************************************************
***Generalized Leontief
cap drop *hat eos
cap program drop nlgl
program nlgl, rclass
	version 15
	syntax varlist(min=6 max=6) if
	local lns1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	local w1: word 4 of `varlist'
	local y1: word 5 of `varlist'
	local y2: word 6 of `varlist'
	
	return local eq "`lns1'=ln({b1=0.1}*(`p1'/`p2')+{b2=0.1}*(`p1'/`p2')^0.5)-ln({b1=0.1}*(`p1'/`p2')+2*{b2=0.1}*(`p1'/`p2')^0.5+1)+{r0=0.01}*`w1'+{r1=0.15}*`y1'+{r2=0.01}*`y2'"
end

nl gl: lns_gly p_glyp p_comp wcount gt till, nolog

predict lns1hat, yhat
gen s2hat=1-exp(lns1hat)

//eos
margins, dyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos


***Translog
cap drop *hat eos
cap program drop nltrans
program nltrans, rclass
	version 15
	syntax varlist(min=6 max=6) if
	local s1: word 1 of `varlist'
	local p1: word 2 of `varlist'
	local p2: word 3 of `varlist'
	local w1: word 4 of `varlist'
	local y1: word 5 of `varlist'
	local y2: word 6 of `varlist'
	
	return local eq "`s1'={b0=0.5}+{b1=0.1}*ln(`p1'/`p2')+{r0=0.01}*`w1'+{r1=0.15}*`y1'+{r2=0.01}*`y2'"
end

nl trans: s_gly p_glyp p_comp wcount gt till, nolog

predict s1hat, yhat
gen s2hat=1-s1hat

//eos
margins, eyex(p_comp) post
margins, coeflegend

gen eos=1+(1/s2hat)*_b[p_comp]
sum eos

