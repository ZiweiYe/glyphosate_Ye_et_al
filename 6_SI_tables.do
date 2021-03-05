********************************************************************************
*				program: SI tables											   *			
*				written by: Ziwei Ye										   *
* 				date: 10/13/2020											   *
*				update:	03/05/2021										   	   *
********************************************************************************

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"

global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"

cd "$do"

********************************************************************************
*							Table S1										   *
********************************************************************************
use "$dd\estim_idnum_10.dta", clear

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

gen dglyp=exp_glyp_idnum>0
gen dcomp=exp_comp_idnum>0
tab dglyp dcomp

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.
tab dglyp dcomp


********************************************************************************
*							Table S2										   *
********************************************************************************
use "$dd\estim_idnum_10.dta", clear
bys idnum: gen noyears=_N
collapse (mean) noyears, by(idnum)
asdoc tabulate noyears, ///
	title(Table S2. Overview of farm-level repeated sampling) ///
	save(tableS2.rtf) replace nocf

********************************************************************************
*							Table S3										   *
********************************************************************************
use "$dd\estim_idnum_10.dta", clear
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

gen dglyp=exp_glyp_idnum>0
gen dcomp=exp_comp_idnum>0
tab dglyp dcomp

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.
tab dglyp dcomp

*lnp
gen lnp=ln(Findex_glyp/Findex_comp)


*wcount difference
gen wcount_cum_d=wcount_cum_gly-wcount_cum_comp
gen wcount_add_d=wcount_add_gly-wcount_add_comp 


***choose alternative measures 
gen till=convt
gen wcount=wcount_cum_d
gen pfuel=pfuel_paddl1/def_factor
gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

***clean
local basevar "lnp wcount gt till pgt bt pfuel hel8"
nmissing s_gly `basevar'
egen miss=rowmiss(s_gly `basevar')
drop if miss

***export summary statistic table
asdoc summarize s_glyp `basevar', save(tableS3.rtf) replace title(Table S3. Summary statistics) dec(2) tzok





