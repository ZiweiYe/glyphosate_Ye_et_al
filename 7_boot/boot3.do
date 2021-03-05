********************************************************************************
*				program: bootstrapping, Model 3								   *			
*				written by: Ziwei Ye										   *
* 				date: 10/29/2020											   *
*				update:	03/05/2021								   	   	       *
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

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 29,721 obs.

*lnp, price index
merge m:1 year scrd using "$dt\fisher_glyp_CRDmean_deflated.dta",keep(matched master) nogenerate 
merge m:1 year scrd using "$dt\fisher_comp_CRDmean_deflated.dta",keep(matched master) nogenerate 
gen lnp=ln(Findex_glyp/Findex_comp)


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

cap program drop estim_boot
program estim_boot, rclass
version 15.0


	***first stage
	
	reg till pfuel hel8 lnp wcount ///
		lam*_*bar lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
		vce(cluster scrd)
	predict v2hat, resid

	***second stage
	fhetprob s_glyp lnp wcount gt till v2hat ///
	lam*_* lam2-lam7 ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) iter(100) ///
	vce(cluster scrd) 
	predict xbhat, xb


	***post estimation
	//return estimates
	return scalar blnp=[#1]lnp
	return scalar bwcount=[#1]wcount
	return scalar bgt=[#1]gt
	return scalar btill=[#1]till
	return scalar bv2hat=[#1]v2hat

	//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
		return scalar ape`v'=`scale'*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		return scalar m`i'=r(mean)
	}

drop v2hat xbhat scale pe* pr_s elas* eos 

end

bootstrap r(blnp) r(bwcount) r(bgt) r(btill) r(bv2hat) ///
	r(apelnp) r(apewcount) r(apegt) r(apetill) ///
	r(melas11) r(melas22) r(melas12) r(melas21) r(meos), cluster(idnum) ///
	reject(e(converged)!=1) reps(1000) seed(1125) idcluster(newid):estim_boot
est store boot
test _bs_5 
estadd scalar chi2=r(chi2)

cd "$dt"

esttab boot using boot3.rtf, ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	b(%9.4f) ///
	stats(chi2) ///
	replace
	
drop newid


