********************************************************************************
*				program: estimation											   *			
*				written by: Ziwei Ye										   *
* 				date: 10/16/2020											   *
*				update:											   	   	       *
********************************************************************************

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"

global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"


*-----------------------------plotid level-------------------------------------*
use "$dd\estim_plotid_10", clear

gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

gen T=year-2009

local chem "glyp atra acet meto comp"
foreach i of local chem {
	gen `i'_rate=ai_`i'_plotid/basearea //in lb/acre
	replace `i'_rate=0 if `i'_rate==.
} //generate the dependent variables


foreach i of local chem {
	ivreghdfe `i'_rate i.year#c.gt (gt=pgt bt), absorb(fe=i.year i.idnum i.scrd#c.T) cluster(scrd)
	est store `i'
	drop fe
	
}



cd "$do"
coefplot atra || acet || meto || comp, drop(_cons gt) xline(0) 
graph save fig1.gph, replace
//manually change the labels

/*
coefplot atra || acet || meto || comp, drop(_cons) yline(0) bycoefs vertical 
graph save fig2.gph, replace






