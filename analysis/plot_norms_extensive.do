*===============================================================================
* Plot contribution norms (intensive margin)
* this file produces Figure 4 in the manuscript
* author: @lrdegeest
*===============================================================================

clear
version 15
set scheme lean1
graph set window fontface "PTSerif-Regular"

*===============================================================================
* Observed
use "norms_results_observed_all.dta", clear
keep if model == 1
sort norm sender_type target_type
** Target: Low 
tw	(line normal_ll norm if sender_type == 0 & target_type == 0, sort lcolor(blue) lw(thick)) ///
	(line normal_ll norm if sender_type == 1 & target_type == 0, sort lcolor(orange) lp(solid) lw(thick)) ///
	(scatteri 1 9, mcolor(black) msymbol(O) msize(3)), ///
		ytitle("Normalized log-likelihood") xtitle("Contribution norm") ///
		title("Target: Low") ///
		xlabel(0(1)10) ///
		legend(order(1 "Low" 2 "High") cols(2) title("Sender", size(4))) ///
		name(low_obs, replace) 
** Target: High
tw	(line normal_ll norm if sender_type == 0 & target_type == 1, sort lcolor(blue) lw(thick)) ///
	(line normal_ll norm if sender_type == 1 & target_type == 1, sort lcolor(orange) lp(solid) lw(thick)) ///
	(scatteri 1 29, mcolor(black) msymbol(O) msize(3)), ///
		ytitle("Normalized log-likelihood") xtitle("Contribution norm") ///
		title("Target: High") ///
		xlabel(0(4)30) ///
		legend(off) ///
		name(high_obs, replace) nodraw
		
* combine
grc1leg low_obs high_obs, title("Observed") cols(2) name(observed, replace) 
*===============================================================================

*===============================================================================
* Unobserved: unknown
use "norms_results_unobserved_unknown.dta", clear
keep if model == 1
sort norm sender_type
tw	(line normal_ll norm if sender_type == 0, sort lcolor(blue) lw(thick)) ///
	(line normal_ll norm if sender_type == 1, sort lcolor(orange) lp(solid) lw(thick)) ///
	(scatteri 1 9, mcolor(blue) msymbol(O) msize(3)) ///
	(scatteri 1 1, mcolor(orange) msymbol(O) msize(3)), ///
		ytitle("Normalized log-likelihood") xtitle("Contribution norm") ///
		title("Target: Unknown") ///
		xlabel(0(1)10) ///
		legend(off) ///
		name(unobserved_unknown, replace) nodraw

* Unobserved: reveal
use "norms_results_unobserved_reveal.dta", clear
keep if model == 1
sort norm sender_type
tw	(line normal_ll norm if sender_type == 0, sort lcolor(blue) lw(thick)) ///
	(line normal_ll norm if sender_type == 1, sort lcolor(orange) lp(solid) lw(thick)) ///
	(scatteri 1 28, mcolor(blue) msymbol(O) msize(3)) ///
	(scatteri 1 25, mcolor(orange) msymbol(O) msize(3)), ///
		ytitle("Normalized log-likelihood") xtitle("Contribution norm") ///
		title("Target: High") ///
		xlabel(11(2)30) ///
		legend(off) ///
		name(unobserved_reveal, replace) nodraw
		
grc1leg unobserved_unknown unobserved_reveal, title("Unobserved") cols(2) name(unobserved, replace)
*===============================================================================

*===============================================================================
* combine both treatments
grc1leg observed unobserved, legendfrom(observed) cols(2) name(p, replace)
graph display p, xsize(6.0) ysize(2.5)
