*===============================================================================
* Expected costs of noncompliance with estimated norms
* this file produces Figure 6 in the mansucript
* author: @lrdegeest
*===============================================================================

version 15
clear
use ec.dta

* low
tw	(line TotalLowUnobserved targetcont10, lc(blue) lp(shortdash_dot) lw(thick)) ///
	(line TotalLowUnobserved_reveal targetcont10, lc(blue) lp(dash) lw(thick)) ///
	(line TotalLowObserved targetcont10, lc(blue) lp(solid) lw(thick)), ///
	ytitle("Expected Punishment") xtitle("Contribution") ///
	title("Low")  ///
	legend(order(1 "Unobserved (High/Low pooled)" 2 "Unobserved (High/Low separated)" 3 "Observed") cols(1) ring(0) position(2) region(color("white%0"))) ///
	name(low, replace)  nodraw
* high
tw	(line TotalHighUnobserved_Unknown targetcont30, lc(orange) lp(shortdash_dot) lw(thick)) ///
	(line TotalHighUnobserved_reveal targetcont11_30, lc(orange) lp(dash) lw(thick)) ///
	(line TotalHighObserved targetcont30, lc(orange) lp(solid) lw(thick)), ///
	ytitle("Expected Punishment") xtitle("Contribution") ///
	title("High") ///
	legend(order(1 "Unobserved (High/Low pooled)" 2 "Unobserved (High/Low separated)" 3 "Observed") cols(1) ring(0) position(2) region(color("white%0"))) ///
	name(high, replace) nodraw
* combine
graph set window fontface PTSerif-Regular
gr combine low high, ycommon
