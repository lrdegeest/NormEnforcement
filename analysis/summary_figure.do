*===============================================================================
* average contributions and distributions
* this file produces Figure 2 in the manuscript
* author: @lrdegeest
*===============================================================================

clear
version 15
use ../data/data_labels.dta

*===============================================================================
* CONTRIBUTIONS

* average by treatment/endowment/time
preserve
collapse (mean) mean_cont=contribute, by(period endowment treatment)
tw	(connected mean_cont period if treatment == 0 & endowment == 0, msymbol(oh) lcolor(blue*0.5) mcolor(blue)) /// 
	(connected mean_cont period if treatment == 0 & endowment == 1, msymbol(oh) lcolor(orange*0.75) mcolor(orange)) /// 
	(connected mean_cont period if treatment == 1 & endowment == 0, msymbol(o) lcolor(blue*0.5) mcolor(blue)) /// 
	(connected mean_cont period if treatment == 1 & endowment == 1, msymbol(o) lcolor(orange*0.75) mcolor(orange)), /// 
		ytitle("Average Contribution") xtitle("Period") xlabel(0(10)50) ylabel(0(5)30) ///
		legend(order(1 "Low (Unobserved)" 2 "High (Unobserved)" 3 "Low (Observed)"  4 "High (Observed)" ) cols(2)) ///
		scheme(lean1) ///
		subtitle("Contributions over time") ///
		legend(ring(0) position(7) size(small)) name(cont_avg_time, replace) nodraw
restore							

*====================================
* DISTRIBUTIONS
capture program drop draw_dists
program draw_dists
	version 15
	syntax anything [if]
	preserve
	keep `if'
	levelsof endowment, local(endow)
	quietly {
		foreach i in `endow' {
			capture drop x`i'_early dens`i'_early x`i'_late dens`i'_late
			kdensity contribute if period <= 10 & endowment == `i', generate(x`i'_early dens`i'_early) nodraw
			kdensity contribute if period >= 39 & period < 50 & endowment == `i', generate(x`i'_late dens`i'`j'_late) nodraw
		}
	}
	gen zero_base = 0
	twoway	(rarea dens0_early zero_base x0_early, color("blue%25")) ///
			(rarea dens0_late zero_base x0_late, color("blue%75")), ///
			title("Low") ytitle("Smoothed density") xtitle("Contribution") ///
			ylabel(0(0.2)0.8) ///
			legend(ring(0) pos(11) col(1) order(1 "Periods 1-10" 2 "Periods 39-49") region(color("white%0"))) ///
			name(low_density, replace) nodraw			
	twoway	(rarea dens1_early zero_base x1_early, color("orange%30")) ///
			(rarea dens1_late zero_base x1_late, color("orange%65")), ///
			title("High") ytitle("Smoothed density") xtitle("Contribution") ///
			legend(ring(0) pos(11) col(1) order(1 "Periods 1-10" 2 "Periods 39-49") region(color("white%0"))) ///
			ylabel(0(0.02)0.08) xlabel(0(10)30) ///
			name(high_density, replace) nodraw
	gr combine low_density high_density, title(`2') name(`1', replace) nodraw		
	drop zero_base x* dens*
	restore
end
draw_dists p1 "Observed" if treatment == 1
draw_dists p2 "Unobserved" if treatment == 0
gr combine p1 p2, cols(1) name(dist, replace) nodraw

* combine
graph set window fontface PTSerif-Regular
gr combine cont_avg_time dist, cols(2) name(p, replace) nodraw
graph display p, xsize(6.0) ysize(3)
*===============================================================================
