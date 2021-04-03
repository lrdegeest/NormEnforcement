*===============================================================================
* estimate expected costs of non-compliance with estimated norms
* produces results for Figure 7
* author: @lrdegeest
*===============================================================================

*===============================================================================
capture program drop expected_cost
program define expected_cost
	clear
    version 15
	
	quietly{
		// set dir here if necessary
		use data/norms_data_estimation.dta 
	}
	
	syntax [anything] [if], treatment(integer) ///
							extensive(integer) intensive(integer) ///
							lowerbound(integer) upperbound(integer) ///
							filename(string) ///
							[reveal(integer 0)]
	
	di "Expected costs of non compliance "
	di " treatment: `treatment'"
	di " norms: extensive = `extensive', intensive = `intensive'"
	di " enforcement from `lowerbound' to `upperbound'"
	di " storing results in memory with matrix `filename'"
	
	quietly{
		keep if treatment == `treatment'
		if `treatment' == 1 {
			gen reveal = 1 if target_cont > 10
			egen sum_reveal = sum(reveal), by(groupid period)
			replace sum_reveal = 1 if sum_reveal == 3
			replace sum_reveal = 2 if sum_reveal == 6
			keep if sum_reveal == `reveal'			
		}
		keep `if'		
		xtset subjectid
		capture erase `filename'
		postutil clear
		tempname results
		postfile `results' /// 
			norm sender_type model ll converged iter N ///
			using `filename', replace
	}
	
	local controls target_cont contribute mean_contribute lagsanctioncost period
	*======================================
	* EXTENSIVE MARGIN
	gen dev = cond(target_cont - `extensive' > 0, target_cont - `extensive', 0) 
	qui xtprobit target_sanction `controls' dev c.dev#c.mean_contribute, re vce(cluster groupid) iter(100)
	di "EM: iterations: " e(ic)
	qui margins, at(target_cont = (`lowerbound'(1)`upperbound')) post
	matrix a = e(b)
	drop dev
	*======================================
	* INTENSIVE MARGIN
	gen dev = cond(target_cont - `intensive' > 0, target_cont - `intensive', 0)
	qui xtpoisson target_sanction `controls' dev c.dev#c.mean_contribute if target_sanction > 0, re vce(cluster groupid) iter(100)
	di "IM iterations: " e(ic)
	qui margins, at(target_cont = (`lowerbound'(1)`upperbound')) predict(xb) post
	matrix a2 = e(b)
	mata : st_matrix("b", 3*exp(st_matrix("a2")))
	local dim `= colsof(b)'
	forvalues i = 1/`dim' {
		if b[1,`i'] > 10 {
			matrix b[1,`i'] = 10
		}
	}
	drop dev
	*======================================
	* EXPECTED COST: P(sanction)*E[sanction | sanction > 0]
	local dim `= colsof(a)'
	matrix ec = J(1,`dim',0)
	forvalues i = 1/`dim' {
		matrix ec[1,`i'] = a[1,`i']*b[1,`i'] // P*S
	}
	matrix `filename'=ec'
	*======================================
end
*===============================================================================


*===============================================================================
* empty contribution matrices
mata: st_matrix("x0_10", range(0,10,1))
mata: st_matrix("x11_30", range(11,30,1))
mata: st_matrix("x0_30", range(0,30,1))
*===============================================================================


* RUN MODELS

*===============================================================================
* OBSERVED

* Low-Low
expected_cost if sender_type == 0 & target_type == 0,
	treatment(2) ///
	extensive(9) intensive(9) ///
	lowerbound(0) upperbound(10) ///
	filename(L1L) 
* Low-High
expected_cost if sender_type == 0 & target_type == 1,
	treatment(2) ///
	extensive(29) intensive(29) ///
	lowerbound(0) upperbound(10) ///
	filename(L1H)
* High-Low
expected_cost if sender_type == 0 & target_type == 1,
	treatment(2) ///
	extensive(9) intensive(9) ///
	lowerbound(0) upperbound(10) ///
	filename(H1L) 
* High-High
expected_cost if sender_type == 1 & target_type == 1,
	treatment(2) ///
	extensive(29) intensive(28) ///
	lowerbound(0) upperbound(30) ///
	filename(H1H) 

* join results into matrices
matrix observed_low = L1L, H1L
matrix colnames observed_low = L1L H1L
matrix observed_high = x0_30, L1H, H1H
matrix colnames observed_high = targetcont30 L1H H1H
* join all matrices into new data frame and save
clear
svmat observed_low, names(col)
svmat observed_high, names(col)
gen TotalLowObserved = L1L + 2*H1L
gen TotalHighObserved = 2*L1H + H1H
* save
save "ec_observed.dta", replace
*===============================================================================

*===============================================================================
* NO HIGH REVEAL

* Low
expected_cost 8 9 Low if sender_type == 0
expected_cost if sender_type == 0 & target_cont <= 10, ///
	treatment(1) reveal(0) ///
	extensive(8) intensive(9) ///
	lowerbound(0) upperbound(10) ///
	filename(Low)  
* High
expected_cost if sender_type == 1 & target_cont <= 10, ///
	treatment(1) reveal(0) ///
	extensive(1) intensive(9) ///
	lowerbound(0) upperbound(10) ///
	filename(High)  

* join results into matrix
matrix unobserved_noreveal = x0_10, Low, High
matrix colnames unobserved_noreveal = target_cont Low High
* join all matrices into new data frame and save
clear
svmat unobserved_noreveal, names(col)
gen TotalLow= Low + 2*High
gen TotalHigh= 2*Low + High
* save
save "ec_no_reveal.dta", replace
*===============================================================================

*===============================================================================
* ONE HIGH REVEAL

* low targeting [0,10]
expected_cost if sender_type == 0 & target_cont <= 10, ///
	treatment(1) reveal(1) ///
	extensive(8) intensive(8) ///
	lowerbound(0) upperbound(10) ///
	filename(LowUnknown) 

* high targeting [0,10]
expected_cost if sender_type == 1 & target_cont <= 10, ///
	treatment(1) reveal(1) ///
	extensive(1) intensive(7) ///
	lowerbound(0) upperbound(10) ///
	filename(HighUnknown) 

* low targeting high
expected_cost if sender_type == 0 & target_cont > 10, ///
	treatment(1) reveal(1) ///
	extensive(20) intensive(20) ///
	lowerbound(11) upperbound(30) ///
	filename(LowHigh) 

* high targeting high: no estimated norms

* join results into matrix
matrix unobserved_unknown = x0_10, LowUnknown, HighUnknown
matrix colnames unobserved_unknown = targetcont10 LowUnknown HighUnknown
matrix unobserved_high = x11_30, LowHigh 
matrix colnames unobserved_high = targetcont11_30 LowHigh 
* join all matrices into new data frame and save
clear
svmat unobserved_unknown, names(col)
svmat unobserved_high, names(col)
gen TotalLowUnobserved = LowUnknown + 2*HighUnknown
gen TotalHighUnobserved_Unknown = 2*LowUnknown + HighUnknown
gen TotalHighUnobserved_reveal = 2*LowHigh //+ HighHigh
save "ec_one_reveal.dta", replace
*===============================================================================

*===============================================================================
* BOTH HIGH REVEAL

* low targeting low
expected_cost if sender_type == 0 & target_cont <= 10, ///
	treatment(1) reveal(2) ///
	extensive(1) intensive(6) ///
	lowerbound(0) upperbound(10) ///
	filename(LowLow) 
	
* high targeting low: no estimated norms	

* low targeting high
expected_cost if sender_type == 0 & target_cont > 10, ///
	treatment(1) reveal(2) ///
	extensive(20) intensive(17) ///
	lowerbound(11) upperbound(30) ///
	filename(LowHigh)

* high targeting high: no estimated norms	
	
* join results into matrix
matrix low = x0_10, LowLow
matrix colnames low = targetcont10 LowLow
matrix high = x11_30, LowHigh
matrix colnames high = targetcont11_30 LowHigh
* join all matrices into new data frame and save
clear
svmat low, names(col)
svmat high, names(col)
gen TotalLow = LowLow
gen TotalHigh = 2*LowHigh
save "ec_both_reveal.dta", replace
*===============================================================================
