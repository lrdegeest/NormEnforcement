*===============================================================================
* estimate expected costs of non-compliance
* this file produces ec.dta which is then used to plot the expected costs
* author: @lrdegeest
*===============================================================================

*===============================================================================
* data, controls
clear
version 15
use ../data/norms_data_estimation.dta
xtset subjectid
gen reveal = 1 if target_cont > 10
egen sum_reveal = sum(reveal), by(subjectid period)
global controls target_cont contribute mean_contribute lagsanctioncost period

*===============================================================================
capture program drop expected_cost
program define expected_cost
	*======================================
	* three arguments:
		* 1. extensive norm
		* 2. intensive norm
		* 3. name of expected cost matrix
	* returns:
		* expected cost matrix
	*======================================
    version 15
	syntax anything [if]
	preserve
	keep `if' 
	levelsof treatment, local(t)
	*======================================
	* EXTENSIVE MARGIN
	local norm1 `1'
	gen dev = cond(target_cont - `norm1' > 0, target_cont - `norm1', 0) 
	if `t' == 1 {
		qui xtprobit target_sanction $controls dev c.dev#c.mean_contribute i.sum_reveal, re vce(cluster groupid) iter(100)
		di "EM: iterations: " e(ic)
		qui summarize target_cont
		if r(max) == 10 {			
			qui margins, at(target_cont = (0(1)10)) post
			matrix a = e(b)
		}
		else if r(max) == 30 {
			qui margins, at(target_cont = (11(1)30)) post
			matrix a = e(b)
		}
	}
	else if `t' == 2 {
		qui xtprobit target_sanction $controls dev c.dev#c.mean_contribute i.sum_reveal, re vce(cluster groupid) iter(100)
		di "EM iterations: " e(ic)
		levelsof target_type, local(target)
		if `target' == 0 {
			qui margins, at(target_cont = (0(1)10)) post
			matrix a = e(b)
		}
		else if `target' == 1 {
			qui margins, at(target_cont = (0(1)30)) post
			matrix a = e(b)
		}
	}
	drop dev
	*======================================
	* INTENSIVE MARGIN
	local norm2 `2'
	gen dev = cond(target_cont - `norm2' > 0, target_cont - `norm2', 0)
	if `t' == 1 {
		qui xtpoisson target_sanction $controls dev c.dev#c.mean_contribute i.sum_reveal if target_sanction > 0, re vce(cluster groupid) iter(100)
		di "IM iterations: " e(ic)
		qui summarize target_cont
		if r(max) == 10 {
			qui margins, at(target_cont = (0(1)10)) predict(xb) post
			matrix a2 = e(b)
		}
		else if r(max) == 30 {
			qui margins, at(target_cont = (11(1)30)) predict(xb) post
			matrix a2 = e(b)
		}
		mata : st_matrix("b", 3*exp(st_matrix("a2")))
		local dim `= colsof(b)'
		forvalues i = 1/`dim' {
			if b[1,`i'] > 10 {
				matrix b[1,`i'] = 10
			}
		}
	}
	else if `t' == 2 {
		qui xtpoisson target_sanction $controls dev c.dev#c.mean_contribute if target_sanction > 0, re vce(cluster groupid) iter(100)		
		di "IM iterations: " e(ic)
		levelsof target_type, local(target)
		if `target' == 0 {
			qui margins, at(target_cont = (0(1)10)) predict(xb) post
			matrix a2 = e(b)
			mata : st_matrix("b", 3*exp(st_matrix("a2")))
			local dim `= colsof(b)'
			forvalues i = 1/`dim' {
				if b[1,`i'] > 10 {
					matrix b[1,`i'] = 10
				}
			}
		}
		else if `target' == 1 {
			qui margins, at(target_cont = (0(1)30)) predict(xb) post
			matrix a2 = e(b)
			mata : st_matrix("b", 3*exp(st_matrix("a2")))
			local dim `= colsof(b)'
			forvalues i = 1/`dim' {
				if b[1,`i'] > 10 {
					matrix b[1,`i'] = 10
				}
			}			
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
	matrix `3'=ec'
	restore
	*======================================
end



*===============================================================================
* RUN MODELS

*======================================
* set up
matrix x = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
matrix x_prime = x'
matrix x2 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
matrix x2_prime = x2'
matrix x3 = (11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
matrix x3_prime = x3'

*======================================
* UNOBSERVED, target_cont <=10
* Low
expected_cost 9 9 L0Unknown if treatment == 1 & sender_type == 0 & target_cont <= 10
* High
expected_cost 1 8 H0Unknown if treatment == 1 & sender_type == 0 & target_cont <= 10

* UNOBSERVED, target_cont > 10
* Low
expected_cost 28 16 L0H if treatment == 1 & sender_type == 0 & target_cont > 10
* High
expected_cost 25 17 H0H if treatment == 1 & sender_type == 0 & target_cont > 10

* UNOBSERVED, target_cont <=10 & sender=Low and sum_reveal==2 (sender knows target is Low)
expected_cost 9 9 L0L if treatment == 1 & sender_type == 0 & target_cont <= 10 & sum_reveal == 2
* UNOBSERVED, target_cont <=10 & sender=High and sum_reveal==1 (sender knows target is Low)
expected_cost 9 9 H0L if treatment == 1 & sender_type == 1 & target_cont <= 10 & sum_reveal == 1

* join results into matrix
** unknown
matrix unobserved_unknown = x2_prime, L0Unknown, H0Unknown
matrix colnames unobserved_unknown = targetcont10 L0Unknown H0Unknown
** low
matrix unobserved_low = L0L, H0L
matrix colnames unobserved_low = L0L H0L
** high
matrix unobserved_high = x3_prime,L0H,H0H
matrix colnames unobserved_high = targetcont11_30 L0H H0H


*======================================
* OBSERVED
* Low-Low
expected_cost 9 9 L1L if treatment == 2 & sender_type == 0 & target_type == 0
* Low-High
expected_cost 9 29 L1H if treatment == 2 & sender_type == 0 & target_type == 1
* High-Low
expected_cost 9 9 H1L if treatment == 2 & sender_type == 1 & target_type == 0
* High-High
expected_cost 9 28 H1H if treatment == 2 & sender_type == 1 & target_type == 1

* join results into matrices
matrix observed_low = L1L, H1L
matrix colnames observed_low = L1L H1L
matrix observed_high = x_prime, L1H, H1H
matrix colnames observed_high = targetcont30 L1H H1H

*===============================================================================
* join all matrices into new data frame and save
clear
svmat unobserved_unknown, names(col)
svmat unobserved_high, names(col)
svmat unobserved_low, names(col)
svmat observed_low, names(col)
svmat observed_high, names(col)
* unobserved
gen TotalLowUnobserved = L0Unknown + 2*H0Unknown
gen TotalLowUnobserved_reveal = L0L + 2*H0L
gen TotalHighUnobserved_Unknown = 2*L0Unknown + H0Unknown
gen TotalHighUnobserved_reveal = 2*L0H + H0H
* observed
gen TotalLowObserved = L1L + 2*H1L
gen TotalHighObserved = 2*L1H + H1H
* save
save "ec.dta", replace
*===============================================================================