*===============================================================================
* Estimate contribution norms: OBSERVED endowments (complete information)
* this file produces norms_results_observed_all.dta which is then used to plot the norms/likelihood surfaces
* author: @lrdegeest
*===============================================================================

clear
version 15

* data
use ../data/norms_data_estimation.dta
* treatment: observed
keep if treatment == 2

*===============================================================================
// run models
xtset subjectid
levelsof target_type, local(target_type)
foreach i in `target_type' {
	preserve
	keep if target_type == `i'
	display "models for `i'"
	capture erase using norms_results_observed_target_`i'.dta
	postutil clear
	tempname results
	postfile `results' /// collect 9 estimation results
		norm ll model sender_type target_type ///
			constant target_cont mean_contribute dev ///
			using norms_results_observed_target_`i', replace
	// run models
	if `i' == 0 {
		local endowment 10
	}
	else if `i' == 1 {
		local endowment 30
	}
	local controls target_cont contribute mean_contribute lagsanctioncost period
	levelsof sender_type, local(sender_type)
	foreach j in `sender_type' {
		forvalues k = 0/`endowment' {
				// deviation from absolute norm
				di "estimating target = `i' and sender = `j' and norm = `k' ... "
				gen dev = cond(target_cont - `k' > 0, target_cont - `k', 0)
				local controls target_cont contribute mean_contribute lagsanctioncost period
				local dev_controls dev c.dev#c.mean_contribute
				// extensive margin
				capture noisily qui xtprobit target_sanction `controls' `dev_controls'  if sender_type == `j', re vce(cluster groupid)
				post `results' (`k') (e(ll)) (1) (`j') (`i') (_b[_cons]) (_b[target_cont]) (_b[mean_contribute]) (_b[dev])
				// intensive margin
				capture noisily qui xtpoisson target_sanction $controls `dev_controls' if sender_type == `j' & target_sanction > 0, re vce(cluster groupid)
				post `results' (`k') (e(ll)) (2) (`j') (`i') (_b[_cons]) (_b[target_cont]) (_b[mean_contribute]) (_b[dev])
				drop dev
				di "Done."
				di " "
		}
	}
	postclose `results'
	restore
}
*===============================================================================

*===============================================================================
// load the results and normalize
use norms_results_observed_target_0, clear 
append using norms_results_observed_target_1
* normalize
egen max = max(ll), by(sender_type target_type model)
egen min = min(ll), by(sender_type target_type model) 
gen normal_ll = (ll - min) / (max - min)
label define endow 0 "Low" 1 "High"
label values sender_type endow
label values target_type endow
// view the models that maximize log-likelihood
gsort -normal_ll model sender_type
// save
save norms_results_observed_all, replace
*===============================================================================