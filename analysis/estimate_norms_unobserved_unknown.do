*===============================================================================
* Estimate contribution norms: UNOBSERVED endowments (incomplete information)
* this file produces norms_results_unobserved_unknown.dta which is then used to plot the norms/likelihood surfaces
* author: @lrdegeest
*===============================================================================

clear
version 15

* data
use ../data/norms_data_estimation.dta
* treatment: unobserved
gen reveal = 1 if target_cont > 10
egen sum_reveal = sum(reveal), by(subjectid period)
keep if treatment == 1 & target_cont <= 10

*===============================================================================
xtset subjectid
global controls target_cont contribute mean_contribute lagsanctioncost period
capture erase norms_results_unobserved_unknown.dta
postutil clear
tempname results
postfile `results' /// collect 10 estimation results:
	norm sender_type model ll constant target_cont mean_contribute dev ///
	n_1 n_2 /// n_1 is observations for intensive margin and n_2 is observations for extensive margin
	using norms_results_unobserved_unknown
	levelsof sender_type, local(sender_type)
	foreach j in `sender_type' {
		forvalues k = 0/10 {
			di "Estimating sender = `j' and norm = `k' ... "
			// deviation from absolute norm
			gen dev = cond(target_cont - `k' > 0, target_cont - `k', 0)
			local controls target_cont contribute mean_contribute lagsanctioncost period i.sum_reveal
			local dev_controls dev c.dev#c.mean_contribute
			// extensive margin
			capture noisily qui xtprobit target_sanction `controls' `dev_controls' if sender_type == `j', re vce(cluster groupid)
			post `results' (`k') (`j') (1) (e(ll)) (_b[_cons]) (_b[target_cont]) (_b[mean_contribute]) (_b[dev]) (e(N)) (.)
			// intensive margin
			capture noisily qui xtpoisson target_sanction $controls `dev_controls' if sender_type == `j' & target_sanction > 0, re vce(cluster groupid)
			post `results' (`k')  (`j') (2) (e(ll)) (_b[_cons]) (_b[target_cont]) (_b[mean_contribute]) (_b[dev]) (.) (e(N))
			drop dev
			di "Done."
			di " "
		}
	}
postclose `results'
*===============================================================================

*===============================================================================
// load the results and normalize
use norms_results_unobserved_unknown, clear 
* normalize
egen max = max(ll), by(model sender_type)
egen min = min(ll), by(model sender_type) 
gen normal_ll = (ll - min) / (max - min)
* label
label define sender 0 "Low" 1 "High"
label values sender_type sender
// view the models that maximize log-likelihood
gsort -normal_ll model
// save
save norms_results_unobserved_unknown, replace
*===============================================================================
