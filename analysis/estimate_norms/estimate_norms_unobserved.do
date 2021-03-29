*===============================================================================
* Estimate contribution norms: UNOBSERVED endowments (incomplete information)
* produces results for Table 3 and Figures 5-6
* author: @lrdegeest
*===============================================================================

*===============================================================================
* program to run models for unobserved
** a pain to adapt this for observed since that treatment involves another loop
capture program drop estimate_norms_unobserved
program define estimate_norms_unobserved
	*======================================
	* arguments:
		* 1. filename: name of results file (the output) 
		* 2. reveal: 0, 1, or 2 high reveal 
		* 3. lower bound for grid search (cannot be less than 0)
		* 4. upper bound for grid search (cannot exceed 30)
	* returns:
		* dta file ("filename.dta") with all estimated norms and their normalized LL
		* to view the max-LL norm for each model/sender_type do "keep normal_ll == 1"
	*======================================
	clear
	version 15
	
	quietly{
		cd "~/Google Drive/research_gdrive/NormEnforcement/data"
		use norms_data_estimation.dta 
	}
	
	syntax [anything] [if], reveal(integer) lowerbound(integer) upperbound(integer) filename(string) 
	
	di "Estimating norms for Unobserved with the following parameters: "
	di " reveal: `reveal', lower: `lowerbound', upper: `upperbound'"
	di " saving results to `filename'"
	
	quietly{
		keep if treatment == 1
		gen reveal = 1 if target_cont > 10
		egen sum_reveal = sum(reveal), by(groupid period)
		replace sum_reveal = 1 if sum_reveal == 3
		replace sum_reveal = 2 if sum_reveal == 6
		keep if sum_reveal == `reveal'
		keep `if'		
		xtset subjectid
		capture erase `filename'
		postutil clear
		tempname results
		postfile `results' /// 
			norm sender_type model ll converged iter N ///
			using `filename', replace
	}
	levelsof sender_type, local(sender_type)
	foreach j in `sender_type' {
		di "here"
		forvalues k =`lowerbound'/`upperbound' {
			di "Estimating sender = `j' and norm = `k' ... "
			// deviation from absolute norm
			gen dev = cond(target_cont - `k' > 0, target_cont - `k', 0)
			local controls target_cont contribute mean_contribute lagsanctioncost period
			local dev_controls dev c.dev#c.mean_contribute
			// extensive margin
			capture noisily qui xtprobit target_sanction `controls' `dev_controls' if sender_type == `j', re vce(cluster groupid) technique(nr 100 bfgs 100)
			post `results' (`k') (`j') (1) (e(ll)) (e(converged)) (e(ic)) (e(N)) 
			// intensive margin
			capture noisily qui xtpoisson target_sanction `controls' `dev_controls' if sender_type == `j' & target_sanction > 0, re vce(cluster groupid) technique(nr 100 bfgs 100)
			post `results' (`k')  (`j') (2) (e(ll)) (e(converged)) (e(ic)) (e(N))
			drop dev
			di "Done."
			di " "
		}
	}
	
	postclose `results'
	use `filename', clear 
	egen max = max(ll), by(model sender_type)
	egen min = min(ll), by(model sender_type) 
	gen normal_ll = (ll - min) / (max - min)
	label define sender 0 "Low" 1 "High"
	label values sender_type sender
end
*===============================================================================


*===============================================================================
* run the models

* no reveal
estimate_norms_unobserved if target_cont <=10, reveal(0) lowerbound(0) upperbound(10) filename(norms_results_unobserved_no_reveal) 

* one reveal
** x \in [0,10]
estimate_norms_unobserved if target_cont <=10, reveal(1) lowerbound(0) upperbound(10) filename(norms_results_unobserved_one_reveal_unknown)
** x \in [11,30]
estimate_norms_unobserved if target_cont > 10, reveal(1) lowerbound(11) upperbound(30) filename(norms_results_unobserved_one_reveal_high)

* both reveal
** target low
estimate_norms_unobserved if target_cont <= 10, reveal(2) lowerbound(0) upperbound(10) filename(norms_results_unobserved_both_reveal_low)
** target high
estimate_norms_unobserved if target_cont > 10, reveal(2) lowerbound(11) upperbound(30) filename(norms_results_unobserved_both_reveal_high)
*===============================================================================
