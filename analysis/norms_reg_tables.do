*===============================================================================
* contribution norms regression tables
* this file produces Tables 2 and 3 in the appendix of the manuscript
*===============================================================================

clear
version 15

*===============================================================================
* data
use ../data/norms_data_estimation.dta
gen reveal = 1 if target_cont > 10
egen sum_reveal = sum(reveal), by(subjectid period)

*===============================================================================
* program to run models by treatment and sender-type
capture program drop run_Xtensive
program define run_Xtensive
    version 15
	syntax anything [if]
	quietly {
		preserve
		keep `if'
		xtset subjectid
		levelsof treatment, local(t)
		if `t' == 1 {
			local controls target_cont contribute mean_contribute lagsanctioncost period i.sum_reveal
		}
		else {
			local controls target_cont contribute mean_contribute lagsanctioncost period
		}		
		local norm1 `1'
		gen dev = cond(target_cont - `norm1' > 0, target_cont - `norm1', 0)
		local dev_controls dev c.dev#c.mean_contribute
		eststo `2': xtprobit target_sanction `controls' `dev_controls', re vce(cluster groupid) 
		estadd scalar LL = e(ll)
	}
	if `t' == 1 {
		margins, dydx(sum_reveal)
	}
	drop dev
	restore
end


capture program drop run_INtensive
program define run_INtensive
    version 15
	syntax anything [if]
	quietly {
		preserve
		keep `if'
		xtset subjectid
		levelsof treatment, local(t)
		if `t' == 1 {
			local controls target_cont contribute mean_contribute lagsanctioncost period i.sum_reveal
		}
		else {
			local controls target_cont contribute mean_contribute lagsanctioncost period
		}
		local norm1 `1'
		gen dev = cond(target_cont - `norm1' > 0, target_cont - `norm1', 0)
		local dev_controls dev c.dev#c.mean_contribute
		eststo `2': xtpoisson target_sanction `controls' `dev_controls' if target_sanction > 0, re vce(cluster groupid) iter(1000)
		estadd scalar LL = e(ll)
	}
	if `t' == 1 {
		margins, dydx(sum_reveal)
	}
	drop dev
	restore
end

*===============================================================================
* EXTENSIVE MARGIN

* unobserved, target_cont <= 10
run_Xtensive 9 L0Unknown if treatment == 1 & sender_type == 0 & target_cont <=10
run_Xtensive 1 H0Unknown if treatment == 1 & sender_type == 1 & target_cont <=10
* unobserved, target_cont > 10
run_Xtensive 28 L0H if treatment == 1 & sender_type == 0 & target_cont > 10
run_Xtensive 25 H0H if treatment == 1 & sender_type == 1 & target_cont > 10

* observed
* low sender
run_Xtensive 9 L1L if treatment == 2 & sender_type == 0 & target_type == 0
run_Xtensive 29 L1H if treatment == 2 & sender_type == 0 & target_type == 1
* high sender
run_Xtensive 9 H1L if treatment == 2 & sender_type == 1 & target_type == 0
run_Xtensive 29 H1H if treatment == 2 & sender_type == 1 & target_type == 1
*===============================================================================

*===============================================================================
* INTENSIVE MARGIN

* unobserved, target_cont <=10
run_INtensive 9 L0Unknown2 if treatment == 1 & sender_type == 0 & target_cont <=10
run_INtensive 8 H0Unknown2 if treatment == 1 & sender_type == 1 & target_cont <=10

* unobserved, target_cont > 10
run_INtensive 16 L0H2 if treatment == 1 & sender_type == 0 & target_cont > 10
run_INtensive 17 H0H2 if treatment == 1 & sender_type == 1 & target_cont > 10

* observed
* low sender
run_INtensive 9 L1L2 if treatment == 2 & sender_type == 0 & target_type == 0
run_INtensive 29 L1H2 if treatment == 2 & sender_type == 0 & target_type == 1
* high sender
run_INtensive 9 H1L2 if treatment == 2 & sender_type == 1 & target_type == 0
run_INtensive 28 H1H2 if treatment == 2 & sender_type == 1 & target_type == 1
*===============================================================================

*===============================================================================
* TABLES
*=====================================
* EXTENSIVE MARGIN
esttab	L1L L1H /// observed, Low sender
		H1L H1H /// unobserved, High sender
		L0Unknown H0Unknown /// unobserved, unknown
		L0H H0H /// unobserved, High target		
		using contribution_norms_extensive.tex, replace ///
				cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
				stats(N LL, fmt(0 3) labels("N" "Log-likelihood")) ///
				numbers nodepvars nomtitles booktabs ///
				mgroups("Observed" "Unobserved", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
				label legend  ///
				collabels(none) ///
				drop(0.sum_reveal) ///
				varlabels(_cons Constant target_cont "Target Contribution" ///
						lagsanctioncost "Lagged sanctions" period Period ///
						mean_contribute "Average Contribution" contribute "Contribution" dev "Deviation" ///
						c.dev#c.mean_contribute "Average contribution X Deviation" ///
						1.sum_reveal "One High reveal" 2.sum_reveal "Both High reveal" ///
						)
		
*=====================================
* INTENSIVE MARGIN
esttab L1L2 L1H2 /// observed, Low sender
		H1L2 H1H2 /// unobserved, High sender
		L0Unknown2 H0Unknown2 /// unobserved, unknown
		L0H2 H0H2 /// unobserved, High target		
		using contribution_norms_intensive.tex, replace ///
				cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
				stats(N LL, fmt(0 3) labels("N" "Log-likelihood")) ///
				numbers nodepvars nomtitles booktabs ///
				mgroups("Observed" "Unobserved", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
				label legend  ///
				collabels(none) ///
				drop(0.sum_reveal) ///
				varlabels(_cons Constant target_cont "Target Contribution" ///
						lagsanctioncost "Lagged sanctions" period Period ///
						mean_contribute "Average Contribution" contribute "Contribution" dev "Deviation" ///
						c.dev#c.mean_contribute "Average contribution X Deviation" ///
						1.sum_reveal "One High reveal" 2.sum_reveal "Both High reveal" ///
						)		
