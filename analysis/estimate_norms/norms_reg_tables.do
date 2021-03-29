*===============================================================================
* contribution norms regression tables
* this file produces the regression tables in the appendix
*===============================================================================

clear
version 15

*===============================================================================
* data
use ../data/norms_data_estimation.dta
* reveal?
gen reveal = 1 if target_cont > 10
egen sum_reveal = sum(reveal), by(groupid period)

*===============================================================================
* programs to run models by treatment and sender-type

* EXTENSIVE
capture program drop run_Xtensive
program define run_Xtensive
    version 15
	syntax anything [if]
	quietly {
		preserve
		keep `if'
		xtset subjectid
		local controls target_cont contribute mean_contribute lagsanctioncost period		
		local norm1 `1'
		gen dev = cond(target_cont - `norm1' > 0, target_cont - `norm1', 0)
		local dev_controls dev c.dev#c.mean_contribute
		eststo `2': xtprobit target_sanction `controls' `dev_controls', re vce(cluster groupid) iter(1000)
		estadd scalar LL = e(ll)
	}
	drop dev
	restore
end

* INTESIVE
capture program drop run_INtensive
program define run_INtensive
    version 15
	syntax anything [if]
	quietly {
		preserve
		keep `if'
		xtset subjectid
		local controls target_cont contribute mean_contribute lagsanctioncost period
		local norm1 `1'
		gen dev = cond(target_cont - `norm1' > 0, target_cont - `norm1', 0)
		local dev_controls dev c.dev#c.mean_contribute
		eststo `2': xtpoisson target_sanction `controls' `dev_controls' if target_sanction > 0, re vce(cluster groupid) iter(1000)
		estadd scalar LL = e(ll)
	}
	drop dev
	restore
end

*===============================================================================
* EXTENSIVE MARGIN (0)

* UNOBSERVED
** no high reveal (sum_reveal == 0)
*** low
run_Xtensive 8 Low_0_NoHighReveal if treatment == 1 & sender_type == 0 & sum_reveal == 0
*** high
run_Xtensive 1 High_0_NoHighReveal if treatment == 1 & sender_type == 1 & sum_reveal == 0

** one high reveal (sum_reveal == 3)
*** low, unknown
run_Xtensive 8 Low_0_OneHighReveal_Unknown if treatment == 1 & sender_type == 0 & sum_reveal == 3 & target_cont <=10
*** low, high
run_Xtensive 20 Low_0_OneHighReveal_High if treatment == 1 & sender_type == 0 & sum_reveal == 3 & target_cont > 10
*** high, unknown
run_Xtensive 1 High0_OneHighReveal_Unknown if treatment == 1 & sender_type == 1 & sum_reveal == 3 & target_cont <=10
*** high, high
**** NA

** both high reveal (sum_reveal == 6)
*** low, low
run_Xtensive 1 Low_0_BothHighReveal_Low if treatment == 1 & sender_type == 0 & sum_reveal == 6 & target_cont <=10
*** low, high
run_Xtensive 20 Low_0_BothHighReveal_High if treatment == 1 & sender_type == 0 & sum_reveal == 6 & target_cont > 10
*** high, low
run_Xtensive 2 High_0_BothHighReveal_Low if treatment == 1 & sender_type == 1 & sum_reveal == 6 & target_cont <=10
*** high, high
run_Xtensive 25 High_0_BothHighReveal_High if treatment == 1 & sender_type == 1 & sum_reveal == 6 & target_cont > 10

** both high reveal

* OBSERVED
** low sender
run_Xtensive 9 L0L if treatment == 2 & sender_type == 0 & target_type == 0
run_Xtensive 29 L0H if treatment == 2 & sender_type == 0 & target_type == 1
* high sender
run_Xtensive 9 H0L if treatment == 2 & sender_type == 1 & target_type == 0
run_Xtensive 29 H0H if treatment == 2 & sender_type == 1 & target_type == 1
*===============================================================================

*===============================================================================
* INTENSIVE MARGIN (1)

* UNOBSERVED
** no high reveal
*** low
run_INtensive 9 Low_1_NoHighReveal if treatment == 1 & sender_type == 0 & sum_reveal == 0
*** high
run_INtensive 9 High_1_NoHighReveal if treatment == 1 & sender_type == 1 & sum_reveal == 0

** one high reveal
*** low, unknown
run_INtensive 8 Low_1_OneHighReveal_Unknown if treatment == 1 & sender_type == 0 & sum_reveal == 3 & target_cont <=10
*** low, high
***** note: norms 20-24 were identical
run_INtensive 20 Low_1_OneHighReveal_High if treatment == 1 & sender_type == 0 & sum_reveal == 3 & target_cont > 10
*** high, unknown
run_INtensive 7 High1_OneHighReveal_Unknown if treatment == 1 & sender_type == 1 & sum_reveal == 3 & target_cont <=10
*** high, high
**** NA


** both high reveal (sum_reveal == 6)
*** low, low
run_INtensive 6 Low_1_BothHighReveal_Low if treatment == 1 & sender_type == 0 & sum_reveal == 6 & target_cont <=10
*** low, high
run_INtensive 17 Low_1_BothHighReveal_High if treatment == 1 & sender_type == 0 & sum_reveal == 6 & target_cont > 10
*** high, low
run_INtensive 8 High_1_BothHighReveal_Low if treatment == 1 & sender_type == 1 & sum_reveal == 6 & target_cont <=10
*** high, high
**** NA


* OBSERVED
** low sender
run_INtensive 9 L1L if treatment == 2 & sender_type == 0 & target_type == 0
run_INtensive 29 L1H if treatment == 2 & sender_type == 0 & target_type == 1
** high sender
run_INtensive 9 H1L if treatment == 2 & sender_type == 1 & target_type == 0
run_INtensive 28 H1H if treatment == 2 & sender_type == 1 & target_type == 1
*===============================================================================

*===============================================================================
* TABLES

* EXTENSIVE MARGIN
esttab	L0L L0H /// observed, Low sender
		H0L H0H /// unobserved, High sender
		Low_0_NoHighReveal High_0_NoHighReveal /// unobserved, no high reveal
		Low_0_OneHighReveal_Unknown Low_0_OneHighReveal_High High0_OneHighReveal_Unknown /// unobserved, one high reveal
		Low_0_BothHighReveal_Low Low_0_BothHighReveal_High High_0_BothHighReveal_Low High_0_BothHighReveal_High /// unobserved, both high reveal
		using norms_extensive_revision.tex, replace ///
			cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
			numbers nodepvars mtitles booktabs ///
			mgroups("Observed" "No High Reveal" "One High Reveal" "Both High Reveal", /// 
				pattern(1 0 0 0 1 0 1 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			label legend  ///
			collabels(none) ///
			varlabels(_cons Constant target_cont "Target Contribution" ///
						lagsanctioncost "Lagged sanctions" period Period ///
						mean_contribute "Average Contribution" contribute "Contribution" dev "Deviation" ///
						c.dev#c.mean_contribute "Average contribution X Deviation")

* INTENSIVE MARGIN
esttab	L1L L1H /// observed, Low sender
		H1L H1H /// unobserved, High sender
		Low_1_NoHighReveal High_1_NoHighReveal /// unobserved, no high reveal
		Low_1_OneHighReveal_Unknown Low_1_OneHighReveal_High High1_OneHighReveal_Unknown /// unobserved, one high reveal
		Low_1_BothHighReveal_Low Low_1_BothHighReveal_High High_1_BothHighReveal_Low /// unobserved, both high reveal
		using norms_intensive_revision.tex, replace ///
			cells(b(star fmt(3)) se(par fmt(2))) star(* 0.10 ** 0.05 *** 0.01) ///
			numbers nodepvars mtitles booktabs ///
			mgroups("Observed" "No High Reveal" "One High Reveal" "Both High Reveal", /// 
				pattern(1 0 0 0 1 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			label legend  ///
			collabels(none) ///
			varlabels(_cons Constant target_cont "Target Contribution" ///
						lagsanctioncost "Lagged sanctions" period Period ///
						mean_contribute "Average Contribution" contribute "Contribution" dev "Deviation" ///
						c.dev#c.mean_contribute "Average contribution X Deviation")	
*===============================================================================
