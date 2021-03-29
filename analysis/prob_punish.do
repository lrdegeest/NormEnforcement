*===============================================================================
* Estimate treatment effect on P(punish sombeody)
* produces tests reported in section 4.1
* author: @lrdegeest
*===============================================================================

use ../data/norms_data_estimation.dta, clear
xtset subjectid
levelsof sender_type, local(sender)
foreach s in `sender' {
	di "Endowment == `s'"
	qui xtprobit target_sanction 1.treatment if sender_type == `s', vce(cluster groupid) nolog
	test 1.treatment
	di " "
}

