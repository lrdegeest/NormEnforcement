*===============================================================================
* average contributions, payoffs and punishment (and nonparametric tests)
* this file produces the values in Table 1
* author: @lrdegeest
*===============================================================================

clear
version 15
use ../data/data_labels.dta

* note: 
** unobserved = 0, observed = 1
** low: endowment = 0, high: endowment = 1

*===============================================================================
* 1. averages by treatment-endowment-group

preserve
collapse (mean) contribute=contribute profit=profit admincost=admincost sanctioncost=sanctioncost, by(treatment endowment group_id)

* stats for table 1
local vars contribute profit admincost sanctioncost
foreach v in `vars' {
	di " "
	di "`v'"
	table treatment endowment, c(mean `v' sd `v') format(%9.2f) center
}

* nonparametric tests
local vars contribute profit admincost sanctioncost
levelsof endowment, local(endow)
foreach v in `vars' {
	di "======================"
	di "variable = `v'"
	foreach i in `endow' {
		di ""
		di " endowment = `i'"
		qui ranksum `v' if endowment == `i', by(treatment)
		di "z = ", r(z)
		di "p = ", r(p)
	}
	di ""
}

* check if high sig different from 10 in unobserved
di "High Unobserved sig different from 10?"
qui signrank contribute = 10 if endowment == 1 & treatment == 0
di "z = ", r(z), " p = ", r(p)

restore
*===============================================================================

*===============================================================================
* 2. pooled averages (by treatment-group)

preserve
collapse (mean) contribute=contribute profit=profit admincost=admincost sanctioncost=sanctioncost, by(treatment group_id)

* stats for table 1
local vars contribute profit admincost sanctioncost
foreach v in `vars' {
	di " "
	di "`v'"
	table treatment, c(mean `v' sd `v') format(%9.2f) center
}

* nonparametric tests
local vars contribute profit admincost sanctioncost
foreach v in `vars' {
	di "======================"
	di "variable = `v'"
	qui ranksum `v', by(treatment)
	di "z = ", r(z)
	di "p = ", r(p)
	di ""
}
restore
*===============================================================================
