*===============================================================================
* average contributions, payoffs and punishment
* this file produces Table 1 in the manuscript
* author: @lrdegeest
*===============================================================================

clear
version 15
use ../data/data_labels.dta

local vars contribute profit 
foreach v in `vars' {
	table treatment endowment, c(mean `v' sd `v') format(%9.2f) center
}