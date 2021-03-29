*==============================================================================
* labeling/formatting the data before reshaping it in R
* this file produces data_sanctions_reshape.dta which is then fed to R script for reshaping into targeting data 
* kind of a pain but doing the reshaping in stata is more painful
* author: @lrdegeest
* ==============================================================================

clear
version 15
use ../data/data_labels

replace treatment = 2 if treatment == 1
replace treatment = 1 if treatment == 0
label define treatment 1 "Incomplete" 2 "Complete"
label values treatment treatment

keep	date treatment period subject group indnum endowment contribute session ///
		otherscontribution_* othersendowment_* otherssanction* sanction* 

drop 	sanction sanctioned*
		
order	date session treatment period subject group indnum endowment contribute

rename 	otherscontribution_* cont_*
rename 	othersendowment_* endow_*

drop   	otherssanction_*

gen 	group_id= 100*session + 10*treatment + 1*group
drop 	group

gen 	subject_id = 10*group_id + 1*subject
drop 	subject

order 	group_id date session treatment period subject_id indnum endowment contribute sanctioncost ///
		endow_* cont_* sanction*

sort 	session treatment group_id subject_id period		

save "data_sanctions_reshape.dta", replace
