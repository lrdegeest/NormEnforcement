*===============================================================================
* set up data for norms models
* this file produces norms_data_estimation.dta
* author: @lrdegeest
*===============================================================================

use "format_norms_data/targeting_data.dta", clear

*====================================
* labels
*====================================
* endowment
label drop endowment
replace endowment = 0 if endowment == 1
replace endowment = 1 if endowment == 2
label define endow 0 "Low" 1 "High" 
label values endowment endow
* target_rank
label define rank 1 "First" 2 "Second" 3 "Third"
label values target_rank rank
* target_endow
replace target_endow = 0 if target_endow == 10
replace target_endow = 1 if target_endow == 30
label values target_endow endow

*====================================
* rename
*====================================
rename target_endow target_type
rename endowment sender_type

*====================================
* gen and sort
*====================================
egen mean_contribute = mean(contribute), by(groupid period)

*====================================
* drop and order and check
*====================================
drop date session
order treatment groupid subjectid indnum sender_type period sanctioncost lagsanctioncost
codebook groupid // should be 19 unique
codebook subjectid // should be 74 unique, so there are 74*50*3 = 11400 obs

*====================================
* save
*====================================
save "norms_data_estimation.dta", replace
