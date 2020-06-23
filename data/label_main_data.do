*==============================================================================
* labeling/formatting the main data (non-targeting data)
* this file produces data_labels.dta which is used to make summary plot and table
* author: @lrdegeest
* ==============================================================================

clear
version 15
use data.dta

//==============================================================================
***** labels/redefinitions
replace endowment = 0 if endowment == 10
replace endowment = 1 if endowment == 30
label define endow 0 "Low" 1 "High" 
label values endowment endow
replace treatment = 0 if treatment == 2
label define treat 0 "Unobserved" 1 "Observed"
label values treatment treat

//==============================================================================
***** subject level data
egen subject_id_string = concat(treatment session subject)
destring subject_id_string, gen(subject_id)
table subject_id 
* 50 obs. each
bysort subject_id: egen subject_total_profit = sum(profit)
bysort subject_id: egen average_contribution = mean(contribute)
bysort subject_id: egen average_pun_sent = mean(admincost)
bysort subject_id: egen average_pun_received = mean(sanctioncost)

//==============================================================================
***** period level data
egen period_id_string = concat(treatment period)
destring period_id_string, gen(period_id)
table period_id 
* 36 in treatment 1 and 40 in treatment 2

bysort period_id: egen average_period_profit = mean(profit)
bysort period_id: egen average_period_contribution = mean(contribute)

bysort period_id: egen average_period_profit_low = mean(profit) if endowment == 0
bysort period_id: egen average_period_contribution_low = mean(contribute) if endowment == 0
bysort period_id: egen average_period_profit_high = mean(profit) if endowment == 1
bysort period_id: egen average_period_contribution_high = mean(contribute)if endowment == 1

bysort period_id: egen ave_period_sanction_sent = mean(admincost)
bysort period_id: egen ave_period_sanction_rec = mean(sanctioncost)

bysort period_id: egen ave_period_sanction_sent_low = mean(admincost) if endowment == 0
bysort period_id: egen ave_period_sanction_rec_low = mean(sanctioncost) if endowment == 0
bysort period_id: egen ave_period_sanction_sent_high = mean(admincost) if endowment == 1
bysort period_id: egen ave_period_sanction_rec_high = mean(sanctioncost) if endowment == 1


//==============================================================================
***** period and group level data
egen group_id_string = concat(treatment session group)
destring group_id_string, gen(group_id)
table group_id 
egen period_group_id = concat(treatment session group period)
table period_group_id
* 4 Obs. in each
bysort period_group_id: egen average_period_group_profit = mean(profit)
bysort period_group_id: egen average_period__group_cont = mean(contribute)
bysort period_group_id: egen average_period_group_profit_low = mean(profit) if endowment == 0
bysort period_group_id: egen average_period_group_cont_low = mean(contribute) if endowment == 0
bysort period_group_id: egen average_period_group_profit_high = mean(profit) if endowment == 1
bysort period_group_id: egen average_period_group_cont_high = mean(contribute)if endowment == 1



//==============================================================================
***** save
save data_labels.dta, replace
