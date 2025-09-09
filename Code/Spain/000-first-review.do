cd "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Data/Spain"

use ESEE_2006_2018_1

gen LTU=0
replace LTU=1 if sales > 6000000
sum sales rmaterials routput
gen EV=.
replace EV=0 if sales > 3000000 & sales <=5999999
replace EV=1 if sales >= 7000000
tab industry_labeled LTU

gen mats = rmaterials*pi_materials
gen s = mats*1000/sales
gen nii = pi_intermediate_goods*rintermediates_noRD
gen s1 = nii/(pi_output*routput)

tab industry_labeled EV, sum(s)
by industry_labeled LTU, sort: egen mean_s = mean(s)
by industry_labeled LTU, sort: egen mean_ls = mean(log(s))
gen beta = exp(mean_ls)
tab industry_labeled LTU, sum(beta)

by industry_labeled LTU, sort: egen mean_s = mean(s)
by industry_labeled LTU, sort: egen mean_ls = mean(log(s))
gen beta = exp(mean_ls)
tab industry_labeled LTU, sum(beta)

tab industry_labeled EV, sum(s1)
tab industry_labeled LTU, sum(s1)

/*
sales - nominal
pbs produccion de bienes y servicios

rmaterials rintermediates_noRD
coint materials vp_coint 

*/



