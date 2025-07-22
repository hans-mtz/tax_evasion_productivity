
clear

clear matrix
   
// log close

// local today `c(current_date)'
// local day : word 1 of `c(current_date)'
// local month : word 2 of `today'
// local year : word 3 of `today'
cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"

log using 020-loop-6-9-25.smcl, replace

// global trim_cutoff=0.05
global g_cut = 0.05
// do 003-loop
do 020-loop-me

// xxx

cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Products"

// outtable using "../../Paper/images/tables/stata-gnr-inter-trim-05", mat(all_inds) replace format(%9.4f) center nobox caption("Production function estimates using GNR(2020) for a Cobb-Douglas functional form") 

// esttab mat(all_inds, fmt(4)) using stata-gnr-trim, csv plain replace

esttab mat(all_inds, fmt(4)) using gnr-cd-me, csv plain replace

// esttab mat(all_inds, fmt(2)) using gnr_fs_trim_corps, csv plain replace

log close
