
clear

clear matrix
   
// log close

// local today `c(current_date)'
// local day : word 1 of `c(current_date)'
// local month : word 2 of `today'
// local year : word 3 of `today'
cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"

log using 005-loop-5-5-25.smcl, replace

scalar trim_cutoff=0.01
local cutoffs_list 0.01 0.03 0.05


forvalues cut = 0.04(0.01)0.07 {
	
	di `cut'
	
	global g_cut = `cut'
	
	do 005-loop-fs
	
	local cutoff `cut'

	
	matrix res_tbl = (nullmat(res_tbl) , all_inds)
// 	matrix grph_tbl = (nullmat(grph_tbl)\ all_inds)
	
	matrix drop all_inds //R_mat*
	
}

mat A=res_tbl[1...,1..12]

mat li A

	outtable using "../../Paper/images/tables/stata-gnr-inter-ded-trim", mat(A) replace format(%9.4f) center nobox caption("Production function estimates using GNR(2020) for a Cobb-Douglas functional form") 		
//
mat li res_tbl

// mat li grph_tbl

log close


