/* Estimating CD using GNR(2020). Intermediates are raw materials */


// Renaming variables to use GNR_code_CD.do file

/************************************************************/
/**  Rename variables using the instructions below.        **/ 
/**  Pay attention to upper and lower case, as it          **/
/**  matters for Stata.  Inputs, output, and the share of  **/
/**  intermediate expenditures in total revenue should     **/
/**  all be expressed in levels.  The code will create     **/
/**  the log values.  Also note that input and output      **/
/**  variables are real values, whereas the share is       **/
/**  the nominal share.                                    **/
/**                                                        **/
/**  Firm ID: id                                           **/
/**  Time series variable (e.g., year, month): time        **/
/**  Real gross output: yg_level                           **/
/**  Real labor: l_level                                   **/
/**  Real capital: k_level                                 **/
/**  Real intermediate inputs): i_level                    **/
/**  Nominal Share of intermediates: si_level              **/ 
/**                                                        **/             
/**  Be sure to set the memory larger than what your       **/  
/**  dataset requires, since this code generates new       **/  
/**  variables.  Having too little memory will lead to     **/  
/**  error messages.                                       **/ 
/************************************************************/


// ren rii i_level // intermediates = mats + serv + energy
// ren si si_level // log share of intermediates = log(intermediates/go)

/* Loop through intermediates, mats + serv, and mats */
// ren rmats i_level
// ren mats_share si_level
// ren mats_share_sales si_level
// drop si
// ren si_sales si_level



/* Setting up local variables */


local inds 311 321 352 313 383 // 321 351 352

// local r_inter rii rms rmats rded_i rnded_i
// local inter_shares log_share ms_share mats_share ded_i_share nded_i_share
// local inter_names m_s_e m_s m ded non_ded

local r_inter rii rmats rded_i 
local inter_shares log_share mats_share ded_i_share
local inter_names mse materials dedudctibles

local mat_i 0

// di "cut is `cut' before loop"

foreach ind of local inds {
	
// 	mat li all_inds
	
	cd "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA"
	use gnr-colombia-stata-data if sic_3 == `ind', clear
	
	ren plant id
	ren year time
	ren rgo yg_level
	ren l l_level
	ren rk k_level
	ren si log_share
	
// 	keep if sic_3 == `ind'
	
	
	local wc 0
	
// 	cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"

	cd "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"
	
// 	mat li all_inds
	
// 	di "cut is `cut' before loop 2"
	
	foreach var of local r_inter  {
	
		local ++wc
		local ++mat_i
		local i_share : word `wc' of `inter_shares'
		local name : word `wc' of `inter_names'
		gen si_level = `i_share'
		gen i_level = `var'
		
		di "cut is `cut' before inner-loop"
// 		matrix R_mat`mat_i' = J(1,7,0) //  m,k,l,bigE,mean si_level, mean error, sd error
		

		keep if si_level > $g_cut & l_rgo~=. & l_rk ~=. & l_l ~=. & l_rii~=.
		
		matrix R_mat`mat_i' = J(1,7,0) //  sic_3,inter, cutoff,m,bigE,mean si_level, N_drop
		
		mat R_mat`mat_i'[1,1]=`ind'
		mat R_mat`mat_i'[1,2]=`wc'
		mat R_mat`mat_i'[1,3]=$g_cut
		
// 		mat R_mat`mat_i'[1,4] = r(N_drop)
		local N_drop = r(N_drop)
		
		quietly do GNR_code_CD_fs

// 		sum *elas mexp_eg
//		get m,k,l, bigE, mean error, sd error

		sum ielas
		mat R_mat`mat_i'[1,4] = r(mean)
		mat R_mat`mat_i'[1,7] = `N_drop'/r(N)

// 		mat R_mat`mat_i'[1,2] = beta

		sum mexp_eg
		mat R_mat`mat_i'[1,5] = r(mean)

		sum si_level
		mat R_mat`mat_i'[1,6] = r(mean)

// 		sum eg 
// 		matrix R_mat`mat_i'[1,6] = r(mean)
// 		mat R_mat`mat_i'[1,4] = r(sd)
		
// 		mat li R_mat`mat_i'
		
		matrix all_inds = (nullmat(all_inds)\ R_mat`mat_i')
		
// 		mat li all_inds
		
		local row "`row' sic`ind'-`name'"
		
		di "Rownames: `row'"
		di "Intermediate: `wc' ; row: `mat_i'"
		
		drop si_level-mexp_eg
	}
	
}

matrix colnames all_inds = inds inter cutoff i bigE si_mean N_drop_prc
matrix rownames all_inds = `row'
//
// outtable using "../Products/stata-gnr-inter", mat(all_inds) replace format(%9.4f) center nobox caption("Production function estimates using GNR(2020) for a Cobb-Douglas functional form") 
