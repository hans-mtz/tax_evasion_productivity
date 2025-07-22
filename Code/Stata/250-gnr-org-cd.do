/*

This code uses Replication GNR data and the Stata CD GNR Code

*/

clear

clear matrix

local inds 311 321 322 331 381 

foreach ind of local inds {
	
	cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry"
	use data_col, clear
	
	gen id_str = strofreal(id,"%8.0f")
	
	gen sic_3_str = substr(id_str,1,3)
	gen sic_3 = real(sic_3_str)
	
	keep if sic_3 == `ind'
	
// 	cd "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA"
// 	use gnr-colombia-stata-data.dta if sic_3==`ind', clear

	

// 	ren plant id
	ren year time
	ren RGO yg_level
// 	ren rgo yg_level
	ren L l_level
// 	ren l l_level
	ren K k_level
// 	ren rk k_level
	ren RI i_level
// 	ren rii i_level // intermediates = mats + serv + energy
	ren si log_si
	gen si_level = exp(log_si)
// 	ren si si_level // log share of intermediates = log(intermediates/go)
// 	ren ii nom_inter

// 	keep if sic_3 == `ind'
	
// 	cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry/GNR"
	cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"

	do GNR_code_CD

	matrix t_mat`mat_i' = J(1,5,0) //  m,l,k,bigE,err sd, mean si_level, N_drop

	sum ielas
	matrix t_mat`mat_i'[1,1]=r(mean)

	sum lelas
	matrix t_mat`mat_i'[1,3]=r(mean)

	sum kelas
	matrix t_mat`mat_i'[1,2]=r(mean)

	sum mexp_eg
	matrix t_mat`mat_i'[1,4]=r(mean)

	sum eg
// 	t_mat`mat_i'[5,1]=r(mean)
	matrix t_mat`mat_i'[1,5]=r(sd)

	matrix all_inds = (nullmat(all_inds)\ t_mat`mat_i')
	
}

matrix colnames all_inds = m k l bigE err_sd
matrix rownames all_inds = `inds'

// xxx
		
cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Products"

esttab mat(all_inds, fmt(2)) using gnr_org_cd, csv plain replace
