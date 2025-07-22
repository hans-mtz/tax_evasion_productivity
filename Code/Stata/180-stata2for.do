
clear

clear matrix

local inds 311 321 322 331 381 
local shares log_si lmats_share log_ded_i_share
local inters l_rii l_rmats l_rded_i
local names ints mats deds
local mat_i 0

foreach ind of local inds {
	
	local wc 0
	
	foreach share of local shares{
		
		
		cd "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA"
		use gnr-colombia-stata-data.dta if sic_3==`ind', clear
		sort sic_3 plant year
		
		gen log_si = log(si)
		gen log_ded_i_share = log(ded_i_share)
		gen l_rmats = log(rmats)
		gen l_rded_i = log(rded_i)

		/* Fortran vars should be order by plant and year and in the following order:
		"plant"
		"year"
		"y"
		"s"
		"m"
		"k"
		"l"
		"ll"
		"mm"
		"mk"
		"ml"
		"kk"
		"kl"
		"klm"
		*/
		
		local ++wc
		local ++mat_i
// 		local i_share : word `wc' of `inter_shares'
		local inti : word `wc' of `inters'
		local name : word `wc' of `names'
	
		
		/* Shares */
		
		gen s = `share'
		gen m = `inti'
		
		/* GNR Intermediates*/
		
	// 	gen s=log(si)
	// 	ren l_rii m
		
		ren l_rgo y
		ren l_rk k
		drop l
		ren l_l l
		gen ll=l*l
		gen mm=m*m
		gen mk=m*k
		gen ml=m*l
		gen kk=k*k
		gen kl=k*l
		gen klm=k*l*m
		
		keep plant year y s m k l ll-klm 
		order plant year y s m k l ll-klm
		keep if s~=. & l~=. & k~=. & m~=. & s > log(0.05) // & j_org == 3
		

		


	// 	keep if sic_3 == `ind'
		
	// 	cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry/GNR"
	// 	cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"
		cd "/Volumes/SSD Hans 1/Github/gnr/Data"
		
		export delimited using "stata_data_trim_`name'`ind'.raw", replace delimiter(tab) novarnames nolabel
	// 	do GNR_code

		matrix t_mat`mat_i' = J(2,1,0) //  m,l,k,bigE,err sd, mean si_level, N_drop

		describe 
		matrix t_mat`mat_i'[1,1]=r(N)
		matrix t_mat`mat_i'[2,1]=r(k)

	// 	sum kelas
	// 	matrix t_mat`mat_i'[3,1]=r(mean)
	//
	// 	sum mexp_eg
	// 	matrix t_mat`mat_i'[4,1]=r(mean)

	// 	sum eg
	// 	t_mat`mat_i'[5,1]=r(mean)
	// 	matrix t_mat`mat_i'[5,1]=r(sd)

		matrix all_inds = (nullmat(all_inds), t_mat`mat_i')
		
		local col "`col' `name'`ind'"
	
	}
	
}

matrix colnames all_inds = `col'
matrix rownames all_inds = N k

// xxx
		
// cd  "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Products"

esttab mat(all_inds) using stata_data_trim_info, csv plain replace
