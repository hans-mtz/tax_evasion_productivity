
// Use data from GNR replication files 

use "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado/Replication_files/Tables_2_3/Colombia/311/data_col.dta", clear

keep id year RGO L K RI si export import sizek hiwag adv wcbc
foreach v of varlist * {
	replace `v'=. if `v'<-999
}
replace si=exp(si)
ren year time
ren RGO yg_level
ren L l_level
ren K k_level
ren RI i_level
ren si si_level

sum *level if si~=. & l~=. & k~=. & i~=.

cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"

quietly do GNR_code_CD

sum *elas mexp_eg


reg yg l k i
