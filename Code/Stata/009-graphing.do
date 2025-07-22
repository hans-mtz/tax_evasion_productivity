

cd "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"

svmat grph_tbl, names(matcol)

label define intermediates 1 "mse" 2 "materials" 3 "deductibles"
label values grph_tblinter intermediates
label variable grph_tblcutoff "Cutoff"
label var grph_tblbigE "big E"
label var grph_tbli "Elasticity"
label var grph_tblinter "Intermediate"
label var grph_tblN_drop_prc "Dropped (%)"

// twoway connected grph_tblbigE grph_tblcutoff if grph_tblinter == 2 , by(grph_tblinds, title("Trimming by Inds")) // name("Trimming2") //saving("../../Paper/images/graphs/i311.png")  title("311 Inds") 
//
// graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-bigE-inds.png", as(png) replace
//

twoway connected  grph_tblbigE grph_tblcutoff || connected grph_tbli grph_tblcutoff, yaxis(2) || if grph_tblinter == 2, xlabel(,grid) by(grph_tblinds, title("Big E vs Elasticity"))

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-bigE-inds.png", as(png) replace

/* By Industry */

twoway connected  grph_tblbigE grph_tblcutoff || connected grph_tbli grph_tblcutoff, yaxis(2) || if grph_tblinds == 311 & grph_tblinter == 2, xlabel(,grid) title("311-Materials")

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-mats-311.png", as(png) replace

twoway connected  grph_tblbigE grph_tblcutoff || connected grph_tbli grph_tblcutoff, yaxis(2) || if grph_tblinds == 321 & grph_tblinter == 2, xlabel(,grid) title("321-Materials")

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-mats-321.png", as(png) replace

twoway connected  grph_tblbigE grph_tblcutoff || connected grph_tbli grph_tblcutoff, yaxis(2) || if grph_tblinds == 352 & grph_tblinter == 2, xlabel(,grid) title("352-Materials")

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-mats-352.png", as(png) replace

twoway connected  grph_tblbigE grph_tblcutoff || connected grph_tbli grph_tblcutoff, yaxis(2) || if grph_tblinds == 383 & grph_tblinter == 2, xlabel(,grid) title("383-Materials")

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-mats-383.png", as(png) replace

/* Percentage of Observations dropped */

twoway connected grph_tblbigE grph_tblcutoff  || connected grph_tblN_drop_prc grph_tblcutoff, yaxis(2) || if grph_tblinter == 2, xlabel(,grid) by(grph_tblinds, title("Dropped vs Big E"))

graph export "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Paper/images/graphs/trim-drop-all.png", as(png) replace


