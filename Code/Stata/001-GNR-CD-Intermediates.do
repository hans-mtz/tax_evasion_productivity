/* Estimating CD using GNR(2020). Intermediates are raw materials */
cd "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA"
use gnr-colombia-stata-data.dta, clear

bys sic_3: sum rgo rii rk l si ded_i_share rded_i mats_share if l_rgo~=. & l_rk ~=. & l_l ~=. & l_rii~=.


cd "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Data/Colombia"
save gnr-colombia-stata-data.dta, replace

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

ren plant id
ren year time
ren rgo yg_level
ren l l_level
ren rk k_level
// ren si log_share


ren rii i_level // intermediates = mats + serv + energy
ren si si_level // log share of intermediates = log(intermediates/go)

ren rded_i i_level
ren ded_i_share si_level

/* Loop through intermediates, mats + serv, and mats */
// ren rmats i_level
// ren mats_share si_level
// ren mats_share_sales si_level
// drop si
// ren si_sales si_level


keep if sic_3 == 311

// sum *level

// cd "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"
cd "/Volumes/SSD Hans 1/Github/Tax_Evasion_Productivity/Code/Stata"

do GNR_code_CD

matrix R_mat = J(1,7,0) //  m,k,l,bigE,mean si_level, mean error, sd error

matrix colnames R_mat = m k l bigE si_mean err_mean err_sd
matrix rownames R_mat = 311:

sum ielas
mat R_mat[1,1] = r(mean)

mat R_mat[1,2] = beta

sum mexp_eg
mat R_mat[1,4] = r(mean)

sum si_level
mat R_mat[1,5] = r(mean)

sum eg 
matrix R_mat[1,6] = r(mean)
mat R_mat[1,7] = r(sd)

mat li R_mat
