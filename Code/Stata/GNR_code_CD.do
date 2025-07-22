/************************************************************/
/**  This code was created to compute the estimator        **/
/**  described in Gandhi, Navarro, and Rivers (2016)       **/
/************************************************************/


/************************************************************/
/**  This code is designed to estimate a gross output      **/
/**  production function with three inputs: capital,       **/
/**  labor, and intermediate inputs                        **/
/**                                                        **/
/**  For a Cobb-Douglas Technology                         **/
/************************************************************/


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


gen yg = ln(yg_level)
gen l = ln(l_level)
gen k = ln(k_level)
gen i = ln(i_level)
gen si = ln(si_level)


/************************************************************/
/**  The code below runs the share regression. This step   **/
/**  recovers the output elasticity of flexible inputs,    **/
/**  which is just a constant for Cobb-Douglas             **/
/************************************************************/

// Obtain initial values from OLS regressions //
regress si if si~=. & l~=. & k~=. & i~=.
matrix test = e(b)
predict temp_c if si~=. & l~=. & k~=. & i~=.
replace temp_c = temp_c - _b[_cons]

//Correct constant term for initial values //
egen mtemp_c = min(temp_c)
scalar ntemp_c=mtemp_c
drop temp_c mtemp_c
scalar ntemp_c=-ntemp_c + 0.1

// Using initial values from OLS, regress shares //
#delimit;
nl ( si = ln({g0=ntemp_c}) ) if si~=. & l~=. & k~=. & i~=., iter(100);
#delimit cr
// Construct predicted elasticities of output with respect to intermediate inputs //
predict ielas if l~=. & k~=. & si~=. & i~=.

// Perform mean adjustment of predicted elasticities //
predict eg if l~=. & k~=. & si~=. & i~=., resid
replace eg=-eg
egen mexp_eg=mean(exp(eg))
replace ielas=ielas-ln(mexp_eg)
replace ielas=exp(ielas)

// xxx
// Rename and save mean adjusted coefficients of estimation //
mat beta=e(b)
svmat double beta
ren beta1 g0

foreach var of varlist g0 {
	egen s`var'=mean(`var')
	drop `var'
	ren s`var' `var'
	replace `var' = `var' / mexp_eg
}	
// clear matrix // Original uncommented

// Compute the integral //
gen integ_G_I = g0
replace integ_G_I=integ_G_I*i 

// Construct script_Y //
gen vg = yg - eg - integ_G_I

// Generate lagged variables //
tset id time
gen vg_1=L.vg
gen l_1=L.l
gen k_1=L.k
gen vg_2=L2.vg
gen l_2=L2.l
gen k_2=L2.k

/************************************************************/
/**  Now to recover the remaining coefficients associated  **/
/**  with capital and labor (the constant of the PDE)      **/
/**                                                        **/
/**  The initial values are set using an OLS regression    **/
/**  of vg on log capital and log labor                    **/
/************************************************************/

// Set initial values using OLS estimation of script_Y on capital, labor //
reg vg l k 
matrix test2 = e(b)
matrix test3 = test2[1,1..2]
matrix test3[1,1] = test2[1,"l"]
matrix test3[1,2] = test2[1,"k"]
matrix drop test2

// Recover capital and labor coefficients using gmm procedure //
// gmm_prod function computes moment conditions using productivity innovation and capital and labor inputs //
gmm gmm_prod_CD if vg!=. & l!=. & k!=. & vg_1!=. & l_1!=. & k_1!=., one nequations(2) parameters(al ak) from(test3) winitial(identity) rhs(vg l k vg_1 l_1 k_1) conv_maxiter(100)


mat beta=e(b)
svmat double beta
ren beta1 al
ren beta2 ak
foreach var of varlist al-ak {
	egen s`var'=mean(`var')
	drop `var'
	ren s`var' `var'
}	

/************************************************************/
/**  Generate productivity in logs and levels, and compute **/
/**  the output elasticities of labor and capital.         **/
/************************************************************/

// generate log productivity //
gen logomega=vg-al*l-ak*k
gen omega=exp(logomega)

// Generate output elasticities of labor and capital inputs //
gen lelas= al
gen kelas= ak 

/************************************************************/
/**  End of the code.                                      **/
/************************************************************/
