cd "/Volumes/SSD Hans 1/Dropbox/Dropbox hmarti33/COLOMBIA"
use gnr-colombia-stata-data.dta, clear
// sum si , if sic == 311
// sum si_level, if sic == 311
sum si_level if sic ==311
sum si_level if sic_3 == 311
sum rmats if sic_3 ==311
sum si_level mats_share if sic_3 == 311
 sum si_level mats_share if sic_3 == 311, detail
gen l_si = log(si_level)
reg l_si l_l l_rk l_rii
gen l_mats_share = log(mats_share)
// reg l_mats_share 1
reg l_mats_share if sic_3 == 311
predict epsilon if sic_3 == 311, resid
sum epsilon, detail
gen E_epsilon = exp(epsilon)
// sum E_psilon
sum E_epsilon
sum E_epsilon if sic_3 == 311
// nl ( l_mats_share = log({g0=0.1}) if sic_3 == 311 
nl ( l_mats_share = log({g0=0.1})) if sic_3 == 311 
predict eg if sic_3 ==311, resid
sum eg
sum epsilon eg
nl ( l_mats_share = log({g0=0.1})) if sic_3 == 311 & l_l ~=. & l_rk ~=. & rmats ~=. & rmats > 0
predict eg_2, resid
sum eg_2
sum eg_2 if sic_3 == 311  & l_l ~=. & l_rk ~=. & rmat
// > s ~=. & rmats > 0
sum eg_2 if sic_3 == 311  & l_l ~=. & l_rk ~=. & rmats  ~=. & rmats > 0
sum l_mats_share l_l l_rk rmats if sic_3 ==311
replace eg =-eg 
// egen bigE = exp(eg)
gen bigE = exp(eg)
sum bigE
// sum bigE  sic_3 == 311  & l_l ~=. & l_rk ~=. & rmats  ~=. & rmats > 0
drop bigE
// gen eg_2  = -eg_2
replace eg_2 = -eg_2 /* Zeros make the variance of epsilon blow up when intermediates = only raw materials */
gen bigE = exp(eg_2) if  sic_3 == 311  & l_l ~=. & l_rk ~=. & rmats  ~=. & rmats > 0
sum bigE
sum l_l l_rk rmats l_mats_share if bigE > 100 
sum l_l l_rk rmats l_mats_share if bigE > 100 & bigE ~=.
sum l_l l_rk rmats mats_share if bigE > 100 & bigE ~=.
sum bigE if bigE < 100

/* Trim share < 0.01 0.02 0.05 experiment */
sum l_l l_rk rmats mats_share if mats_share > 0.01
sum l_l l_rk rmats mats_share bigE if mats_share > 0.01
sum l_l l_rk rmats mats_share bigE if mats_share > 0.02
sum l_l l_rk rmats mats_share bigE if mats_share > 0.05
sum l_l l_rk rmats mats_share bigE if mats_share > 0.015
// hist bigE
// hist bigE, bin(1000)
// hist bigE if sic_3 == 311, bin(1000)
// hist bigE if sic_3 == 311 & mats_share < 0.1, bin(1000)
// hist bigE if sic_3 == 311 & mats_share < 0.05, bin(1000)
// hist bigE if sic_3 == 311 & mats_share < 0.03, bin(1000)
// hist bigE if sic_3 == 311 & mats_share < 0.01, bin(1000)
// hist bigE if sic_3 == 311 & mats_share > 0.01, bin(1000)
// hist bigE if sic_3 == 311 & mats_share > 0.1, bin(1000)
// hist bigE if sic_3 == 311 & mats_share > 0.01, bin(1000)
// hist bigE if sic_3 == 311 & mats_share > 0.05, bin(1000)
sum bigE, detail
sum mats_share
scalar mats_share_95 = r(p95)
scalar mats_share_90 = r(p99)
scalar li
// scalar li if sic_3 == 311
sum mats_share if sic_3 == 311
// local mats_share_90 = r(90)
local mats_share_90 = r(p90)
local li
local di
local di mats_share_90
return list
sum mats_share if sic_3 == 311, detail
local mats_share_90 r(p90)
local li
local list
di mats_share_90
return list
scalar mats_share_90 = r(p90)
scalar list
centile mats_share, centile(1(1)5)
centile mats_share if sic_3 == 311, centile(1(1)5)
centile mats_share if sic_3 == 311  & l_l ~=. & l_rk ~=. , centile(1(1)5)
