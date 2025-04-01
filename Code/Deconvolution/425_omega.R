# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/deconv_prod_fun.RData")
load("Code/Products/boot_tax_ev_mmt.RData")

## Setting seed for Reproducibility --------------
set.seed(66636)
## Define Variables ---------------------

mc_cores <- detectCores()-2

ins_v = c(
    # "lag_k",
    # "lag_l",
    "lag_m",
    "lag_2_w_eps"
)

run_vars_omega_iv<-expand.grid(inds=names(fs_list),ins=ins_v, stringsAsFactors = FALSE)


## Obtainig omega estimates --------------

## Deconvoluting \omega from (1-\beta)\varepsilon -----------

(omega_norm_ar1_res_list<-mcmapply(
    deconvolute_norm_iv,
    x=run_vars_omega_iv[,"inds"],
    ins=run_vars_omega_iv[,"ins"],
    MoreArgs = list(
        prod_fun_list=prod_fun_list_ivar1,
        fs_list=fs_list
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    mc.cores = mc_cores
))

names(omega_norm_ar1_res_list)<-paste(run_vars_omega_iv[,"inds"],run_vars_omega_iv[,"ins"])

## Collecting Deconvolution Results -------------

(omega_norm_ar1_tbl<-do.call(rbind,omega_norm_ar1_res_list) |> as.data.frame()) 


## Saving Preliminary Results ---------------------------------

save(
    # omega_norm_res_list,omega_norm_tbl,
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    run_vars_omega_iv,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## test 

# load("Code/Products/omega_deconv_mle.RData")

# tst_tbl <- as.data.frame(evasion_tbl)
# tst_tbl$id <- omega_norm_tbl[,"id"]

