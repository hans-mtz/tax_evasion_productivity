# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
# load("Code/Products/colombia_data.RData")
load("Code/Products/deconv_funs.Rdata")
# load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/deconv_prod_fun.RData")
# load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/fs.RData")
load("Code/Products/run-vars.RData") # Top evading industries

## Setting seed for Reproducibility --------------
set.seed(66636)
## Define Variables ---------------------

mc_cores <- detectCores()-2

# ins_v = c(
#     # "lag_k",
#     # "lag_l",
#     "lag_m",
#     "lag_2_w_eps"
# )

# # inds_in_fs_list<-grep(paste0("(",paste(top_5_ev_inds,collapse="|"),") log_deductible_\\w+"),names(fs_list), value = TRUE)# |> table()

# inds_in_fs_list<-grep("\\d{3} log_mats_\\w+",names(fs_list), value = TRUE)
# inds_in_fs_list

# # run_vars_iv<-expand.grid(inds=names(fs_list[11:15]),ins=ins_v, stringsAsFactors = FALSE)
# run_vars_iv<-expand.grid(inds=inds_in_fs_list,ins=ins_v, stringsAsFactors = FALSE)



## Obtainig omega estimates --------------

## Deconvoluting \omega from (1-\beta)\varepsilon -----------

(omega_norm_ar1_res_list<-mcmapply(
    deconvolute_norm_iv,
    x=run_vars_iv[,"inds"],
    ins=run_vars_iv[,"ins"],
    MoreArgs = list(
        prod_fun_list=prod_fun_list_ivar1,
        fs_list=fs_list
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    mc.cores = mc_cores
))

names(omega_norm_ar1_res_list)<-paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"])

## Collecting Deconvolution Results -------------

(omega_norm_ar1_tbl<-do.call(rbind,omega_norm_ar1_res_list) |> as.data.frame()) 


## Saving Preliminary Results ---------------------------------

save(
    # omega_norm_res_list,omega_norm_tbl,
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    # run_vars_iv, inds_in_fs_list,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## test 

# load("Code/Products/omega_ar1_deconv_mle.RData")

# tst_tbl <- as.data.frame(evasion_tbl)
# tst_tbl$id <- omega_norm_tbl[,"id"]

