# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
library(splines)
library(statmod)
# load("Code/Products/colombia_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/np-deconv-funs.RData")

# load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/deconv_prod_fun.RData")
# load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/fs.RData")
load("Code/Products/run-vars.RData") # Top evading industries
load("Code/Products/bs_mle_data.RData")
## Setting seed for Reproducibility --------------
# set.seed(66636)
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

## %% Deconvoluting \omega from (1-\beta)\varepsilon -----------

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


## %% Saving Preliminary Results ---------------------------------

save(
    # omega_norm_res_list,omega_norm_tbl,
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    # run_vars_iv, inds_in_fs_list,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## %% Non-parametric deconvolution ---------------------

# Don't do all the instruments, select best ones
# Best instruments are m^*_{t-1} and W^*_{t-2}

select_ins <- c(
    "lag_m",
    "lag_2_w_eps"
)

run_vars_pick_iv <- run_vars_iv %>% 
    filter(
        ins %in% select_ins
    )

## %% Saving variables ---------------------

save(
    run_vars, run_vars_iv, run_vars_pick_iv,
    file = "Code/Products/run-vars.RData"
)

## %% Semi-Parametric Deconvolution Function ---------------------

estimate_theta_W(
    fs_list[["369 log_mats_share"]],
    prod_fun_list_ivar1[["369 log_mats_share lag_m"]],
    # eps_pdf_list[["331 log_mats_share"]],
    gl,
    lambda = lambda, parallel = TRUE
)

### All industrires ---------------------

omega_semi_np_deconv_ls <- mcmapply(
  function(x,y){
    cat("Deconvoluting industry:",x,"with instrument:",y,"\n")
    res<-estimate_theta_W(
        fs_list[[x]],
        prod_fun_list_ivar1[[paste(x,y)]],
        gl,
        lambda = lambda, parallel = FALSE
    )
    return(res)
  },
  x = run_vars_pick_iv$inds,
  y = run_vars_pick_iv$ins,
  mc.cores = mc_cores,
  SIMPLIFY = FALSE
)
names(omega_semi_np_deconv_ls)<-paste(run_vars_pick_iv$inds,run_vars_pick_iv$ins)
## %% Saving results ---------------------

save(
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    omega_semi_np_deconv_ls, #omega_full_np_deconv_ls,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## %% Full NP Deconvolution Function ---------------------
# estimate_np_theta_omega(
#     fs_list[["369 log_mats_share"]],
#     prod_fun_list_ivar1[["369 log_mats_share lag_m"]],
#     eps_pdf_list[["369 log_mats_share"]],
#     gl,
#     lambda = lambda, parallel = TRUE
# )

### All industrires ---------------------
omega_full_np_deconv_ls <- mcmapply(
  function(x,y){
    cat("Deconvoluting industry:",x,"with instrument:",y,"\n")
    res <- estimate_np_theta_omega(
        fs_list[[x]],
        prod_fun_list_ivar1[[paste(x,y)]],
        eps_pdf_list[[x]],
        gl,
        lambda = lambda, parallel = FALSE
    )
    return(res)
  },
  x = run_vars_pick_iv$inds,
  y = run_vars_pick_iv$ins,
  mc.cores = mc_cores,
  SIMPLIFY = FALSE
)

names(omega_full_np_deconv_ls)<-paste(run_vars_pick_iv$inds,run_vars_pick_iv$ins)
## %% Saving results ---------------------

save(
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    omega_semi_np_deconv_ls, omega_full_np_deconv_ls,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## %% Get Stats ---------------------

omg_sp_stats_df <- get_stats.list(omega_semi_np_deconv_ls)
omg_sp_stats_df

omg_np_stats_df <- get_stats.list(omega_full_np_deconv_ls)
omg_np_stats_df

## %% Saving results ---------------------

save(
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    omega_semi_np_deconv_ls, omega_full_np_deconv_ls,
    omg_sp_stats_df, omg_np_stats_df,
    file="Code/Products/omega_ar1_deconv_mle.RData"
)

## %% testings  ---------------------

# load("Code/Products/omega_ar1_deconv_mle.RData")
# omega_semi_np_deconv_ls[[1]]
