# %% Load data and packages ---------------
library(tidyverse)
library(ivreg)
library(parallel)
# load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
# load("Code/Products/deconv.RData")
# load("Code/Products/boot_tax_ev_mmt.RData") # Top evading industries
# load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/fs.RData") # Fist stage results
load("Code/Products/run-vars.RData") # Fist stage results for ME

# %%  Setting up folders and vars ----------------
folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"
mc_cores <- detectCores()-2

# select_fs <- grep("log_deductible(_\\w+)+",names(fs_list), value = FALSE) 
select_fs <- grep("log_mats(_\\w+)+",names(fs_list), value = FALSE) 
J = length(select_fs) # Number of industries to estimate
ins_v = c(
    "lag_k",
    "lag_l",
    "lag_m",
    "lag_2_w_eps"
)

# Deductible intermediates
# run_vars_iv<-expand.grid(inds=names(fs_list[11:20]),ins=ins_v, stringsAsFactors = FALSE)
# Raw materials
run_vars_iv<-expand.grid(inds=names(fs_list[select_fs]),ins=ins_v, stringsAsFactors = FALSE)

select_inds <- str_extract(names(fs_list[select_fs]),"\\d+")


## %% Saving variables ---------------------

save(
    run_vars, run_vars_iv,
    file = "Code/Products/run-vars.RData"
)
## %% Estimation ------------------------------------------

### h- AR(1)

# (prod_fun_list_ar1<-mclapply(
#     names(fs_list),
#     estimate_prod_fn,
#     fs_list=fs_list,
#     f=obj_fun_ivar1,
#     ins = "lag_2_w_eps+lag_m",
#     mc.cores = mc_cores
# ))

(prod_fun_list_ivar1<-mcmapply(
    estimate_prod_fn_bounds,
    x = run_vars_iv$inds,
    ins = run_vars_iv$ins,
    MoreArgs = list(
        fs_list=fs_list,
        f=obj_fun_ivar1_bounds
    ),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
))

names(prod_fun_list_ivar1)<-paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"])

## %% Saving prelim results -----------------------------------

save(
    prod_fun_list_ivar1,
    file="Code/Products/deconv_prod_fun.RData"
)

## %% ME results -----------------------------------

(pf_me_list<-mcmapply(
    estimate_prod_fn_bounds,
    x = run_vars_iv$inds,
    ins = run_vars_iv$ins,
    MoreArgs = list(
        fs_list=fs_list_me,
        f=obj_fun_ivar1_bounds
    ),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
))

names(pf_me_list)<-paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"])

## %% Saving results -----------------------------------

save(
    pf_me_list, prod_fun_list_ivar1,
    file="Code/Products/deconv_prod_fun.RData"
)

### h - third degree polynomial

# (prod_fun_list<-mclapply(
#     names(fs_list),
#     estimate_prod_fn,
#     fs_list=fs_list,
#     f=obj_fun_markov,
#     mc.cores = mc_cores
# ))


# (prod_fun_list<-mclapply(
#     names(fs_list),
#     function(x){
#         params<-list(
#             gauss_int=gauss_hermite,
#             epsilon_mu=fs_list[[x]]$epsilon_mu,
#             epsilon_sigma=fs_list[[x]]$epsilon_sigma,
#             beta = fs_list[[x]]$beta
#         )

#         alpha0<-coef(
#             lm(
#                 cal_W ~ k+l,
#                 fs_list[[x]]$data
#             )
#         )

#         res<-optim(
#             alpha0[-1],
#             obj_fun_markov,
#             NULL,
#             fs_list[[x]]$data,
#             params,
#             method = "BFGS",
#             control = list(
#                 maxit = 300
#             )
#         )
#         return(
#             list(
#                 coeffs=c(
#                     m=fs_list[[x]]$beta,
#                     res$par
#                     ),
#                 convergence = res$convergence==0
#                 )
#         )
#     },
#     mc.cores = mc_cores
# ))

# names(prod_fun_list)<-names(fs_list)

## %% Collecting Results ------------------------------

# evasion_tbl<-sapply(
#     names(prod_fun_list),
#     \(x)prod_fun_list[[x]]$coeffs
# ) |> t() |> round(4)

# rownames(evasion_tbl) <- paste0(top_evading_inds[1:5])

"Code/Products/deconv_prod_fun.RData" |> load()

evasion_tbl_ivar1<-sapply(
    names(prod_fun_list_ivar1),
    \(x)prod_fun_list_ivar1[[x]]$coeffs
) |> t() |> round(4)

rownames(evasion_tbl_ivar1) <- paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"]) #paste0(top_evading_inds[1:5])

pf_me_df<-sapply(
    names(pf_me_list),
    \(x)pf_me_list[[x]]$coeffs
) |> t() |> round(4)

rownames(pf_me_df) <- paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"]) #paste0(top_evading_inds[1:5])




## %% Reading Results from Fortran GNR CD to Compare -------------

# Setting up folders and vars ----------------

# %% Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"


(CD_fortran_R <- lapply(
    # union(evasion_inds,gnr_inds),
    # top_10_revenue$sic_3[1:J],
    # top_evading_inds[1:5],
    select_inds,
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_mats_trim_coeffs_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)
)

CD_fortran_tbl_R <- do.call(rbind,CD_fortran_R)
CD_fortran_tbl_R

## %% OLS Results -------------------------------------

(ols_CD <-sapply(
    # top_10_revenue$sic_3[1:J],
    # top_evading_inds[1:5],
    select_inds,
    function(x){
        reg<-test_data %>%
        filter(
            sic_3==x,
            is.finite(y),
            is.finite(k),
            is.finite(l),
            is.finite(m),
            log_mats_share > log(0.05)
        ) %>%
        select(!m) %>%
        mutate(
            m= log(materials)
            # m= log(deductible_intermediates)
        ) %>%
        fixest::feols(
            y~m+k+l, data = .
        )
        return(coef(reg)[c("m","k","l")])
    },
    USE.NAMES = TRUE
)|> t())

rownames(ols_CD)<-paste0(select_inds)

## %% Stata CD GNR + ME Results ----------------------------

read.csv("Code/Products/gnr-cd-me.csv", skip = 1) -> gnr_cd_me
gnr_cd_me
## %% Saving results -----------------------------------

save(
    # prod_fun_list, 
    # evasion_tbl, 
    prod_fun_list_ivar1,
    evasion_tbl_ivar1,
    pf_me_list, pf_me_df,
    CD_fortran_tbl_R, ols_CD,
    gnr_cd_me,
    file="Code/Products/deconv_prod_fun.RData"
)

## %% Comparing Results -----------------------------------

# "Code/Products/deconv_prod_fun.RData" |> load()

PF_tbl<-evasion_tbl_ivar1 %>%
    as.data.frame() %>%
    mutate(
        sic_3 = str_extract(rownames(.),"\\d+"),
        # inter = str_extract(rownames(.),"log_\\w+(_\\w+)*"),
        method = paste("TE-GNR: ",str_extract(rownames(.),"lag_\\w+")
    )) %>%
    rbind(
        CD_fortran_tbl_R %>%
            mutate(
                method = "CD-GNR"
            ) %>%
            select(sic_3=inds,method, m,k,l)
    ) %>%
    bind_rows(
        ols_CD %>%
            as.data.frame() %>%
            mutate(
                sic_3 = rownames(.),
                method = "OLS"
            ) %>%
            select(sic_3,method,m,k,l)
    ) %>%
    pivot_longer(
        cols = c(m,k,l),
        names_to = "input",
        values_to = "coeff"
    ) %>%
    pivot_wider(
        names_from = method,
        values_from = coeff
    ) %>%
    select(
        sic_3, 
        input,
        `TE-GNR:  lag_k`,
        `TE-GNR:  lag_l`,
        `TE-GNR:  lag_m`, 
        `TE-GNR:  lag_2_w_eps`, 
        `CD-GNR`, OLS
    ) #%>%
    # knitr::kable()
PF_tbl #|> View()

PF_me_tbl <- pf_me_df %>%
    as.data.frame() %>%
    mutate(
        sic_3 = str_extract(rownames(.),"\\d+"),
        # inter = str_extract(rownames(.),"log_\\w+(_\\w+)*"),
        method = paste("TE-GNR: ",str_extract(rownames(.),"lag_\\w+")
    )) %>%
    rbind(
        gnr_cd_me %>%
            mutate(
                method = "CD-GNR",
            ) %>%
            select(sic_3,method, m,k,l)
    ) %>%
    bind_rows(
        ols_CD %>%
            as.data.frame() %>%
            mutate(
                sic_3 = rownames(.),
                method = "OLS"
            ) %>%
            select(sic_3,method,m,k,l)
    ) %>%
    pivot_longer(
        cols = c(m,k,l),
        names_to = "input",
        values_to = "coeff"
    ) %>%
    pivot_wider(
        names_from = method,
        values_from = coeff
    ) %>%
    select(
        sic_3, 
        input,
        `TE-GNR:  lag_k`,
        `TE-GNR:  lag_l`,
        `TE-GNR:  lag_m`, 
        `TE-GNR:  lag_2_w_eps`, 
        `CD-GNR`, OLS
    )

PF_me_tbl # |> View()
## %% Collecting 1st stage results Diagnostics ----------------

tsls_1s_diag_tbl<-sapply(
    names(prod_fun_list_ivar1),
    \(x)c(
        ins= str_extract(x,"lag_\\w+"),
        sic_3 = str_extract(x,"\\d+"),
        prod_fun_list_ivar1[[x]]$diagnostics[1,]
        # id=x
    ) #|> t()
) |> t() |> as.data.frame()

tsls_1s_diag_me_tbl<-sapply(
    names(pf_me_list),
    \(x)c(
        ins= str_extract(x,"lag_\\w+"),
        sic_3 = str_extract(x,"\\d+"),
        pf_me_list[[x]]$diagnostics[1,]
        # id=x
    ) #|> t()
) |> t() |> as.data.frame()

## %% Saving results -----------------------------------

save(
    # prod_fun_list, 
    # evasion_tbl, 
    prod_fun_list_ivar1,
    evasion_tbl_ivar1,
    pf_me_list, pf_me_df,
    CD_fortran_tbl_R, ols_CD,
    PF_tbl, PF_me_tbl, gnr_cd_me,
    tsls_1s_diag_tbl, tsls_1s_diag_me_tbl,
    file="Code/Products/deconv_prod_fun.RData"
)

## Reviewing results ----------------------------

# load("Code/Products/deconv_prod_fun.RData")
