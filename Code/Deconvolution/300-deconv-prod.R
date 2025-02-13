# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
# load("Code/Products/deconv.RData")
load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/boot_deconv_mle.RData")

# Data Wrangling ------------------------------------
# colombia_data_frame<-colombia_data_frame %>%
#     mutate(
#         log_ded_share = log(((materials+deductible_expenses)/sales)),
#         log_mats_share = log((materials/sales))
#     )

## Setting up cores----------------------------------

mc_cores <- detectCores()-2
# run_vars <- cbind(
#     inds=rep(order_sic[1:6], each=3),
#     input=c("log_share","log_ded_share","log_mats_share")
# )

## Estimation ------------------------------------------


(prod_fun_list<-mclapply(
    names(fs_list),
    function(x){
        params<-list(
            gauss_int=gauss_hermite,
            epsilon_mu=fs_list[[x]]$epsilon_mu,
            epsilon_sigma=fs_list[[x]]$epsilon_sigma,
            beta = fs_list[[x]]$beta
        )

        alpha0<-coef(
            lm(
                cal_W ~ k+l,
                fs_list[[x]]$data
            )
        )

        res<-optim(
            alpha0[-1],
            obj_fun_markov,
            NULL,
            fs_list[[x]]$data,
            params,
            method = "BFGS",
            control = list(
                maxit = 300
            )
        )
        return(
            list(
                coeffs=c(
                    m=fs_list[[x]]$beta,
                    res$par
                    ),
                convergence = res$convergence==0
                )
        )
    },
    mc.cores = mc_cores
))

names(prod_fun_list)<-names(fs_list)

## Collecting Results ------------------------------

evasion_tbl<-sapply(names(prod_fun_list),\(x)prod_fun_list[[x]]$coeffs) |> t() |> round(4)
rownames(evasion_tbl) <- paste0(top_evading_inds[1:5])
## Reading Results from Fortran GNR CD to Compare -------------

# Setting up folders and vars ----------------

Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"

(CD_fortran_R <- lapply(
    # union(evasion_inds,gnr_inds),
    top_evading_inds[1:5],
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_coeffs_R_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)
)

CD_fortran_tbl_R <- do.call(rbind,CD_fortran_R)
CD_fortran_tbl_R

## OLS Results -------------------------------------

(ols_CD<-sapply(
    top_evading_inds[1:5],
    function(x){
        reg<-colombia_data_frame %>%
        select(!m) %>%
        filter(sic_3==x) %>%
        mutate(
            m= log(materials)
        ) %>%
        fixest::feols(
            y~m+k+l, data = .
        )
        return(coef(reg)[c("m","k","l")])
    },
    USE.NAMES = TRUE
)|> t())

rownames(ols_CD)<-paste0(top_evading_inds[1:5])


## Saving results -----------------------------------

save(
    prod_fun_list, evasion_tbl, CD_fortran_tbl_R, ols_CD,
    file="Code/Products/deconv_prod_fun.RData"
)
