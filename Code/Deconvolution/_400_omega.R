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

## Obtainig omega estimates --------------

# Productivity = exp(omega+varepsilon)
# 1 Get W_squiggle = big_W - alpha*[k,l] = omega+(1-\beta)varepsilon
# 2 Deconvolute \int f_{\omega}(W_squiggle -(1-\beta)*\varepsilon))f_{\varepsilon}(\varepsilon)d\varepsilon


## Testing --------------------------
# alpha <- prod_fun_list[["322 log_mats_share"]]$coeffs
# df<-fs_list[["322 log_mats_share"]]$data %>%
#     filter(
#         is.finite(cal_W),
#         is.finite(k),
#         is.finite(l)
#     ) %>%
#     mutate(
#         W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l
#     )
# fixest::feols(W_squiggle~1,df)
# params<-list(
#         gauss_int=gauss_hermite,
#         epsilon_mu=fs_list[["322 log_mats_share"]]$epsilon_mu,
#         epsilon_sigma=fs_list[["322 log_mats_share"]]$epsilon_sigma
#     )

# init<-c(
#     mu=mean(df$W_squiggle, na.rm = TRUE), 
#     sigma=1.3^(sd(df$W_squiggle, na.rm = TRUE))
# )

# f_w_n(0,0,params$epsilon_mu,params$epsilon_sigma,alpha[["m"]])
# eval_llh_fun(0,f_w_n,0,1,params,alpha[["m"]])
# obj_fun_f(
#     init,
#     f_w_n,
#     df$W_squiggle,
#     params,
#     alpha[["m"]]
# )
# res<-optim(
#     init,
#     obj_fun_f,
#     NULL,
#     f_w_n,
#     df$W_squiggle,
#     params,
#     alpha[["m"]],
#     method = "BFGS",
#     control=list(fnscale=-1) #Maximizing instead of minimizing
# )
# res

## Deconvoluting \omega from (1-\beta)\varepsilon -----------

(omega_norm_res_list<-mclapply(
    names(fs_list),
    deconvolute_norm,
    prod_fun_list=prod_fun_list,
    fs_list=fs_list,
    mc.cores = mc_cores
))

names(omega_norm_res_list)<-names(fs_list)

## Collecting Deconvolution Results -------------

(omega_norm_tbl<-do.call(rbind,omega_norm_res_list))

## Deconvoluting \omega from (1-\beta)\varepsilon -----------

(omega_norm_ar1_res_list<-mclapply(
    names(fs_list),
    deconvolute_norm,
    prod_fun_list=prod_fun_list_ar1,
    fs_list=fs_list,
    mc.cores = mc_cores
))

names(omega_norm_ar1_res_list)<-names(fs_list)

## Collecting Deconvolution Results -------------

(omega_norm_ar1_tbl<-do.call(rbind,omega_norm_ar1_res_list))


## Saving Preliminary Results ---------------------------------

save(
    omega_norm_res_list,omega_norm_tbl,
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    file="Code/Products/omega_deconv_mle.RData"
)

## test 

# load("Code/Products/omega_deconv_mle.RData")

# tst_tbl <- as.data.frame(evasion_tbl)
# tst_tbl$id <- omega_norm_tbl[,"id"]

## Bootstrapping Prod Fun and Productivity -------------------------------------

col_df <- colombia_data_frame %>%
    select(
        sic_3, log_mats_share, juridical_organization, 
        gross_output, year, plant, k, l, y,
        log_sales
    )

boot_pf_list <- mclapply(
    1:1,
    function(i){
        resampled_data<-resample_by_group(col_df,sic_3)
        ### First Stage -------------------------
        fs_list_temp<-mapply(
            first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
            run_vars[,"inds"],
            run_vars[,"input"],
            MoreArgs = list(data=resampled_data),
            SIMPLIFY = FALSE#,
            # mc.cores = mc_cores
        )

        names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])

        ## Estimating Prod Fun ------------------------------------------

        ### h- AR(1)

        # prod_fun_ar1_temp<-lapply(
        #     names(fs_list_temp),
        #     estimate_prod_fn,
        #     fs_list=fs_list_temp,
        #     f=obj_fun_ar1#,
        #     # mc.cores = mc_cores
        # )

        # names(prod_fun_ar1_temp)<-names(fs_list_temp)

        # pf_tbl_ar1_temp<-sapply(
        #     names(prod_fun_ar1_temp),
        #     \(x)c(prod_fun_ar1_temp[[x]]$coeffs,id=x,markov="AR(1)")
        # ) |> t()

        ### h - third degree polynomial

        prod_fun_temp<-lapply(
            names(fs_list_temp),
            estimate_prod_fn,
            fs_list=fs_list_temp,
            f=obj_fun_markov#,
            # mc.cores = mc_cores
        )

        names(prod_fun_temp)<-names(fs_list_temp)
        
        pf_tbl_temp<-sapply(
            names(prod_fun_temp),
            \(x)c(prod_fun_temp[[x]]$coeffs,id=x,markov="3DG Poly")
        ) |> t()
       
       ## Estimating Productivity -----------------------
        # #### AR(1)
        # omega_ar1_temp<-lapply(
        # names(fs_list_temp),
        # deconvolute_norm,
        # prod_fun_list=prod_fun_ar1_temp,
        # fs_list=fs_list_temp#,
        # # mc.cores = mc_cores
        # )

        # ## Collecting Deconvolution Results -------------
        # omega_ar1_tbl_temp<-do.call(rbind,omega_ar1_temp)
        # omega_ar1_tbl_temp$markov <-"AR(1)"

        ### 3rd Degree Polynomial 
        omega_temp<-lapply(
        names(fs_list_temp),
        deconvolute_norm,
        prod_fun_list=prod_fun_temp,
        fs_list=fs_list_temp#,
        # mc.cores = mc_cores
        )

        ## Collecting Deconvolution Results -------------
        omega_tbl_temp<-do.call(rbind,omega_temp)
        # omega_tbl_temp$markov <- "3DG Poly"

        # res_tbl <- merge(
        #     rbind(pf_tbl_ar1_temp,pf_tbl_temp) |> as.data.frame(),
        #     rbind(omega_ar1_tbl_temp,omega_tbl_temp) |> as.data.frame()
        # )

        res_tbl <- data.frame(
            pf_tbl_temp,
            omega_tbl_temp
        )
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(res_tbl)

    },
    mc.cores = mc_cores
)

load("Code/Products/omega_deconv_mle.RData")
## Saving Preliminary Results ---------------------------------

save(
    omega_norm_res_list,omega_norm_tbl,
    omega_norm_ar1_res_list,omega_norm_ar1_tbl,
    boot_pf_list,
    file="Code/Products/omega_deconv_mle.RData"
)