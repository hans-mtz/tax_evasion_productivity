# Load data and packages ---------------
library(tidyverse)
library(ivreg)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
# load("Code/Products/deconv.RData")
load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/boot_deconv_mle.RData")

folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"

# Data Wrangling ------------------------------------
# colombia_data_frame<-colombia_data_frame %>%
#     mutate(
#         log_ded_share = log(((materials+deductible_expenses)/sales)),
#         log_mats_share = log((materials/sales))
#     )

# First Stage Panel ---------------------------------

# fs_list<-mcmapply(
#     first_stage_panel, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
#     run_vars_iv[,"inds"],
#     run_vars_iv[,"input"],
#     MoreArgs = list(data=colombia_data_frame),
#     SIMPLIFY = FALSE,
#     mc.cores = mc_cores
# )

## Testing ------------------------------------------

# load("Code/Products/deconv_prod_fun.RData")

# x<-"311 log_mats_share"

# alpha<-coef(
#         lm(
#             cal_W ~ k+l,
#             fs_list[[x]]$data
#         )
#     )

# alpha <- c(k=0.0719, l=0.4735)

# # lag_m and lag_2_w_eps are stronger instruments than lag_k and lag_l 

# eta<-fs_list[[x]]$data %>% 
#         ungroup() %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - alpha["k"]*k-alpha["l"]*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
#         ) %>%
#         ivreg::ivreg(w_eps~ lag_w_eps|lag_k, data=., na.action = "na.exclude") |>
#         residuals() #|>
#         # summary(diagnostics =TRUE)# |>
#         # str()

# summ_ivreg<-fs_list[[x]]$data %>% 
#         ungroup() %>%
#         filter(
#             is.finite(cal_W),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m)
#         ) %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - alpha["k"]*k-alpha["l"]*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
#         ) %>%
#         ivreg::ivreg(w_eps~ lag_w_eps|lag_k, data=.) |>
#         # residuals() #|>
#         summary(diagnostics =TRUE)# |>
#         # str()

# fs_list[[x]]$data %>% 
#         ungroup() %>%
#         filter(
#             is.finite(cal_W),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m)
#         ) %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - alpha["k"]*k-alpha["l"]*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
#         ) %>%
#         ivreg::ivreg(w_eps~ 1|lag_w_eps|lag_k+lag_l, data=.) |>
#         # residuals() #|>
#         summary(diagnostics =TRUE)# |>

# summ_ivreg$diagnostics |>
#     as.data.frame() %>%
#     mutate(
#         stars = case_when(
#             `p-value` < 0.01 ~ "***",
#             `p-value` < 0.05 ~ "**",
#             `p-value` < 0.1 ~ "*",
#             .default =  ""
#         )
#     )

# fs_list[["351 log_mats_share"]]$data |> 
#     filter(rownames(fs_list[["351 log_mats_share"]]$data) %in% names(eta))

# fs_list[["351 log_mats_share"]]$data %>% 
#         ungroup() %>%
#         arrange(plant,year) %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - alpha["k"]*k-alpha["l"]*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
#         ) |> rownames()

# params<-list(
#         gauss_int=gauss_hermite,
#         epsilon_mu=fs_list[["351 log_mats_share"]]$epsilon_mu,
#         epsilon_sigma=fs_list[["351 log_mats_share"]]$epsilon_sigma,
#         beta = fs_list[["351 log_mats_share"]]$beta
#     )

# obj_fun_ivar1(alpha,fs_list[[x]]$data,params,"lag_k")

# obj_fun_ivar1(c(-3262.87,4199.43),fs_list[[x]]$data,params,"lag_k")

# estimate_prod_fn_bounds(x,fs_list,obj_fun_ivar1_bounds,"lag_k")

# obj_fun_ivar1_bounds<-function(alpha,data,params,ins){
#     a_k <- alpha[1]
#     a_l <- alpha[2]
#     fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

#     df <-data %>% 
#         ungroup() %>%
#         filter(
#             is.finite(cal_W),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m)
#         ) %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - a_k*k-a_l*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
            
#         )

#     eta <- ivreg::ivreg(fml, data=df, na.action = "na.exclude") |>
#         residuals()


#     moments<-apply(
#         df[c("k","l")],
#         2, 
#         function(i){
#         mean(i*eta, na.rm = TRUE)
#         }
#     )

#     obj <- t(moments) %*% moments
#     return(sqrt(obj[1]))

# }

# estimate_prod_fn_bounds<-function(x,fs_list,f,ins){
#     params<-list(
#         gauss_int=gauss_hermite,
#         epsilon_mu=fs_list[[x]]$epsilon_mu,
#         epsilon_sigma=fs_list[[x]]$epsilon_sigma,
#         beta = fs_list[[x]]$beta
#     )

#     alpha0<-coef(
#         lm(
#             cal_W ~ k+l,
#             fs_list[[x]]$data
#         )
#     )

#     res<-optim(
#         alpha0[-1],
#         f,
#         NULL,
#         fs_list[[x]]$data,
#         params,
#         ins,
#         method = "L-BFGS-B",
#         lower = c(0,0),
#         upper = c(1,1),
#         control = list(
#             maxit = 300
#         )
#     )

#     fml <- as.formula(paste0("w_eps ~ lag_w_eps |",ins))

#     ivreg_sum<-fs_list[[x]]$data %>% 
#         ungroup() %>%
#         filter(
#             is.finite(cal_W),
#             is.finite(k),
#             is.finite(l),
#             is.finite(m)
#         ) %>%
#         group_by(plant) %>%
#         mutate(
#             w_eps = cal_W - res$par["k"]*k-res$par["l"]*l,
#             lag_w_eps = lag(w_eps, order_by = year),
#             lag_2_w_eps = lag(w_eps, 2,order_by = year),
#             lag_k = lag(k, order_by = year),
#             lag_l = lag(l, order_by = year),
#             lag_m = lag(m, order_by = year)
            
#         ) %>%
#         ivreg::ivreg(fml, data=.) |>
#         summary(diagnostics =TRUE)

#     return(
#         list(
#             coeffs=c(
#                 m=fs_list[[x]]$beta,
#                 k = res$par[["k"]],
#                 l = res$par[["l"]]
#                 ),
#             convergence = res$convergence,
#             instrument = ins,
#             diagnostics = ivreg_sum$diagnostics |>
#                 as.data.frame() %>%
#                 mutate(
#                     stars = case_when(
#                         `p-value` < 0.01 ~ "***",
#                         `p-value` < 0.05 ~ "**",
#                         `p-value` < 0.1 ~ "*",
#                         .default =  ""
#                     )
#                 )
#             )
#     )
# }


## Setting up cores----------------------------------

mc_cores <- detectCores()-2
# run_vars <- cbind(
#     inds=rep(order_sic[1:6], each=3),
#     input=c("log_share","log_ded_share","log_mats_share")
# )

## Estimation ------------------------------------------

### Declaring Instruments & Running Vars ------------------------------



ins_v = c(
    "lag_k",
    "lag_l",
    "lag_m",
    "lag_2_w_eps"
)

run_vars_iv<-expand.grid(inds=names(fs_list),ins=ins_v, stringsAsFactors = FALSE)


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

## Saving prelim results -----------------------------------

save(
    prod_fun_list_ivar1,
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

## Collecting Results ------------------------------

# evasion_tbl<-sapply(
#     names(prod_fun_list),
#     \(x)prod_fun_list[[x]]$coeffs
# ) |> t() |> round(4)

# rownames(evasion_tbl) <- paste0(top_evading_inds[1:5])

# "Code/Products/deconv_prod_fun.RData" |> load()

evasion_tbl_ivar1<-sapply(
    names(prod_fun_list_ivar1),
    \(x)prod_fun_list_ivar1[[x]]$coeffs
) |> t() |> round(4)

rownames(evasion_tbl_ivar1) <- paste(run_vars_iv[,"inds"],run_vars_iv[,"ins"]) #paste0(top_evading_inds[1:5])




## Reading Results from Fortran GNR CD to Compare -------------

# Setting up folders and vars ----------------

# Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"


(CD_fortran_R <- lapply(
    # union(evasion_inds,gnr_inds),
    top_10_revenue$sic_3[1:5],
    # top_evading_inds[1:5],
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
    top_10_revenue$sic_3[1:5],
    # top_evading_inds[1:5],
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

rownames(ols_CD)<-paste0(top_10_revenue$sic_3[1:5])


## Saving results -----------------------------------

save(
    # prod_fun_list, 
    # evasion_tbl, 
    prod_fun_list_ivar1,
    evasion_tbl_ivar1,
    CD_fortran_tbl_R, ols_CD,
    file="Code/Products/deconv_prod_fun.RData"
)

## Comparing Results -----------------------------------

# "Code/Products/deconv_prod_fun.RData" |> load()

PF_tbl<-evasion_tbl_ivar1 %>%
    as.data.frame() %>%
    mutate(
        sic_3 = str_extract(rownames(.),"\\d+"),
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

## Collecting 1st stage results Diagnostics ----------------

tsls_1s_diag_tbl<-sapply(
    names(prod_fun_list_ivar1),
    \(x)c(
        ins= str_extract(x,"lag_\\w+"),
        sic_3 = str_extract(x,"\\d+"),
        prod_fun_list_ivar1[[x]]$diagnostics[1,]
        # id=x
    ) #|> t()
) |> t() |> as.data.frame()

## Saving results -----------------------------------

save(
    # prod_fun_list, 
    # evasion_tbl, 
    prod_fun_list_ivar1,
    evasion_tbl_ivar1,
    CD_fortran_tbl_R, ols_CD,
    PF_tbl,
    tsls_1s_diag_tbl,
    file="Code/Products/deconv_prod_fun.RData"
)
