## %% Load Libraries ---------------------------------------------------------------
library(tidyverse)
library(parallel)
# load("Code/Products/colombia_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/run-vars.RData") # Top evading industries
load("Code/Products/fs.RData") # Top evading industries
load("Code/Products/boot_fs.RData")
# load("Code/Products/boot_tax_ev_mmt.RData")
# load("Code/Products/omega_ar1_deconv_mle.RData")
# load("Code/Products/deconv_prod_fun.RData")
# load("Code/Products/boot_deconv_mle.RData")

## %% Setting seed for Reproducibility --------------
set.seed(66636)
# J = 10 # Number of Top Revenue Industries 1<=J<= 20
R = 250 # Number of Bootstrap Replicates
## Define Variables ---------------------

mc_cores <- detectCores()-2
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
# top_evading_inds <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)
# run_vars<-expand.grid(inds=top_evading_inds[1:5],input="log_mats_share", stringsAsFactors = FALSE)

# run_vars$r_input <- rep(c("materials","deductible_intermediates"), each=J)
# # run_vars$r_input <- "deductible_intermediates"

# pick_rows <- run_vars$inds %in% top_5_ev_inds

## %% Bootstrapping Prod Fun and Productivity -------------------------------------

# col_df <- colombia_data_frame %>%
#     select(
#         sic_3, log_mats_share, juridical_organization, 
#         gross_output, year, plant, k, l, m, y,
#         log_sales, log_deductible_intermediates_share,
#         deductible_intermediates, materials
#     )


# boot_fs_list<-mclapply(
#     1:R,
#     function(i){
#     resampled_data<-resample_by_group(col_df,sic_3)
#     ### First Stage -------------------------
#     fs_list_temp<-mapply(
#         first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
#         run_vars[pick_rows,"inds"],
#         run_vars[pick_rows,"input"],
#         run_vars[pick_rows,"r_input"],
#         MoreArgs = list(data=resampled_data),
#         SIMPLIFY = FALSE#,
#         # mc.cores = mc_cores
#     )

#     names(fs_list_temp)<-paste(run_vars[c(11:14,20),"inds"],run_vars[c(11:14,20),"input"])
#     return(fs_list_temp)
#     },
#     mc.cores = mc_cores
# )

# save(
#     boot_fs_list,
#     file="Code/Products/boot_pf_prod.RData"
# )


# col_df <- colombia_data_frame %>%
#     select(
#         sic_3, log_mats_share, juridical_organization, 
#         gross_output, year, plant, k, l, y, m,
#         log_sales, log_deductible_intermediates_share,
#         deductible_intermediates, materials
#     )
## %% Using Boot FS List from Boot MLE -------------------

# load("Code/Products/boot_deconv_mle.RData")

# lapply(
#     seq_along(boot_res_list),
#     function(x){
#         boot_res_list[[x]]$fs_list
#     }
# ) -> boot_fs_list


boot_pf_list <- mclapply(
    1:R,
    function(i){
        # resampled_data<-resample_by_group(col_df,sic_3)
        # ### First Stage -------------------------
        # fs_list_temp<-mapply(
        #     first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
        #     run_vars[,"inds"],
        #     run_vars[,"input"],
        #     MoreArgs = list(data=resampled_data),
        #     SIMPLIFY = FALSE#,
        #     # mc.cores = mc_cores
        # )

        # names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])

        # if(i %% 20==0){cat("FS done — bootstrap replicate:",i,"\n")}

        ## Estimating Prod Fun ------------------------------------------
        fs_list_temp <- boot_fs_list[[i]]
        # names(fs_list_temp)<-paste(run_vars[pick_rows,"inds"],run_vars[pick_rows,"input"])

        ### h- AR(1)

        prod_fun_list_ivar1_temp<-mapply(
            estimate_prod_fn_bounds,
            x = run_vars_pick_iv$inds,
            ins = run_vars_pick_iv$ins,
            MoreArgs = list(
                fs_list=fs_list_temp,
                f=obj_fun_ivar1_bounds
            ),
            SIMPLIFY = FALSE#,
            # mc.cores = mc_cores
        )

        names(prod_fun_list_ivar1_temp)<-paste(run_vars_pick_iv[,"inds"],run_vars_pick_iv[,"ins"])


        pf_tbl_ivar1_temp<-sapply(
            names(prod_fun_list_ivar1_temp),
            \(x)c(prod_fun_list_ivar1_temp[[x]]$coeffs,id=x,markov="AR(1)")
        ) |> t()

        if(i %% 20==0){cat("PF done — bootstrap replicate:",i,"\n")}

        ### h - third degree polynomial

        # prod_fun_temp<-lapply(
        #     names(fs_list_temp),
        #     estimate_prod_fn,
        #     fs_list=fs_list_temp,
        #     f=obj_fun_markov#,
        #     # mc.cores = mc_cores
        # )

        # names(prod_fun_temp)<-names(fs_list_temp)
        
        # pf_tbl_temp<-sapply(
        #     names(prod_fun_temp),
        #     \(x)c(prod_fun_temp[[x]]$coeffs,id=x,markov="3DG Poly")
        # ) |> t()
       
       # Estimating Productivity -----------------------
        #### AR(1)

        omega_ar1_list_temp<-mapply(
            deconvolute_norm_iv,
            x=run_vars_pick_iv[,"inds"],
            ins=run_vars_pick_iv[,"ins"],
            MoreArgs = list(
                prod_fun_list=prod_fun_list_ivar1_temp,
                fs_list=fs_list_temp
            ),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
            # mc.cores = mc_cores
        )
        omega_ar1_list_temp
        ## Collecting Deconvolution Results -------------

        names(omega_ar1_list_temp)<-paste(run_vars_pick_iv[,"inds"],run_vars_pick_iv[,"ins"])

        ## Collecting Deconvolution Results -------------

        omega_tbl_temp<-do.call(rbind,omega_ar1_list_temp) |> as.data.frame()

        if(i %% 20==0){cat("Omega done — bootstrap replicate:",i,"\n")}

        res_tbl <- data.frame(
            # pf_tbl_temp,
            pf_tbl_ivar1_temp,
            omega_tbl_temp
        )
        if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
        return(res_tbl)

    },
    mc.cores = mc_cores
)

# load("Code/Products/omega_deconv_mle.RData")
## Saving Preliminary Results ---------------------------------

save(
    boot_pf_list, boot_fs_list,
    file="Code/Products/boot_pf_prod.RData"
)

## Collecting Results --------------------------------------------`3

# load("Code/Products/boot_pf_prod.RData")
# load("Code/Products/deconv_prod_fun.RData")
# load("Code/Products/omega_ar1_deconv_mle.RData")



boot_pf_tbl<-do.call(rbind,boot_pf_list) |> 
    as.data.frame() %>% 
    mutate(
        across(
            !c(id,markov,sic_3, ins, dist),
            as.numeric
        )
    ) %>%
    left_join(
        as.data.frame(evasion_tbl_ivar1) %>%
            ungroup() %>%
            mutate(
                sic_3 = stringr::str_extract(rownames(.),"\\d{3}"),
                ins = stringr::str_extract(rownames(.),"lag_\\w?(_w_eps)?")
            ),
        by = c("sic_3","ins"),
        suffix = c("",".t0")
    ) %>%
    left_join(
        as.data.frame(omega_norm_ar1_tbl) %>%
            mutate(
                across(
                    !sic_3:dist,
                    as.numeric
                )
            ),
        by = c("sic_3","ins"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_boot_m = m - m.t0,
        bc_boot_k = k - k.t0,
        bc_boot_l = l - l.t0,
        bc_boot_mu = mu- mu.t0,
        bc_boot_sigma = sigma - sigma.t0 
    ) %>%
    group_by(sic_3,ins) %>%
    reframe(
        value_m = quantile(bc_boot_m, probs = c(0.975, 0.025)),
        value_k = quantile(bc_boot_k, probs = c(0.975, 0.025)),
        value_l = quantile(bc_boot_l, probs = c(0.975, 0.025)),
        value_mu = quantile(bc_boot_mu, probs = c(0.975, 0.025)),
        value_sigma = quantile(bc_boot_sigma, probs = c(0.975, 0.025)),
        probs = c(0.975, 0.025),
        CI = c("LCI","UCI"),
        CI_m = max(m.t0) - value_m,
        CI_k = max(k.t0) - value_k,
        CI_l = max(l.t0) - value_l,
        CI_mu = max(mu.t0) - value_mu,
        CI_sigma = max(sigma.t0) - value_sigma,
        m = max(m.t0),
        k = max(k.t0),
        l = max(l.t0),
        mu = max(mu.t0),
        sigma = max(sigma.t0)
    ) %>%
    select(
        sic_3, ins, CI,CI:CI_sigma, m:sigma
    ) %>%
    pivot_wider(
        names_from = CI,
        values_from = c(CI_m:CI_sigma)
    ) 

boot_pf_tbl<-boot_pf_tbl %>%
    mutate(
        across(
            !c(sic_3, ins),
            ~ round(.x, 4)
        ),
        across(
            m:sigma,
            ~ glue::glue("{x}", x=.x),
            .names = "coef_{col}"
        ),
        CI_m = glue::glue("[{CI_m_LCI}, {CI_m_UCI}]"),
        CI_k = glue::glue("[{CI_k_LCI}, {CI_k_UCI}]"),
        CI_l = glue::glue("[{CI_l_LCI}, {CI_l_UCI}]"),
        CI_mu = glue::glue("[{CI_mu_LCI}, {CI_mu_UCI}]"),
        CI_sigma = glue::glue("[{CI_sigma_LCI}, {CI_sigma_UCI}]")
    ) %>%
    select(
        sic_3, ins,
        coef_m, CI_m,
        coef_k, CI_k,
        coef_l, CI_l,
        coef_mu, CI_mu,
        coef_sigma, CI_sigma    
    ) %>%
    pivot_longer(
        cols = starts_with(c("coef","CI")),
        names_to = c("type", "var"),
        names_pattern = "^(coef|CI)_(.*)$",
        values_to = "value" 
    ) %>%
    pivot_wider(
        names_from = var,
        values_from = value
    )




boot_pf_tbl_90<-do.call(rbind,boot_pf_list) |> 
    as.data.frame() %>% 
    mutate(
        across(
            !c(id,markov,sic_3, ins, dist),
            as.numeric
        )
    ) %>%
    left_join(
        as.data.frame(evasion_tbl_ivar1) %>%
            ungroup() %>%
            mutate(
                sic_3 = stringr::str_extract(rownames(.),"\\d{3}"),
                ins = stringr::str_extract(rownames(.),"lag_\\w?(_w_eps)?")
            ),
        by = c("sic_3","ins"),
        suffix = c("",".t0")
    ) %>%
    left_join(
        as.data.frame(omega_norm_ar1_tbl) %>%
            mutate(
                across(
                    !sic_3:dist,
                    as.numeric
                )
            ),
        by = c("sic_3","ins"),
        suffix = c("", ".t0")
    ) %>%
    mutate(
        bc_boot_m = m - m.t0,
        bc_boot_k = k - k.t0,
        bc_boot_l = l - l.t0,
        bc_boot_mu = mu- mu.t0,
        bc_boot_sigma = sigma - sigma.t0 
    ) %>%
    group_by(sic_3,ins) %>%
    reframe(
        value_m = quantile(bc_boot_m, probs = c(0.95, 0.05)),
        value_k = quantile(bc_boot_k, probs = c(0.95, 0.05)),
        value_l = quantile(bc_boot_l, probs = c(0.95, 0.05)),
        value_mu = quantile(bc_boot_mu, probs = c(0.95, 0.05)),
        value_sigma = quantile(bc_boot_sigma, probs = c(0.95, 0.05)),
        probs = c(0.95, 0.05),
        CI = c("LCI","UCI"),
        CI_m = max(m.t0) - value_m,
        CI_k = max(k.t0) - value_k,
        CI_l = max(l.t0) - value_l,
        CI_mu = max(mu.t0) - value_mu,
        CI_sigma = max(sigma.t0) - value_sigma,
        m = max(m.t0),
        k = max(k.t0),
        l = max(l.t0),
        mu = max(mu.t0),
        sigma = max(sigma.t0)
    ) %>%
    select(
        sic_3, ins, CI,CI:CI_sigma, m:sigma
    ) %>%
    pivot_wider(
        names_from = CI,
        values_from = c(CI_m:CI_sigma)
    ) 

boot_pf_tbl_90<-boot_pf_tbl_90 %>%
    mutate(
        across(
            !c(sic_3, ins),
            ~ round(.x, 4)
        ),
        across(
            m:sigma,
            ~ glue::glue("{x}", x=.x),
            .names = "coef_{col}"
        ),
        CI_m = glue::glue("[{CI_m_LCI}, {CI_m_UCI}]"),
        CI_k = glue::glue("[{CI_k_LCI}, {CI_k_UCI}]"),
        CI_l = glue::glue("[{CI_l_LCI}, {CI_l_UCI}]"),
        CI_mu = glue::glue("[{CI_mu_LCI}, {CI_mu_UCI}]"),
        CI_sigma = glue::glue("[{CI_sigma_LCI}, {CI_sigma_UCI}]")
    ) %>%
    select(
        sic_3, ins,
        coef_m, CI_m,
        coef_k, CI_k,
        coef_l, CI_l,
        coef_mu, CI_mu,
        coef_sigma, CI_sigma    
    ) %>%
    pivot_longer(
        cols = starts_with(c("coef","CI")),
        names_to = c("type", "var"),
        names_pattern = "^(coef|CI)_(.*)$",
        values_to = "value" 
    ) %>%
    pivot_wider(
        names_from = var,
        values_from = value
    )

## Saving Final Results ---------------------------------

save(
    boot_pf_list, boot_pf_tbl, boot_fs_list,
    boot_pf_tbl_90,
    file="Code/Products/boot_pf_prod.RData"
)

## Testing ----------------------------------
load("Code/Products/boot_pf_prod.RData")

boot_pf_tbl_90
# boot_pf_tbl %>%
#     group_by(sic_3,ins) %>%
#     select(mu, sigma)

# prod_fun_list_ivar1_temp<-mapply(
#     estimate_prod_fn_bounds,
#     x = run_vars_pick_iv$inds[1:3],
#     ins = run_vars_pick_iv$ins[1:3],
#     MoreArgs = list(
#         fs_list=boot_fs_list[[1]],
#         f=obj_fun_ivar1_bounds
#     ),
#     SIMPLIFY = FALSE#,
#     # mc.cores = mc_cores
# )
# names(boot_fs_listprod_fun_list_ivar1_temp)<-paste(run_vars_pick_iv[1:3,"inds"],run_vars_pick_iv[1:3,"ins"])
# omega_ar1_list_temp<-mapply(
#     deconvolute_norm_iv,
#     x=run_vars_pick_iv[1:3,"inds"],
#     ins=run_vars_pick_iv[1:3,"ins"],
#     MoreArgs = list(
#         prod_fun_list=boot_fs_listprod_fun_list_ivar1_temp,
#         fs_list=boot_fs_list[[1]]
#     ),
#     SIMPLIFY = FALSE,
#     USE.NAMES = FALSE
#     # mc.cores = mc_cores
# )


# boot_pf_tbl_90 %>% select(contains("sigma"))
