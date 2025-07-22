# %% Load data and packages ---------------
library(tidyverse)
library(parallel)
# load("Code/Products/colombia_data.RData")
load("Code/Products/test_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/intermediates.RData") # Top evading industries
# load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/fs.RData") # Fist stage results

## %% Setting seed for Reproducibility --------------
set.seed(66636)
J = 10 # Number of Top Revenue Industries 1<=J<= 20
R = 250 # Number of Bootstrap Replicates
## Define Variables ---------------------

mc_cores <- detectCores()-2
# top_evading_inds <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)
# run_vars<-expand.grid(inds=top_evading_inds[1:5],input="log_mats_share", stringsAsFactors = FALSE)

# inter_named <- c(
#     "log_mats_share" = "materials",
#     "log_deductible_intermediates_share" = "deductible_intermediates"   
# )
# # inter_named[rep("log_mats_share", 10)]
# run_vars$r_input <- inter_named[run_vars$input]

## %% Deconvoluting ------------------------

### First Stage -------------------------

# fs_list<-mcmapply(
#     first_stage_panel, #sic_3, log_mats_share, juridical_organization, gross_output, year, plant, k, l
#     run_vars[,"inds"],
#     run_vars[,"input"],
#     run_vars[,"r_input"],
#     MoreArgs = list(data=test_data),#colombia_data_frame),
#     SIMPLIFY = FALSE,
#     mc.cores = mc_cores
# )

# names(fs_list)<-paste(run_vars[,"inds"],run_vars[,"input"])
# # names(fs_list)<-run_vars[,"inds"] #Only one Intermediate materials

### %% Deconvluting LogNormal Distribution ---------------

lognorm_res_list<-mclapply(
    names(fs_list),
    deconvolute_lognorm,
    fs_list=fs_list,
    mc.cores = mc_cores
)

# lognorm_res_list<-mclapply(
#     names(fs_list),
#     function(x){
#         params<-list(
#             gauss_int=gauss_hermite,
#             epsilon_mu=fs_list[[x]]$epsilon_mu,
#             epsilon_sigma=fs_list[[x]]$epsilon_sigma
#         )

#         init<-c(
#             mu=mean(fs_list[[x]]$data$cal_V, na.rm = TRUE), 
#             sigma=1.3^(sd(fs_list[[x]]$data$cal_V, na.rm = TRUE))
#         )

#         res<-optim(
#             init,
#             obj_fun_ln,
#             NULL,
#             fs_list[[x]]$data$cal_V,
#             params,
#             method = "BFGS",
#             control=list(fnscale=-1) #Maximizing instead of minimizing
#         )

#         mu<-res$par[1]
#         sigma<-1.3^(res$par[2])|> round(6)
#         mean_lognorm<-exp(mu+0.5*sigma^2) |> round(6)
#         sd_lognorm<-mean_lognorm*sqrt(exp(sigma^2)-1) |> round(6)
#         mode<-exp(mu-sigma^2) |> round(6)
#         # n<-length(fs_list[[x]]$data$cal_V)
#         # me<- 1.96*sqrt((sd_lognorm^2)/n+(sd_lognorm^4)/(2*(n-1)))
#         ev_params<-c(
#             mu = mu,
#             sigma = sigma,
#             mean=mean_lognorm,
#             sd=sd_lognorm,
#             mode=mode,
#             median=exp(mu) |> round(6),
#             convergence = res$convergence,
#             id = x,
#             dist = "lognormal"
#             # n = n,
#             # mu_LCI = mu_hat*exp(-me),
#             # mu_UCI = mu_hat*exp(me)

#         )
#         return(ev_params)
#     },
#     mc.cores = mc_cores
# )

# names(lognorm_res_list)<-names(fs_list)
lognorm_tbl<-do.call(rbind,lognorm_res_list)

### %% Truncated Normal ---------------------

trc_norm_res_list<-mclapply(
    names(fs_list),
    deconvolute_trcnorm,
    fs_list = fs_list,
    mc.cores = mc_cores
)

# trc_norm_res_list<-mclapply(
#     names(fs_list),
#     function(x){
#         params<-list(
#             gauss_int=gauss_hermite,
#             epsilon_mu=fs_list[[x]]$epsilon_mu,
#             epsilon_sigma=fs_list[[x]]$epsilon_sigma
#         )
#         init<-c(
#             mu=mean(fs_list[[x]]$data$cal_V, na.rm = TRUE), 
#             sigma=1.3^(sd(fs_list[[x]]$data$cal_V, na.rm = TRUE))
#         )
#         res<-optim(
#             init,
#             obj_fun_f,
#             NULL,
#             f_trc_norm,
#             fs_list[[x]]$data$cal_V,
#             params,
#             method = "BFGS",
#             control=list(fnscale=-1)
#         )
#         mu<-res$par[1]
#         sigma<-1.3^res$par[2]
#         alpha <- -mu/sigma
#         Z = 1 - pnorm(0,mean=mu, sd=sigma)
#         num = dnorm(0,mean=mu,sd=sigma)
#         mean_trcnorm = mu+sigma*num/Z
#         num_median = pnorm(0,mean=mu,sd=sigma)+1
#         median_trcnorm = mu+qnorm(num_median/2, mean=mu, sd=sigma)*sigma
#         variance_trcnorm = sigma*sigma*(1+alpha*num/Z-(num/Z)^2)
#         # n<-sum(!is.na(fs_list[[x]]$data$cal_V))
#         # me<-1.96*sigma/sqrt(n)
#         ev_params<-c(
#             mu = mu,
#             sigma = sigma,
#             mean=mean_trcnorm, 
#             sd= sqrt(variance_trcnorm),
#             mode = mu,
#             median = median_trcnorm,
#             convergence = res$convergence,
#             id = x,
#             dist = "truncated normal"
#             # n = n#,
#             # mu_LCI = mu-1.96*sigma/sqrt(n),
#             # mu_UCI = mu+1.96*sigma/sqrt(n)
#         )
#         return(ev_params)
#     },
#     mc.cores = mc_cores
# )

# names(trc_norm_res_list)<-names(fs_list)

## %% Collecting Deconvolucion Results ------------------

lognorm_tbl<-do.call(rbind,lognorm_res_list)
trcnorm_tbl<-do.call(rbind,trc_norm_res_list)

## Saving Preliminary Results ---------------------------------

save(
    fs_list,trc_norm_res_list, lognorm_res_list,
    lognorm_tbl, trcnorm_tbl,
    file="Code/Products/boot_deconv_mle.RData"
)

# rbind(
#     lognorm_tbl,
#     cbind(trcnorm_tbl,mode=NA, median=NA)[,c(1,2,3,4,8,9,5,6,7)]
# )

(mle_deconv_tbl <-rbind(lognorm_tbl,trcnorm_tbl) |>
    as.data.frame() )#%>%
    # mutate(
    #     across(
    #         -c(id, dist),
    #         ~ as.numeric(.x) |> round(4)
    #     ),
    #     sic_3 = str_extract(id,"\\d+")
    # ) %>%
    # mutate(
    #     sic_3 = fct_reorder(sic_3, mean, max)
    # ) %>%
    # select(
    #     sic_3,dist,mu:median
    # ) %>%
    # arrange(desc(sic_3),dist)

# mle_deconv_tbl<-mle_deconv_tbl |> as.data.frame() %>%
#     mutate(
#         across(
#             -c(id, dist),
#             ~ as.numeric(.x) |> round(4)
#         ),
#         sic_3 = str_extract(id,"\\d+")
#     ) %>%
#     select(
#         sic_3,dist,mu:median
#     )

## Saving Preliminary Results ---------------------------------

save(
    fs_list,trc_norm_res_list, lognorm_res_list,
    lognorm_tbl, trcnorm_tbl, mle_deconv_tbl,
    file="Code/Products/boot_deconv_mle.RData"
)

## Bootstrapping -----------------------------------
# load("Code/Products/boot_deconv_mle.RData")

# col_df <- colombia_data_frame %>%
#     select(
#         sic_3, log_mats_share, juridical_organization, 
#         gross_output, year, plant, k, l, m, y,
#         log_sales, log_deductible_intermediates_share,
#         deductible_intermediates, materials
#     )

col_df <- test_data %>%
    filter(
        log_mats_share > log(threshold_cut)
    )

boot_res_list<-mclapply(
    1:R,
    function(i){
    resampled_data<-resample_by_group(col_df,sic_3)
    ### First Stage -------------------------
    fs_list_temp<-mapply(
        first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
        run_vars[,"inds"],
        run_vars[,"input"],
        run_vars[,"r_input"],
        MoreArgs = list(data=resampled_data),
        SIMPLIFY = FALSE#,
        # mc.cores = mc_cores
    )

    names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])
    ### Deconvluting LogNormal Distribution ---------------

    lognorm_temp<-lapply(
        names(fs_list_temp),
        deconvolute_lognorm,
        fs_list=fs_list_temp
    )

    lognorm_tbl_temp<-do.call(rbind,lognorm_temp)

    ### Truncated Normal ---------------------

    trc_norm_temp<-lapply(
        names(fs_list_temp),
        deconvolute_trcnorm,
        fs_list=fs_list_temp
    )

    trunc_norm_tbl_temp<-do.call(rbind,trc_norm_temp)

    if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}

    return(
        list(
            tbl=rbind(lognorm_tbl_temp,trunc_norm_tbl_temp),
            fs_list=fs_list_temp
            )
        )
    },
    mc.cores = mc_cores
)

# boot_res_list<-mclapply(
#     1:200,
#     function(i){
#     resampled_data<-resample_by_group(col_df,sic_3)
#     ### First Stage -------------------------
#     fs_list_temp<-mapply(
#         first_stage_panel, #sic_3,log_mats_share, juridical_organization, gross_output, year, plant, k, l
#         run_vars[,"inds"],
#         run_vars[,"input"],
#         MoreArgs = list(data=resampled_data),
#         SIMPLIFY = FALSE#,
#         # mc.cores = mc_cores
#     )

#     names(fs_list_temp)<-paste(run_vars[,"inds"],run_vars[,"input"])
#     # names(fs_list)<-run_vars[,"inds"] #Only one Intermediate materials
    
#     ### Deconvluting LogNormal Distribution ---------------

#     lognorm_temp<-lapply(
#         names(fs_list_temp),
#         function(x){
#             params<-list(
#                 gauss_int=gauss_hermite,
#                 epsilon_mu=fs_list_temp[[x]]$epsilon_mu,
#                 epsilon_sigma=fs_list_temp[[x]]$epsilon_sigma
#             )

#             init<-c(
#                 mu=mean(fs_list_temp[[x]]$data$cal_V, na.rm = TRUE), 
#                 sigma=1.3^(sd(fs_list_temp[[x]]$data$cal_V, na.rm = TRUE))
#             )

#             res<-optim(
#                 init,
#                 obj_fun_ln,
#                 NULL,
#                 fs_list_temp[[x]]$data$cal_V,
#                 params,
#                 method = "BFGS",
#                 control=list(fnscale=-1) #Maximizing instead of minimizing
#             )

#             mu<-res$par[1]
#             sigma<-1.3^(res$par[2])|> round(6)
#             mean_lognorm<-exp(mu+0.5*sigma^2) |> round(6)
#             sd_lognorm<-mean_lognorm*sqrt(exp(sigma^2)-1) |> round(6)
#             mode<-exp(mu-sigma^2) |> round(6)
#             # n<-length(fs_list[[x]]$data$cal_V)
#             # me<- 1.96*sqrt((sd_lognorm^2)/n+(sd_lognorm^4)/(2*(n-1)))
#             ev_params<-c(
#                 mu = mu,
#                 sigma = sigma,
#                 mean=mean_lognorm,
#                 sd=sd_lognorm,
#                 mode=mode,
#                 median=exp(mu) |> round(6),
#                 convergence = res$convergence,
#                 id = x,
#                 dist = "lognormal"
#                 # n = n,
#                 # mu_LCI = mu_hat*exp(-me),
#                 # mu_UCI = mu_hat*exp(me)

#             )
#             return(ev_params)
#         }#,
#         # mc.cores = mc_cores
#     )

#     # names(lognorm_res_list)<-names(fs_list_temp)
#     lognorm_tbl_temp<-do.call(rbind,lognorm_temp)

#     ### Truncated Normal ---------------------

#     trc_norm_temp<-lapply(
#         names(fs_list_temp),
#         function(x){
#             params<-list(
#                 gauss_int=gauss_hermite,
#                 epsilon_mu=fs_list_temp[[x]]$epsilon_mu,
#                 epsilon_sigma=fs_list_temp[[x]]$epsilon_sigma
#             )
#             init<-c(
#                 mu=mean(fs_list_temp[[x]]$data$cal_V, na.rm = TRUE), 
#                 sigma=1.3^(sd(fs_list_temp[[x]]$data$cal_V, na.rm = TRUE))
#             )
#             res<-optim(
#                 init,
#                 obj_fun_f,
#                 NULL,
#                 f_trc_norm,
#                 fs_list_temp[[x]]$data$cal_V,
#                 params,
#                 method = "BFGS",
#                 control=list(fnscale=-1)
#             )
#             mu<-res$par[1]
#             sigma<-1.3^res$par[2]
#             alpha <- -mu/sigma
#             Z = 1 - pnorm(0,mean=mu, sd=sigma)
#             num = dnorm(0,mean=mu,sd=sigma)
#             mean_trcnorm = mu+sigma*num/Z
#             num_median = pnorm(0,mean=mu,sd=sigma)+1
#             median_trcnorm = mu+qnorm(num_median/2, mean=mu, sd=sigma)*sigma
#             variance_trcnorm = sigma*sigma*(1+alpha*num/Z-(num/Z)^2)
#             # n<-sum(!is.na(fs_list[[x]]$data$cal_V))
#             # me<-1.96*sigma/sqrt(n)
#             ev_params<-c(
#                 mu = mu,
#                 sigma = sigma,
#                 mean=mean_trcnorm, 
#                 sd= sqrt(variance_trcnorm),
#                 mode = mu,
#                 median = median_trcnorm,
#                 convergence = res$convergence,
#                 id = x,
#                 dist = "truncated normal"
#                 # n = n#,
#                 # mu_LCI = mu-1.96*sigma/sqrt(n),
#                 # mu_UCI = mu+1.96*sigma/sqrt(n)
#             )
#             return(ev_params)
#         }#,
#         # mc.cores = mc_cores
#     )

#     # names(trc_norm_res_list)<-names(fs_list_temp)
#     trunc_norm_tbl_temp<-do.call(rbind,trc_norm_temp)
#     if(i %% 20==0){cat("Done with bootstrap replicate:",i,"\n")}
#     return(rbind(lognorm_tbl_temp,trunc_norm_tbl_temp))
#     },
#     mc.cores = mc_cores
# )

## Saving Prelim Results ---------------------------------

save(
    fs_list,trc_norm_res_list, lognorm_res_list,
    lognorm_tbl, trcnorm_tbl, mle_deconv_tbl,
    boot_res_list,
    file="Code/Products/boot_deconv_mle.RData"
)

## Collecting Results -----------------------------

# load("Code/Products/boot_deconv_mle.RData")

lapply(
    seq_along(boot_res_list),
    function(x){
        boot_res_list[[x]]$tbl
    }
) |> do.call(rbind, args=_) |> as.data.frame()-> boot_res_df

boot_mle_deconv_tbl<- boot_res_df %>%
    # do.call(rbind, boot_res_list) |> 
    as.data.frame() %>%
    # mutate(
    #     sic_3 = str_extract(id,"\\d+")
    # ) %>%
    left_join(
        as.data.frame(mle_deconv_tbl),
        by = c("sic_3", "inter","dist"),
        suffix = c("",".t0")
    ) %>%
    # head() 
    mutate(
        across(
            !c(sic_3,inter,dist),
            as.numeric
        ),
        bc_boot_mean = mean-mean.t0,
        bc_boot_sd = sd-sd.t0,
        # id = fct_reorder(id,mean.t0,min)
    ) %>%
    # filter(is.na(bc_boot_sd))
    group_by(sic_3,inter,dist) %>%
    reframe(
        value_mean = quantile(bc_boot_mean, probs=c(0.975,0.025)),
        value_sd = quantile(bc_boot_sd, probs = c(0.975,0.025), na.rm=TRUE),
        probs = c(0.975,0.025),
        ci_name = c("LCI","UCI"),
        CI_mean = max(mean.t0)-value_mean,
        CI_sd = max(sd.t0, na.rm=TRUE)-value_sd,
        mean = max(mean.t0),
        sd = max(sd.t0, na.rm=TRUE)#,
        
    ) %>%
    select(
        sic_3,inter,dist,mean,ci_name,CI_mean,sd,CI_sd
    ) %>%
    pivot_wider(
        names_from = ci_name,
        values_from = c(CI_mean,CI_sd) #CI_mean# 
    ) %>%
    ungroup() %>%
    mutate(
        sic_3 = factor(sic_3, levels = top_10_revenue$sic_3[1:J])
    ) %>%
    # arrange(desc(sic_3),dist) %>%
    select(
        SIC = sic_3,
        Inter. = inter,
        Distribution = dist,
        contains("mean"),
        contains("sd")
    )

boot_mle_deconv_tbl


boot_tx_ev_mle_tbl<- boot_res_df %>%
    # do.call(rbind, boot_res_list) |> 
    # as.data.frame() %>%
    # mutate(
    #     sic_3 = str_extract(id,"\\d+")
    # ) %>%
    left_join(
        as.data.frame(mle_deconv_tbl),
        by = c("sic_3", "inter","dist"),
        suffix = c("",".t0")
    ) %>%
    # head() 
    mutate(
        across(
            !c(sic_3,inter,dist),
            as.numeric
        ),
        bc_boot_mean = mean-mean.t0,
        bc_boot_sd = sd-sd.t0,
        # id = fct_reorder(id,mean.t0,min)
    ) %>%
    # filter(is.na(bc_boot_sd))
    group_by(sic_3,inter,dist) %>%
    reframe(
        value_mean = quantile(bc_boot_mean, probs=c(0.975,0.025)),
        value_sd = quantile(bc_boot_sd, probs = c(0.975,0.025), na.rm=TRUE),
        probs = c(0.975,0.025),
        ci_name = c("LCI","UCI"),
        CI_mean = max(mean.t0)-value_mean,
        CI_sd = max(sd.t0, na.rm=TRUE)-value_sd,
        mean = max(mean.t0),
        sd = max(sd.t0, na.rm=TRUE)#,
        
    ) %>%
    select(
        sic_3,inter,dist,mean,ci_name,CI_mean,sd,CI_sd
    ) %>%
    pivot_wider(
        names_from = ci_name,
        values_from = c(CI_mean,CI_sd) #CI_mean# 
    ) %>%
    mutate(
        across(
            !c(sic_3,inter,dist),
            ~round(.,4)
        ),
        CI_mean = glue::glue("[{CI_mean_LCI}, {CI_mean_UCI}]"),
        CI_sd = glue::glue("[{CI_sd_LCI}, {CI_sd_UCI}]"),
        coeff_mean = glue::glue("{mean}"),
        coeff_sd = glue::glue("{sd}")
    ) %>%
    select(
        sic_3,inter,dist,coeff_mean, coeff_sd, CI_mean, CI_sd
    ) %>%
    pivot_longer(
        cols = contains(c("CI","coeff")),
        names_to = c("type","statistic"),
        names_pattern = "(CI|coeff)_(mean|sd)",
        values_to = "Value"
    ) %>%
    mutate(
        type = factor(type, levels = c("CI","coeff")),
        sic_3 = factor(sic_3, levels = top_10_revenue$sic_3[1:J])
    ) %>%
    pivot_wider(
        names_from = statistic,
        values_from = Value
    ) %>%
    group_by(sic_3, inter, dist) %>%
    arrange(desc(type), .by_group = TRUE)

boot_tx_ev_mle_tbl

## Plotting to check if things make sense--------


# density_plots<-lapply(
#     unique(mle_deconv_tbl$sic_3),
#     function(i){
#         x<-seq(0,2,by=0.01)

#         mu<-mle_deconv_tbl %>% 
#             filter(sic_3==i, dist=="lognormal") %>%
#             pull(mu)
#         sigma<-mle_deconv_tbl %>% 
#             filter(sic_3==i, dist=="lognormal") %>%
#             pull(sigma.sigma)
#         mu2<-mle_deconv_tbl %>% 
#             filter(sic_3==i, dist=="truncated normal") %>%
#             pull(mu)
#         sigma2<-mle_deconv_tbl %>% 
#             filter(sic_3==i, dist=="truncated normal") %>%
#             pull(sigma.sigma)

#         plot(
#             x,
#             dnorm(x,mu2,sigma2),
#             type = "l", col="red",
#                     xlab = "", ylab = "",
#             main = i
#         )
#         lines(
#             x,
#             dlnorm(x,mu,sigma),
#             type = "l", col="blue"
#         )
#     }
# )

# par(mfrow = c(5, 1)) # Establecer la disposición de la cuadrícula

# for (plot in density_plots) {
#    print(plot)
# }

# par(mfrow = c(1, 1))


## Saving Results ---------------------------------

save(
    fs_list,trc_norm_res_list, lognorm_res_list,
    lognorm_tbl, trcnorm_tbl, mle_deconv_tbl,
    boot_res_list, boot_mle_deconv_tbl,
    boot_tx_ev_mle_tbl, top_5_ev_inds,
    file="Code/Products/boot_deconv_mle.RData"
)

## Loading Results ---------------------------------

# load("Code/Products/boot_deconv_mle.RData")

# boot_tx_ev_mle_tbl %>%
#     filter(
#         inter == "deductible_intermediates",
#         sic_3 %in% top_5_ev_inds
#     ) %>% View()
