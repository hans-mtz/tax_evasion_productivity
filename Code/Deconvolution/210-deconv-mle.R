# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/deconv.RData")


# Data Wrangling ------------------------------------
# colombia_data_frame<-colombia_data_frame %>%
#     mutate(
#         log_ded_share = log(((materials+deductible_expenses)/sales)),
#         log_mats_share = log((materials/sales))
#     )

# First Stage for Industries where we found evidence of tax evasion

mc_cores <- detectCores()-2
run_vars <- cbind(
    inds=rep(order_sic[1:6], each=3),
    input=c("log_share","log_ded_share","log_mats_share")
)

# fs_list<-mcmapply(
#     first_stage,
#     run_vars[,"inds"],
#     run_vars[,"input"],
#     MoreArgs = list(data=colombia_data_frame),
#     SIMPLIFY = FALSE,
#     mc.cores = mc_cores
# )

fs_list<-mcmapply(
    first_stage_panel,
    run_vars[,"inds"],
    run_vars[,"input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

names(fs_list)<-mapply(
    paste, 
    run_vars[,"inds"],
    run_vars[,"input"],
    USE.NAMES = FALSE
)

## Estimation ----------------------------------------

# norm_res_list<-mclapply(
#     names(fs_list),
#     function(x){
#         params<-list(
#             gauss_int=gauss_hermite,
#             epsilon_mu=fs_list[[x]]$epsilon_mu,
#             epsilon_sigma=fs_list[[x]]$epsilon_sigma
#         )
#         res<-optim(
#             c(0,1),
#             obj_fun,
#             NULL,
#             fs_list[[x]]$cal_V,
#             params,
#             method = "BFGS",
#             control=list(fnscale=-1)
#         )
#         mu<-res$par[1]
#         sigma<-1.3^res$par[2]
#         n<-length(fs_list[[x]]$cal_V)
#         me<-1.96*sigma/sqrt(n)
#         res$ev_params<-c(
#             mu_hat=mu, 
#             sigma_hat=sigma,
#             # n = n,
#             mu_LCI = mu-1.96*sigma/sqrt(n),
#             mu_UCI = mu+1.96*sigma/sqrt(n)
#         )
#         return(res)
#     },
#     mc.cores = mc_cores
# )

# names(norm_res_list)<-names(fs_list)

### Tax-Evasion Normal Distribution-------------------

norm_res_list<-mclapply(
    names(fs_list),
    function(x){
        params<-list(
            gauss_int=gauss_hermite,
            epsilon_mu=fs_list[[x]]$epsilon_mu,
            epsilon_sigma=fs_list[[x]]$epsilon_sigma
        )
        res<-optim(
            c(0,1),
            obj_fun,
            NULL,
            fs_list[[x]]$data$cal_V,
            params,
            method = "BFGS",
            control=list(fnscale=-1)
        )
        mu<-res$par[1]
        sigma<-1.3^res$par[2]
        n<-length(fs_list[[x]]$data$cal_V)
        me<-1.96*sigma/sqrt(n)
        res$ev_params<-c(
            mu_hat=mu, 
            sigma_hat=sigma,
            # n = n,
            mu_LCI = mu-1.96*sigma/sqrt(n),
            mu_UCI = mu+1.96*sigma/sqrt(n)
        )
        return(res)
    },
    mc.cores = mc_cores
)

names(norm_res_list)<-names(fs_list)

### Tax-Evasion Log-Normal Distribution --------------

# lognorm_res_list<-mclapply(
#     names(fs_list),
#     function(x){
#         params<-list(
#             gauss_int=gauss_hermite,
#             epsilon_mu=fs_list[[x]]$epsilon_mu,
#             epsilon_sigma=fs_list[[x]]$epsilon_sigma
#         )
#         res<-optim(
#             c(0,1),
#             obj_fun_ln,
#             NULL,
#             fs_list[[x]]$cal_V,
#             params,
#             method = "BFGS",
#             control=list(fnscale=-1)
#         )

#         mu<-res$par[1]
#         sigma<-1.3^(res$par[2])|> round(6)
#         n<-length(fs_list[[x]]$cal_V)
#         mu_hat<-exp(mu+0.5*sigma^2) |> round(6)
#         sigma_hat<-mu_hat*sqrt(exp(sigma^2)-1) |> round(6)
#         mode<-exp(mu-sigma^2) |> round(6)
#         me<- 1.96*sqrt((sigma_hat^2)/n+(sigma_hat^4)/(2*(n-1)))
#         res$ev_params<-c(
#             log_mu = mu,
#             log_sigma = sigma,
#             # n = n,
#             mu_hat=mu_hat,
#             sigma_hat=sigma_hat,
#             mode=mode,
#             median=exp(mu) |> round(6),
#             mu_LCI = mu_hat*exp(-me),
#             mu_UCI = mu_hat*exp(me)

#         )
#         return(res)
#     },
#     mc.cores = mc_cores
# )

# names(lognorm_res_list)<-names(fs_list)

lognorm_res_list<-mclapply(
    names(fs_list),
    function(x){
        params<-list(
            gauss_int=gauss_hermite,
            epsilon_mu=fs_list[[x]]$epsilon_mu,
            epsilon_sigma=fs_list[[x]]$epsilon_sigma
        )
        res<-optim(
            c(0,1),
            obj_fun_ln,
            NULL,
            fs_list[[x]]$data$cal_V,
            params,
            method = "BFGS",
            control=list(fnscale=-1)
        )

        mu<-res$par[1]
        sigma<-1.3^(res$par[2])|> round(6)
        n<-length(fs_list[[x]]$data$cal_V)
        mu_hat<-exp(mu+0.5*sigma^2) |> round(6)
        sigma_hat<-mu_hat*sqrt(exp(sigma^2)-1) |> round(6)
        mode<-exp(mu-sigma^2) |> round(6)
        me<- 1.96*sqrt((sigma_hat^2)/n+(sigma_hat^4)/(2*(n-1)))
        res$ev_params<-c(
            log_mu = mu,
            log_sigma = sigma,
            # n = n,
            mu_hat=mu_hat,
            sigma_hat=sigma_hat,
            mode=mode,
            median=exp(mu) |> round(6),
            mu_LCI = mu_hat*exp(-me),
            mu_UCI = mu_hat*exp(me)

        )
        return(res)
    },
    mc.cores = mc_cores
)

names(lognorm_res_list)<-names(fs_list)


## Saving Results ---------------------------------

save(
    norm_res_list, lognorm_res_list, fs_list, run_vars,
    file="Code/Products/deconv_mle.RData"
)