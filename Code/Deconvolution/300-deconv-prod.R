# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/deconv.RData")
load("Code/Products/deconv_mle.RData")

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

## Saving results -----------------------------------

save(
    prod_fun_list,
    file="Code/Products/deconv_prod_fun.RData"
)
