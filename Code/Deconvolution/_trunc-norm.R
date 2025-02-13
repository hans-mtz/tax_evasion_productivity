# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/deconv.RData")
load("Code/Products/deconv_mle.RData")
load("Code/Products/intermediates.RData")

## Truncated Normal Distribution --------------

## New funs -----------------------------------

f_trc_norm <- function(epsilon,v,mu,sigma){
    x<-sum(epsilon,v)
    if (x>=0){
        num<-f_e(epsilon,v,mu,sigma)
        den<- (1-pnorm(0,mean=mu, sd=sigma))
        return((1/sigma)*(num/den))
    } else {
       return(1e-300)
    }
}

eval_llh_fun<-function(v,f,mu,sigma,params){
    log_lh<-log(E_nrv(f,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,v,mu,sigma))
    return(log_lh)
}

obj_fun_f<- function(theta,f,V,params){
    mu<-theta[1]
    sigma<-1.3^(theta[2])
    sum_llh<-sapply(V,eval_llh_fun,f,mu,sigma,params) |> sum()
    return(sum_llh)
}

## Testing -----------------------------------

# E_nrv(f_trc_norm,0,1,gauss_hermite,0,1,1) |> log()
# params<-list(gauss_int=gauss_hermite, epsilon_mu=0, epsilon_sigma=1)
# eval_llh_fun(0,f_trc_norm,1,1,params)
# obj_fun_f(c(1,1),f_trc_norm,fs_list[["322 log_mats_share"]]$data$cal_V,params)

# optim(c(1,1),obj_fun_f,NULL,f_trc_norm,fs_list[["322 log_mats_share"]]$data$cal_V,params,method="BFGS",control = list(fnscale=-1))


# x<-seq(0,2,by=0.01)
# plot(
#     x,
#     sapply(x,\(x)f_trc_norm(0,x,1.16,0.26))
# )

# init<-c(
#     mu=mean(fs_list[["322 log_mats_share"]]$data$cal_V, na.rm = TRUE), 
#     sigma=1.3^(sd(fs_list[["322 log_mats_share"]]$data$cal_V, na.rm = TRUE))
# )

# optim(init,obj_fun_f,NULL,f_trc_norm,fs_list[["322 log_mats_share"]]$data$cal_V,params,method="BFGS",control = list(fnscale=-1))

## Estimating --------------------------------
mc_cores <- detectCores()-2
top_evasion_inds <- tax_ev_test_tbl %>% arrange(desc(log_mats_share)) %>% pull(sic_3)
run_vars<-expand.grid(inds=top_evasion_inds[1:6],input=intermediates, stringsAsFactors = FALSE)
# run_vars_gnr<-expand.grid(inds=top_evasion_inds [1:5],input=intermediates_gnr, stringsAsFactors = FALSE)

### First Stage

fs_list<-mcmapply(
    first_stage_panel,
    run_vars$inds,
    run_vars$input,
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



trc_norm_res_list<-mclapply(
    names(fs_list),
    function(x){
        params<-list(
            gauss_int=gauss_hermite,
            epsilon_mu=fs_list[[x]]$epsilon_mu,
            epsilon_sigma=fs_list[[x]]$epsilon_sigma
        )
        init<-c(
            mu=mean(fs_list[[x]]$data$cal_V, na.rm = TRUE), 
            sigma=1.3^(sd(fs_list[[x]]$data$cal_V, na.rm = TRUE))
        )
        res<-optim(
            init,
            obj_fun_f,
            NULL,
            f_trc_norm,
            fs_list[[x]]$data$cal_V,
            params,
            method = "BFGS",
            control=list(fnscale=-1)
        )
        mu<-res$par[1]
        sigma<-1.3^res$par[2]
        n<-sum(!is.na(fs_list[[x]]$data$cal_V))
        # me<-1.96*sigma/sqrt(n)
        res$ev_params<-c(
            mu_hat=mu, 
            sigma_hat=sigma,
            n = n#,
            # mu_LCI = mu-1.96*sigma/sqrt(n),
            # mu_UCI = mu+1.96*sigma/sqrt(n)
        )
        return(res)
    },
    mc.cores = mc_cores
)

names(trc_norm_res_list)<-names(fs_list)

## Saving Results ---------------------------------

save(
    trc_norm_res_list, fs_list,
    file="Code/Products/deconv_mle_truncated_normal.RData"
)