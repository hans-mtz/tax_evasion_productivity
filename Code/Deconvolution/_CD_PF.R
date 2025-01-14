# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/deconv.RData")
load("Code/Products/deconv_mle.RData")

# Data Wrangling ------------------------------------
colombia_data_frame<-colombia_data_frame %>%
    mutate(
        log_ded_share = log(((materials+deductible_expenses)/sales)),
        log_mats_share = log((materials/sales))
    )

## Setting up cores-------------------------

mc_cores <- detectCores()-2
run_vars <- cbind(
    inds=rep(order_sic[1:6], each=3),
    input=c("log_share","log_ded_share","log_mats_share")
)

## Functions -------------------------------

E_h <- function(W,params=params){

    w_1<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x),
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )

    w_2<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x)^2,
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )

    w_3<-sapply(
        W,
        function(w){
            E_nrv(
                \(x,w)(w-(1-params$beta)*x)^3,
                params$epsilon_mu, 
                params$epsilon_sigma, 
                params$gauss_int,
                w=w
            )
        }
    )

    return(cbind(w_1,w_2,w_3))
    
}

# E_h_par <- function(W,params){

#     w_1<-mclapply(
#         W,
#         function(w){
#             E_nrv(
#                 \(x,w)(w-(1-params$beta)*x),
#                 params$epsilon_mu, 
#                 params$epsilon_sigma, 
#                 params$gauss_int,
#                 w=w
#             )
#         },
#         mc.cores = mc_cores
#     ) |> do.call(c,args=_)

#     w_2<-mclapply(
#         W,
#         function(w){
#             E_nrv(
#                 \(x,w)(w-(1-params$beta)*x)^2,
#                 params$epsilon_mu, 
#                 params$epsilon_sigma, 
#                 params$gauss_int,
#                 w=w
#             )
#         },
#         mc.cores = mc_cores
#     ) |> do.call(c,args=_)

#     w_3<-mclapply(
#         W,
#         function(w){
#             E_nrv(
#                 \(x,w)(w-(1-params$beta)*x)^3,
#                 params$epsilon_mu, 
#                 params$epsilon_sigma, 
#                 params$gauss_int,
#                 w=w
#             )
#         },
#         mc.cores = mc_cores
#     ) |> do.call(c,args=_)

#     return(cbind(w_1,w_2,w_3))
    
# }

first_stage_panel <- function(sic, var, data) {
    fml <- paste0(var,"~1") |> as.formula()
    fs_reg <- data %>%
        mutate(
            treat = ifelse(juridical_organization == 3, "Corp", "Non-Corp")
        ) %>%
        filter(
            sic_3 == sic,
            juridical_organization == 3,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        fixest::feols(fml, data = .)

    log_D <- coefficients(fs_reg)[[1]]
    epsilon <- residuals(fs_reg)
    big_E <- -epsilon |> exp() |> mean()
    beta <- exp(log_D - log(big_E))
    mean_epsilon <- mean(-epsilon)
    variance_epsilon <- var(-epsilon)

    ## Deconvolution ------------------------

    tbl <- data %>%
        filter(
            sic_3 == sic,
            !is.na(.data[[var]]),
            .data[[var]] < Inf,
            .data[[var]] > -Inf
        ) %>%
        mutate(
            y = log(gross_output),
            cal_V = .data[[var]] - log(beta) - log(big_E),
            cal_W = log(gross_output)-beta*(m-cal_V)
        ) %>%
        select(
            sic_3, year, plant, cal_V, cal_W, k, l, y
        )

    result_list <- list(
        data = tbl,
        epsilon_mu = mean_epsilon,
        epsilon_sigma = sqrt(variance_epsilon),
        beta = beta,
        big_E = big_E
    )
    return(result_list)
}

obj_fun_markov<-function(alpha,data,params){
    eta <-data %>% 
        ungroup() %>%
        group_by(plant) %>%
        mutate(
            w_eps = cal_W - alpha[1]*k-alpha[2]*l,
            lag_w_eps = lag(w_eps, order_by = year)
        ) %>%
        lm(w_eps~E_h(lag_w_eps,params), data=., na.action = na.exclude) |>
        residuals()


    moments<-apply(
        data[c("k","l")],
        2, 
        function(i){
        mean(i*eta, na.rm = TRUE)
        }
    )

    obj <- t(moments) %*% moments
    return(obj[1])

}

## Testing -----------------------------------

params<-list(
    gauss_int=gauss_hermite,
    epsilon_mu=fs_list[[1]]$epsilon_mu,
    epsilon_sigma=fs_list[[1]]$epsilon_sigma,
    beta = fs_list[[1]]$beta
)

E_nrv(\(x,w)(w-(1-params$beta)*x),params$epsilon_mu,params$epsilon_sigma,params$gauss_int,w=W[1])
E_nrv(\(x,w)(w-(1-params$beta)*x)^2,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,w=W[1])
E_nrv(\(x,w)(w-(1-params$beta)*x)^3,params$epsilon_mu,params$epsilon_sigma,params$gauss_int,w=W[1])

W<-rnorm(10)

w<-sapply(
    W,
    function(w){
        E_nrv(
            \(x,w)(w-(1-params$beta)*x)^2,
            params$epsilon_mu, 
            params$epsilon_sigma, 
            params$gauss_int,
            w=w
        )
    }
)
w

mclapply(
    W,
    function(w){
        E_nrv(
            \(x,w)(w-(1-params$beta)*x)^2,
            params$epsilon_mu, 
            params$epsilon_sigma, 
            params$gauss_int,
            w=w
        )
    },
    mc.cores = mc_cores
) |> do.call(c, args=_)
w

system.time(
    E_h(W,params)
)
system.time(
    E_h_par(W,params)
)

# No gain in parallelization

E_h_par(W,params)
E_h(W,params)


lm(k~E_h(lag_k,params=params),colombia_data_frame, na.action = "na.exclude") |> summary()


colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        M = 
        W = log(gross_output)-
    ) %>%
    lm(k~E_h(l_k,params=params), data=., na.action = "na.exclude") |> summary()

obj_fun_2s<-function(alpha,data){

    w
}

## -----------------------

first_stage_panel(322,"log_share",colombia_data_frame)

mapply(
    first_stage_panel,
    run_vars[,"inds"],
    run_vars[,"input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE#,
    # mc.cores = mc_cores
)

fs_list_panel<-mcmapply(
    first_stage_panel,
    run_vars[,"inds"],
    run_vars[,"input"],
    MoreArgs = list(data=colombia_data_frame),
    SIMPLIFY = FALSE,
    mc.cores = mc_cores
)

names(fs_list_panel)<-mapply(
    paste, 
    run_vars[,"inds"],
    run_vars[,"input"],
    USE.NAMES = FALSE
)

# a tibble
fs_list_panel[["322 log_mats_share"]]$data[,"cal_V"] |> str() 
# a vector
fs_list_panel[["322 log_mats_share"]]$data$cal_V |> str()
fs_list_panel$`322 log_mats_share`$data$cal_V |> str()
fs_list_panel$`322 log_mats_share`[["data"]]$cal_V |> str()

alpha <- c(2,2)

eta<-fs_list[[1]]$data %>% ungroup() %>% group_by(plant) %>%
    mutate(
        w_eps = cal_W - alpha[1]*k-alpha[2]*l,
        lag_w_eps = lag(w_eps, order_by = year)
    ) %>%
    lm(w_eps~E_h(lag_w_eps,params), data=., na.action = na.exclude) |> residuals() 


# NAs and NaNs are propagated
eta %*% as.matrix(fs_list[[1]]$data[c("k","l")])

moments<-apply(
    fs_list[[1]]$data[c("k","l")], 
    2, 
    function(i){
    mean(i*eta, na.rm = TRUE)
    }
)
t(moments) %*% moments

# it works!!
obj_fun_markov(c(2,2),fs_list[[1]]$data,params)


alpha0<-coef(
    lm(
        cal_W ~ k+l,
        fs_list[[1]]$data
    )
)

optim(
    alpha0[-1],
    obj_fun_markov,
    NULL,
    fs_list[[1]]$data,
    params,
    method = "BFGS",
    control = list(
        maxit = 200
    )
)

optim(
    alpha0[-1],
    obj_fun_markov,
    NULL,
    fs_list[[1]]$data,
    params,
    method = "L-BFGS-B",
    lower = -1, upper = 1,
    control = list(
        maxit = 300
    )
)

optim(
    alpha0[-1],
    obj_fun_markov,
    NULL,
    fs_list[[1]]$data,
    params,
    method = "SANN"
)

optim(
    alpha0[-1],
    obj_fun_markov,
    NULL,
    fs_list[[1]]$data,
    params,
    method = "Nelder-Mead"
)

## Reading Deconvolution results- Prd Fun Parameters
load("Code/Products/deconv_prod_fun.RData")
# sapply(names(prod_fun_list),\(x)prod_fun_list[[x]]$coeffs) |> t() |> round(4)
sapply(paste0(evasion_inds," log_share"),\(x)prod_fun_list[[x]]$coeffs) |> t() |> round(4)

## 

load("Code/Products/Fortran_CD_GNR.RData")
library(readxl)

# Especifica la ruta al archivo Excel
Stata_GNR_results_folder <- "/Volumes/SSD Hans 1/Github/gnr/Code/GNR-ado"
file_path <- file.path(Stata_GNR_results_folder, "CD_GNR_coeffs.xlsx")
# Lee el archivo Excel
Stata_results <- read_excel(file_path)

left_join(Stata_results,CD_fortran_tbl[,c(1,3,2,4)], by=c("INDS"="inds"))
