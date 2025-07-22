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

## MCMC ----------
# Objective: simulate \eta such that moments hold
# i.e., E[\eta k_t]=0 and E[\eta l_t]=0

# Pseudo code:
# 1) Start with W_squiggle and get eta_0
# 2) Generate random eta'=eta_0 + xi
# 3) if log(unif(T)) < -g(x,eta')+exp(g(x,eta_0)) 
#               accept candidate eta_t=eta'
#               otherwise reject eta_t=eta_0
# 4) Generate w_t+1=h(w_t) + eta_t

## Testing --------------------

x <- "322 log_mats_share"

alpha <- prod_fun_list[[x]]$coeffs
df<-fs_list[[x]]$data %>%
    filter(
        is.finite(cal_W),
        is.finite(k),
        is.finite(l)
    ) %>%
    ungroup() %>%
    group_by(plant) %>%
    mutate(
        W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l,
        lag_W_squiggle = lag(W_squiggle, order_by = year)
    )
params<-list(
        gauss_int=gauss_hermite,
        epsilon_mu=fs_list[[x]]$epsilon_mu,
        epsilon_sigma=fs_list[[x]]$epsilon_sigma,
        beta = fs_list[[x]]$beta
    )
h_fit <- lm(W_squiggle ~ E_h(lag_W_squiggle,params),df)
h_fit |> summary()
h_fit$coefficients
model.matrix(W_squiggle~poly(lag_W_squiggle,3,raw=TRUE),df) |> str()
model.matrix(W_squiggle~poly(lag_W_squiggle,3,raw=TRUE),df) %*% h_fit$coefficients
model.matrix(~poly(runif(4470),3,raw=TRUE)) %*% h_fit$coefficients

predict(h_fit,data.frame(lag_W_squiggle=runif(4470)))
h_fit |> str()
