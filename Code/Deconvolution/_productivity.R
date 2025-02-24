# Load data and packages ---------------
library(tidyverse)
library(parallel)
load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/deconv_prod_fun.RData")

## Obtainig productivity estimates --------------
# Productivity = exp(omega+varepsilon)
# 1 Get W_squiggle = big_W - alpha*[k,l] = omega+(1-\beta)varepsilon
# 2 Deconvolute \int f_{prod}(exp(W_squiggle +\beta*\varepsilon))f_{\varepsilon}(\varepsilon)d\varepsilon



alpha <- prod_fun_list[["322 log_mats_share"]]$coeffs
fs_list[["322 log_mats_share"]]$data %>%
    mutate(
        W_squiggle = cal_W - alpha[2]*k-alpha[3]*l
    )

