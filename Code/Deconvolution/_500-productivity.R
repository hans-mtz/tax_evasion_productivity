# Load data and packages ---------------
library(tidyverse)
# library(parallel)
# library(quantreg)
# library(ggplot2)
load("Code/Products/colombia_data.RData")
load("Code/Products/deconv_funs.Rdata")
load("Code/Products/boot_deconv_mle.RData")
load("Code/Products/omega_deconv_mle.RData")
load("Code/Products/boot_tax_ev_mmt.RData")
load("Code/Products/deconv_prod_fun.RData")

# Setting Seed -----------
set.seed(987654)
## Compare to GNR --------

folder_results <- "/Volumes/SSD Hans 1/Github/gnr/Data/"


CD_fortran_prod_list <- lapply(
    # union(evasion_inds,gnr_inds),
    top_evading_inds[1:5],
    \(x){
        temp_coeff<-read.csv(paste0(folder_results,"CD_productivity_R_",x,".out"))
        temp_coeff$inds <- x
        return(temp_coeff)
    }
)

CD_GNR_prod_df<-do.call(rbind,CD_fortran_prod_list)

# Convolute -----------------------------

# productivity = exp{\omega + \varepsilon}
# 1) Generate random draw from f_\omega
# 2) Generate random draw from f_\varepsilon
# 3) form prod = exp{\omega + \varepsilon}
# 4) Compare distribution characteristics with GNR and OLS


get_productivity<-function(
    x,
    omega_norm_res_list,
    fs_list,
    nsim
    ){
    # Generate random draws nsim draw --------
    omega_mu<-omega_norm_res_list[[x]][["mu.mu"]] |> as.numeric()
    omega_sigma<-omega_norm_res_list[[x]][["sigma.sigma"]] |> as.numeric()

    omega<-rnorm(nsim,mean=omega_mu,sd=omega_sigma)

    epsilon_mu<-fs_list[[x]]$epsilon_mu
    epsilon_sigma<-fs_list[[x]]$epsilon_sigma

    epsilon<-rnorm(nsim,mean=epsilon_mu,sd=epsilon_sigma)

    # Generate productivity --------------

    phi <- exp(omega+epsilon)

    inds <- str_extract(x,"\\d+")

    return(data.frame(phi=phi, inds=inds))
}

lapply(
    names(fs_list),
    get_productivity,
    omega_norm_res_list,
    fs_list,
    1000
) |> do.call(rbind,args=_) -> productivity_df

productivity_df %>%
    group_by(inds) %>%
    summarise(
        Mean = mean(phi),
        SD = sd(phi),
        Q1 = quantile(phi, probs=0.25)[[1]],
        Median = median(phi),
        Q3 = quantile(phi, probs=0.75)[[1]]
    ) -> productivity_tbl

CD_GNR_prod_df %>%
    group_by(inds) %>%
    summarise(
        Mean = mean(CD.productivity),
        SD = sd(CD.productivity),
        Q1 = quantile(CD.productivity, probs=0.25)[[1]],
        Median = median(CD.productivity),
        Q3 = quantile(CD.productivity, probs=0.75)[[1]]
    ) -> CD_GNR_prod_tbl

## Saving results ---------------------

save(
    productivity_tbl, CD_GNR_prod_tbl,
    productivity_df, CD_GNR_prod_df,
    file="Code/Products/productivity.RData"
)

## Testing ----------------------------------

# # Generate random draws 1000 draw --------
# # names(omega_norm_res_list)<- names(fs_list)
# x<-"351 log_mats_share"
# omega_mu<-omega_norm_res_list[[x]][["mu.mu"]] |> as.numeric()
# omega_sigma<-omega_norm_res_list[[x]][["sigma.sigma"]] |> as.numeric()

# omega<-rnorm(1000,mean=omega_mu,sd=omega_sigma)

# epsilon_mu<-fs_list[[x]]$epsilon_mu
# epsilon_sigma<-fs_list[[x]]$epsilon_sigma

# epsilon<-rnorm(1000,mean=epsilon_mu,sd=epsilon_sigma)

# # Generate productivity --------------

# phi <- exp(omega+epsilon)

# alpha <- prod_fun_list[[x]]$coeffs
# df<-fs_list[[x]]$data %>%
#     filter(
#         is.finite(cal_W),
#         is.finite(k),
#         is.finite(l)
#     ) %>%
#     mutate(
#         W_squiggle = cal_W - alpha[["k"]]*k-alpha[["l"]]*l,
#         exp_W_sqg = exp(W_squiggle)
#     )

# df$exp_W_sqg |> max() -> max_exp_W_sqg

# inds <- str_extract(x,"\\d+")

