## Non-parametric deconvolution of productivity omega with the CURRENT PF estimates ---------------
## Why (2026-09-24): the saved omega estimates (425_omega.R -> omega_ar1_deconv_mle.RData, used by
## 550-np-prod.R) were built from an older first stage (GNR-style E[exp eps] estimated, e.g. beta = 0.294
## in 331 vs 0.321 now) and older single-instrument alphas (e.g. alpha_K = 1 in 369 with lag_m). They do
## not match the PF table in thesis ch. 6. This script reruns the same estimator (estimate_np_theta_omega:
## penalized B-spline deconvolution of W~ = omega + (1-beta) eps, f_eps from corporations' residuals) with:
##   - the paper sample and beta from 931.1-fs-se-het.RData (fs_all_ls; eps = measurement error, E[eps]=0),
##   - the single-instrument alphas in 1472-pf-instrument-comparison.csv for lag_m (m*_{it-1}) and
##     lag_2_w_eps (W~_{it-2}), i.e. exactly the point estimates shown in ch. 6's table.
## All firms (corporations included): productivity is defined for every firm. Existing outputs untouched.
## Output: Code/Products/omega_deconv_current_pf.RData (omega_cur_np_ls, omega_cur_stats_df)
library(splines)
library(statmod)
library(parallel)
library(dplyr)

load("Code/Products/np-deconv-funs.RData")         # estimate_np_theta_omega, np_pdf, gl, lambda, ...
load("Code/Products/931.1-fs-se-het.RData")        # fs_all_ls
set.seed(557788)

five <- c("331", "322", "369", "313", "321")
pf <- read.csv("Code/Products/1472-pf-instrument-comparison.csv") %>%
    mutate(sic_3 = as.character(sic_3)) %>%
    filter(sic_3 %in% five, ins %in% c("lag_m", "lag_2_w_eps"))

run_one <- function(i) {
    r <- pf[i, ]
    fs <- fs_all_ls[[r$sic_3]]
    pf_list <- list(coeffs = c(m = fs$beta, k = r$alpha_K, l = r$alpha_L))
    cat("omega:", r$sic_3, r$ins, "beta", fs$beta, "aK", r$alpha_K, "aL", r$alpha_L, "\n")
    estimate_np_theta_omega(fs, pf_list, np_pdf(fs), gl, lambda = lambda, parallel = FALSE)
}
omega_cur_np_ls <- mclapply(seq_len(nrow(pf)), run_one, mc.cores = min(nrow(pf), detectCores() - 1))
names(omega_cur_np_ls) <- paste(pf$sic_3, pf$ins)
omega_cur_stats_df <- get_stats.list(omega_cur_np_ls)
print(omega_cur_stats_df)

save(omega_cur_np_ls, omega_cur_stats_df, pf,
     file = "Code/Products/omega_deconv_current_pf.RData")
