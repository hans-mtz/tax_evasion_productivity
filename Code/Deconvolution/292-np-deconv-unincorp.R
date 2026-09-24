## Non-parametric deconvolution of u = ln(M*/M) on UNINCORPORATED firms only ----------------
## Why (2026-09-24): 291-bs-deconv.R deconvolves cal_V pooled over ALL firms in the industry
## (corporations included, whose u = 0 by assumption), which dilutes f_u toward zero and makes
## its mean match the one-sample test, not the preferred two-sample test (pref_tax_ev_test_tbl,
## 206-boot-test.R: beta from corporations, test on unincorporated firms only).
## This script re-runs the same estimator (same f_eps from corporations' residuals, same
## penalized B-spline logspline, same lambda/knots rule) on the unincorporated subsample, i.e.
## the same firms the preferred test uses. 291's results are left untouched.
## Output: Code/Products/np_deconv_unincorp.RData (unincorp_np_deconv_list, unincorp_np_stats_df)
library(splines)
library(statmod)
library(parallel)
library(dplyr)

load("Code/Products/np-deconv-funs.RData")   # functions, fs_list, gl, lambda, n_knots, pspline_degree
load("Code/Products/test_data.RData")        # juridical_organization, to split corps / unincorporated
set.seed(557788)

select_fs_l <- paste0(c("331", "322", "369", "313", "321"), " log_mats_share")
jo <- test_data %>% ungroup() %>% distinct(plant, year, juridical_organization)

run_one <- function(nm) {
    fs <- fs_list[[nm]]
    eps_pdf <- np_pdf(fs)   # f_eps from corporations' residuals (non-NA epsilon rows), as in 291
    fs_u <- fs
    fs_u$data <- fs$data %>%
        left_join(jo, by = c("plant", "year")) %>%
        filter(juridical_organization != 3)
    cat("Estimating", nm, "on", nrow(fs_u$data), "unincorporated obs\n")
    estimate_np_theta(fs_u, eps_pdf, gl, lambda = lambda, parallel = FALSE)
}

unincorp_np_deconv_list <- mclapply(select_fs_l, run_one, mc.cores = length(select_fs_l))
names(unincorp_np_deconv_list) <- select_fs_l
unincorp_np_stats_df <- get_stats.list(unincorp_np_deconv_list)
print(unincorp_np_stats_df)

save(unincorp_np_deconv_list, unincorp_np_stats_df,
     file = "Code/Products/np_deconv_unincorp.RData")
