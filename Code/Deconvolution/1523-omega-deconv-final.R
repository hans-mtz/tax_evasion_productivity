## PRODUCT: Code/Products/1523-omega-deconv-final.RData := non-parametric deconvolution of productivity omega on the FINAL
## specification (2026-09-26). Copy of 293-omega-deconv-current-pf.R with only the inputs swapped:
##   - first stage: 1501-fs-net.RData (two-tax share, juridical organization codes 6-9 excluded; beta, f_eps from corporations);
##   - alphas: 1517-pf-systems-all-industries.RData, point = minimum of the test-inversion statistic, instruments
##     lag_2_w_eps (W~_{it-2}, the headline) and lag_m (m*_{it-1}, comparison);
##   - industries: the seven selected for deconvolution (313, 321, 322, 324, 331, 342, 369).
## Same estimator: estimate_np_theta_omega (penalized B-spline deconvolution of W~ = omega + (1-beta) eps), all firms.
## Output: omega_fin_np_ls, omega_fin_stats_df, pf
library(splines); library(statmod); library(parallel); library(dplyr)
load("Code/Products/np-deconv-funs.RData")               # estimate_np_theta_omega, np_pdf, gl, lambda, ...
load("Code/Products/1501-fs-net.RData")                  # fs_net_ls
load("Code/Products/1517-pf-systems-all-industries.RData")   # res
set.seed(557788)

seven <- c("313", "321", "322", "324", "331", "342", "369")
pf <- res %>% filter(sic_3 %in% seven, system %in% c("lag_2_w_eps", "lag_m")) %>%
    transmute(sic_3, ins = system, alpha_K, alpha_L)

run_one <- function(i) {
    r <- pf[i, ]; fs <- fs_net_ls[[r$sic_3]]
    pf_list <- list(coeffs = c(m = fs$beta, k = r$alpha_K, l = r$alpha_L))
    cat("omega:", r$sic_3, r$ins, "beta", fs$beta, "aK", r$alpha_K, "aL", r$alpha_L, "\n")
    estimate_np_theta_omega(fs, pf_list, np_pdf(fs), gl, lambda = lambda, parallel = FALSE)
}
omega_fin_np_ls <- mclapply(seq_len(nrow(pf)), run_one, mc.cores = min(nrow(pf), detectCores() - 1))
names(omega_fin_np_ls) <- paste(pf$sic_3, pf$ins)
omega_fin_stats_df <- get_stats.list(omega_fin_np_ls)
print(omega_fin_stats_df)
save(omega_fin_np_ls, omega_fin_stats_df, pf, file = "Code/Products/1523-omega-deconv-final.RData")
cat("Saved: Code/Products/1523-omega-deconv-final.RData\n")
