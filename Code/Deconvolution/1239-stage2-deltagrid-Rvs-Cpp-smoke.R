## R-side smoke test companion to the C++ deltagrid port (2026-09-08) --------
## Fits the SAME 2 (delta1,delta2) points, same x0, same base_seed=20260829
## (moment set A's own R-side default -- NOT the C++ CLI's own 20260907
## default, NOT the old 15-moment system's 20260827 -- checked directly in
## Code/Rcpp/1200-stage2-elvis.cpp before running this) as the C++ smoke
## test, to confirm the ported deltagrid mode agrees numerically before
## trusting it for the full 96-point run.

library(tidyverse)
library(Rcpp)
library(nloptr)
library(RcppParallel)

sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
load("Code/Products/1200-stage2-data.RData")

DELTA_BOUND <- 60

build_run_sample <- function(ins_choice, data = stage2_data, trim_top_pct = 0) {
    base <- data %>% dplyr::filter(ins == ins_choice, !corp)
    interior <- base %>%
        dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
        mutate(corner = 0L)
    if (trim_top_pct > 0) {
        Mstar_cutoff <- quantile(interior$M_star, 1 - trim_top_pct, na.rm = TRUE)
        interior <- interior %>% dplyr::filter(M_star <= Mstar_cutoff)
    }
    corner_obs <- base %>%
        dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>%
        mutate(corner = 1L)
    bind_rows(interior, corner_obs) %>%
        dplyr::select(plant, year, sic_3, M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner)
}

cue_objective_common <- function(Ghat) {
    dvec <- colMeans(Ghat); Omega <- cov(Ghat)
    eig <- eigen(Omega, symmetric = TRUE)
    pos <- eig$values > 1e-8 * max(eig$values)
    A <- eig$vectors[, pos, drop = FALSE]
    d2 <- as.numeric(t(A) %*% dvec)
    0.5 * sum(d2^2 / eig$values[pos])
}

## par = (delta0, eta, lambda, gamma[1:9]) -- FIXED delta1,delta2 -- mirrors
## inner_obj_A_fixedDelta in grid_estimator.cpp exactly.
cue_objective_A_fixedDelta <- function(par, delta1, delta2, dat, n_burn, n_keep, base_seed) {
    delta0 <- par[1]; eta <- par[2]; lambda <- par[3]; gamma <- par[-(1:3)]
    Ghat <- mh_tilted_average_A_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$.row_id, dat$beta, lambda, delta0, delta1, delta2, eta, gamma, dat$corner,
        n_burn, n_keep, base_seed = base_seed
    )
    cue_objective_common(Ghat)
}

fit_one_point <- function(delta1, delta2, run_sample, n_burn, n_keep, par_init, base_seed,
                           lambda_lo = 1e-8, lambda_hi = 1e-4, xtol_rel = 1e-4, maxeval = 2000, maxtime = 180) {
    dat <- run_sample
    obj <- function(par) cue_objective_A_fixedDelta(par, delta1, delta2, dat, n_burn, n_keep, base_seed)
    lower <- c(-DELTA_BOUND, 0,     lambda_lo, rep(-Inf, 9))
    upper <- c( DELTA_BOUND, 0.999, lambda_hi, rep( Inf, 9))
    ctrl <- list(xtol_rel = xtol_rel, maxeval = maxeval, maxtime = maxtime)
    res1 <- nloptr::bobyqa(x0 = par_init, fn = obj, lower = lower, upper = upper, control = ctrl)
    res2 <- nloptr::bobyqa(x0 = res1$par, fn = obj, lower = lower, upper = upper, control = ctrl)
    list(delta1 = delta1, delta2 = delta2, value = res2$value, par = res2$par,
         value1 = res1$value, convergence = res2$convergence, iter = res2$iter)
}

run_sample <- build_run_sample("lag_m", trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
RcppParallel::setThreadOptions(numThreads = 1)   # match C++ smoke test's n_threads=1 exactly

x0 <- c(3.2411576554474, 0.00064447638164912, 6.51e-7,
        0.000266931500722254, -0.00320709266345527, 0.0173026816261523, 0.0671075607612255,
        0.0361581141572298, -0.331783440956923, 8.08433700184273e-06, 3.21236234943358e-11,
        0.0933072166415534)

points <- list(c(4.123, 0.503), c(2, 0.2))
for (p in points) {
    t0 <- Sys.time()
    fit <- fit_one_point(p[1], p[2], run_sample, n_burn = 500, n_keep = 1000, par_init = x0, base_seed = 20260829)
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("R: d1=%.4g d2=%.4g  Lhat=%.10g (pass1=%.10g)  lambda_hat=%.10g  delta0=%.10g eta=%.10g  conv=%d iter=%d  time=%.1fs\n",
                p[1], p[2], fit$value, fit$value1, fit$par[3], fit$par[1], fit$par[2], fit$convergence, fit$iter, dt))
    cat("  gamma:", paste(sprintf("%.10g", fit$par[-(1:3)]), collapse = ", "), "\n")
}
