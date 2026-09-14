## lag_2_cal_W full coarse grid, EVERY point independently seeded from lag_m's
## own best-fit par -- moment set A (9 moments), trim=0.5% (2026-09-08) ------
## The multi-start check (1232) showed lag_m's fit finds a materially better,
## sign-flipped (delta1,delta2>0) optimum than the naive ascending chain, at
## lambda=2.15e-8 and 1e-7 specifically. A first attempt to port this into
## the full 16-point coarse grid via the driver's par_init CLI override
## FAILED silently: sequential per-lambda chaining means par_init only ever
## seeds the FIRST grid point (lambda=1e-9); every later point warm-starts
## from the PREVIOUS point's own converged fit, so the whole 16-point chain
## reproduced the original naive-chain results bit-for-bit (confirmed
## directly, max abs diff in Lhat = 0 across all 16 points).
##
## Fix: every lambda point fit INDEPENDENTLY from the SAME par_lagm vector --
## no chaining across lambda at all (matches exactly what worked in 1232, now
## extended from 3 points to the full 16-point grid). Within each point, the
## usual two-pass BOBYQA refinement still applies (pass 2 warm-started from
## pass 1's own result) -- unchanged from fit_one_lambda_A everywhere else in
## this project.
##
## Same 16-point log-spaced grid (1e-9 to 1e-4, factor ~2.15) as every other
## coarse run, for direct comparability against the naive-chain baseline
## (backed up: Code/Products/1211-...-trim0.005.RData.bak-9moment-coarse).

library(tidyverse)
library(Rcpp)
library(nloptr)
library(RcppParallel)

sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
load("Code/Products/1200-stage2-data.RData")

DELTA_BOUND <- 60

build_run_sample <- function(ins_choice, data = stage2_data, corner_mode = "include_zero", trim_top_pct = 0) {
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

cue_objective_A <- function(par, lambda, dat, n_burn, n_keep) {
    delta0 <- par[1]; delta1 <- par[2]; delta2 <- par[3]; eta <- par[4]
    gamma  <- par[-(1:4)]
    Ghat <- mh_tilted_average_A_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$.row_id, dat$beta, lambda, delta0, delta1, delta2, eta, gamma, dat$corner,
        n_burn, n_keep
    )
    cue_objective_common(Ghat)
}

fit_one_lambda_A <- function(lambda, run_sample, n_burn, n_keep, par_init,
                              xtol_rel = 1e-4, maxeval = 2000, maxtime = 90) {
    dat <- run_sample
    obj <- function(par) cue_objective_A(par, lambda, dat, n_burn, n_keep)
    lower <- c(rep(-DELTA_BOUND, 3), 0,     rep(-Inf, 9))
    upper <- c(rep( DELTA_BOUND, 3), 0.999, rep( Inf, 9))
    ctrl <- list(xtol_rel = xtol_rel, maxeval = maxeval, maxtime = maxtime)
    res1 <- nloptr::bobyqa(x0 = par_init, fn = obj, lower = lower, upper = upper, control = ctrl)
    res2 <- nloptr::bobyqa(x0 = res1$par, fn = obj, lower = lower, upper = upper, control = ctrl)
    list(lambda = lambda, value = res2$value, par = res2$par,
         convergence = res2$convergence, iter = res2$iter,
         value1 = res1$value, delta_value = res2$value - res1$value,
         max_abs_gamma = max(abs(res2$par[-(1:4)])), phase = "quantile")
}

run_sample <- build_run_sample("lag_2_cal_W", trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
RcppParallel::setThreadOptions(numThreads = 10)

LAMBDA_GRID <- c(1e-9, 2.15e-9, 4.64e-9, 1e-8, 2.15e-8, 4.64e-8, 1e-7, 2.15e-7, 4.64e-7,
                  1e-6, 2.15e-6, 4.64e-6, 1e-5, 2.15e-5, 4.64e-5, 1e-4)

par_lagm <- c(3.2411576554474, 4.1538810604154, 0.508582490563503, 0.00064447638164912,
              0.000266931500722254, -0.00320709266345527, 0.0173026816261523, 0.0671075607612255,
              0.0361581141572298, -0.331783440956923, 8.08433700184273e-06, 3.21236234943358e-11,
              0.0933072166415534)

cat(sprintf("[lag_2_cal_W, A, allpoints-lagmseed] n=%d (%d corner), n_burn=500, n_keep=1000\n",
            nrow(run_sample), sum(run_sample$corner)))

fits <- vector("list", length(LAMBDA_GRID))
for (i in seq_along(LAMBDA_GRID)) {
    lam <- LAMBDA_GRID[i]
    t0 <- Sys.time()
    fits[[i]] <- fit_one_lambda_A(lam, run_sample, n_burn = 500, n_keep = 1000, par_init = par_lagm)
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    f <- fits[[i]]
    cat(sprintf("  lambda=%.3g: Lhat=%.6g (pass1=%.6g, delta=%.3g), delta1=%.4g, delta2=%.4g, conv=%d, iter=%d, max|gamma|=%.4g, time=%.1fs\n",
                f$lambda, f$value, f$value1, f$delta_value, f$par[2], f$par[3], f$convergence, f$iter, f$max_abs_gamma, dt))
}

save(fits, par_lagm, LAMBDA_GRID, file = "Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData")
cat("\nSaved: Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData\n")
