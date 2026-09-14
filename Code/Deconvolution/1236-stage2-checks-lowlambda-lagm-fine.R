## Two small independent-seed checks, moment set A (9 moments), trim=0.5% ----
## (2026-09-08)
## (1) lag_2_cal_W: does the flat/passing region (currently the entire
##     1e-9..4.64e-7 span of the lag_m-seeded grid) turn around below 1e-9,
##     or keep extending? 3 widely-spaced points (1e-10, 1e-12, 1e-14), each
##     seeded independently from lag_m's own best-fit par (same design that
##     worked for the main 16-point grid -- NOT chained to each other).
## (2) lag_m: does seeding EVERY point independently from lag_m's own best
##     fit (rather than the sequential ascending chain that produced jagged
##     neighbors near lambda=1e-6) fix that jaggedness? Same 4 lambda values
##     the earlier chained fine grid found erratic at (6.51e-7, 8.39e-7,
##     1.21e-6, 1.59e-6), re-fit independently from lag_m's own best-fit par.
## Both are cheap checks (~7 fits total) -- if either surfaces something
## interesting, extend; if not, move on to the (delta1,delta2) grid.

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
         max_abs_gamma = max(abs(res2$par[-(1:4)])))
}

RcppParallel::setThreadOptions(numThreads = 10)

par_lagm <- c(3.2411576554474, 4.1538810604154, 0.508582490563503, 0.00064447638164912,
              0.000266931500722254, -0.00320709266345527, 0.0173026816261523, 0.0671075607612255,
              0.0361581141572298, -0.331783440956923, 8.08433700184273e-06, 3.21236234943358e-11,
              0.0933072166415534)

run_check <- function(ins_choice, lambdas, par_init, label) {
    dat <- build_run_sample(ins_choice, trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
    cat(sprintf("== %s (%s), n=%d ==\n", label, ins_choice, nrow(dat)))
    out <- vector("list", length(lambdas))
    for (i in seq_along(lambdas)) {
        lam <- lambdas[i]
        t0 <- Sys.time()
        out[[i]] <- fit_one_lambda_A(lam, dat, n_burn = 500, n_keep = 1000, par_init = par_init)
        dt <- as.numeric(Sys.time() - t0, units = "secs")
        f <- out[[i]]
        cat(sprintf("  lambda=%.3g: Lhat=%.6g (pass1=%.6g), delta1=%.4g, delta2=%.4g, conv=%d, iter=%d, max|gamma|=%.4g, time=%.1fs\n",
                    f$lambda, f$value, f$value1, f$par[2], f$par[3], f$convergence, f$iter, f$max_abs_gamma, dt))
    }
    out
}

## Check 1: lag_2_cal_W, does the flat region turn around below 1e-9?
lag2W_lowlambda <- run_check("lag_2_cal_W", c(1e-10, 1e-12, 1e-14), par_lagm, "lag_2_cal_W low-lambda extension")

## Check 2: lag_m, independent-seed fine grid around its own optimum
lagm_fine <- run_check("lag_m", c(6.51e-7, 8.39e-7, 1.21e-6, 1.59e-6), par_lagm, "lag_m independent-seed fine check")

save(lag2W_lowlambda, lagm_fine, par_lagm, file = "Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData")
cat("\nSaved: Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData\n")
