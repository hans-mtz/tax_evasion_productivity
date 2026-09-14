## Optimizer-swap check, lambda=4.64e-8, lag_m, moment set A (2026-09-09) ----
## Same combined (delta0,delta1,delta2,eta,gamma) vector, same bounds, same
## x0, same n_burn/n_keep/maxeval/xtol_rel/maxtime as the BOBYQA fit already
## on hand -- ONLY the algorithm changes (bounded Nelder-Mead via
## nloptr::neldermead, per Schennach ELVIS_supplement.pdf p.28's own choice
## for her gamma sub-problem -- here applied, per the user's explicit scope,
## to the SAME joint problem we already solve with BOBYQA, not a redesign
## into her fully-separated theta/gamma optimization). Reports iterations,
## wall time, and Lhat for direct comparison against the BOBYQA result.

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
        n_burn, n_keep, base_seed = 20260829
    )
    cue_objective_common(Ghat)
}

fit_neldermead <- function(lambda, run_sample, n_burn, n_keep, par_init,
                            xtol_rel = 1e-4, maxeval = 2000, maxtime = 1800) {
    dat <- run_sample
    obj <- function(par) cue_objective_A(par, lambda, dat, n_burn, n_keep)
    lower <- c(rep(-DELTA_BOUND, 3), 0,     rep(-Inf, 9))
    upper <- c(rep( DELTA_BOUND, 3), 0.999, rep( Inf, 9))
    ctrl <- list(xtol_rel = xtol_rel, maxeval = maxeval, maxtime = maxtime)
    t0 <- Sys.time()
    res1 <- nloptr::neldermead(x0 = par_init, fn = obj, lower = lower, upper = upper, control = ctrl)
    res2 <- nloptr::neldermead(x0 = res1$par, fn = obj, lower = lower, upper = upper, control = ctrl)
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    list(lambda = lambda, value = res2$value, par = res2$par,
         value1 = res1$value, convergence = res2$convergence, iter = res2$iter,
         iter1 = res1$iter, time_seconds = dt)
}

run_sample <- build_run_sample("lag_m", trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
RcppParallel::setThreadOptions(numThreads = 4)

par_init <- c(3.2411576554474, 4.1538810604154, 0.508582490563503, 0.00064447638164912,
              0.000266931500722254, -0.00320709266345527, 0.0173026816261523, 0.0671075607612255,
              0.0361581141572298, -0.331783440956923, 8.08433700184273e-06, 3.21236234943358e-11,
              0.0933072166415534)

fit <- fit_neldermead(4.64e-8, run_sample, n_burn = 1000, n_keep = 3000, par_init = par_init)
cat(sprintf("NELDER-MEAD: lambda=4.64e-8  Lhat=%.8g (pass1=%.8g)  delta1=%.6g delta2=%.6g  conv=%d  iter=%d (pass1=%d)  time=%.1fs\n",
            fit$value, fit$value1, fit$par[2], fit$par[3], fit$convergence, fit$iter, fit$iter1, fit$time_seconds))
save(fit, file = "Code/Products/1254-lambda-neldermead-test.RData")
