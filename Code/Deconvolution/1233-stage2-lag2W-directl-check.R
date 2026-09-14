## Global-optimizer check for lag_2_cal_W, moment set A (9 moments), trim=0.5% --
## (2026-09-08) The multi-start check (1232) showed seeding from lag_m's own
## fit finds a materially better (lower Lhat) AND sign-flipped (delta1,
## delta2>0) optimum than the naive ascending warm-start chain, at least at
## lambda=2.15e-8 and 1e-7. But "use lag_m's fit because I liked the sign" is
## not a defensible justification on its own -- it invites exactly the
## question "did you try a global optimizer first?". This script answers
## that directly: NLOPT_GN_DIRECT_L (deterministic, no seed to justify, no
## tuning beyond bounds/budget -- the simplest global option available) as
## PASS 1, replacing the current naive-chain-warm-started LOCAL BOBYQA pass 1
## -- SAME two-pass structure as fit_one_lambda_A everywhere else in this
## project, pass 2 is untouched (local BOBYQA, gamma still fully unbounded).
## Tested on the SAME 3 lambdas as 1232 for direct comparability.
##
## Bounds for the global pass only (pass 2 stays unbounded on gamma, as
## always): delta0-2 keep the existing DELTA_BOUND=60 box (already used
## throughout this project); gamma boxed at +-50 -- roughly 10-15x the
## largest |gamma| observed in ANY fit so far (coarse/fine/multistart all
## topped out under ~5) -- generous enough not to bind, finite enough to keep
## DIRECT_L's search space reasonable.

library(tidyverse)
library(Rcpp)
library(nloptr)
library(RcppParallel)

sourceCpp("Code/Rcpp/1200-stage2-elvis.cpp")
load("Code/Products/1200-stage2-data.RData")

DELTA_BOUND <- 60
GAMMA_BOUND_GLOBAL <- 50

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

## PASS 1 = NLOPT_GN_DIRECT_L (global, bounded gamma), PASS 2 = local BOBYQA
## (unbounded gamma) -- same two-pass count as the existing local-local
## fit_one_lambda_A, just swapping what runs first.
fit_one_lambda_A_global <- function(lambda, run_sample, n_burn, n_keep,
                                     xtol_rel = 1e-4, maxeval_global = 1000, maxtime_global = 300,
                                     maxeval_local = 2000, maxtime_local = 90) {
    dat <- run_sample
    obj <- function(par) cue_objective_A(par, lambda, dat, n_burn, n_keep)

    lower_g <- c(rep(-DELTA_BOUND, 3), 0,     rep(-GAMMA_BOUND_GLOBAL, 9))
    upper_g <- c(rep( DELTA_BOUND, 3), 0.999, rep( GAMMA_BOUND_GLOBAL, 9))
    t0 <- Sys.time()
    res1 <- nloptr::directL(fn = obj, lower = lower_g, upper = upper_g,
                             control = list(maxeval = maxeval_global, maxtime = maxtime_global))
    t1 <- as.numeric(Sys.time() - t0, units = "secs")

    lower_l <- c(rep(-DELTA_BOUND, 3), 0,     rep(-Inf, 9))
    upper_l <- c(rep( DELTA_BOUND, 3), 0.999, rep( Inf, 9))
    ctrl_l <- list(xtol_rel = xtol_rel, maxeval = maxeval_local, maxtime = maxtime_local)
    t0 <- Sys.time()
    res2 <- nloptr::bobyqa(x0 = res1$par, fn = obj, lower = lower_l, upper = upper_l, control = ctrl_l)
    t2 <- as.numeric(Sys.time() - t0, units = "secs")

    list(lambda = lambda, value = res2$value, par = res2$par,
         convergence = res2$convergence, iter = res2$iter,
         value1 = res1$value, delta_value = res2$value - res1$value,
         max_abs_gamma = max(abs(res2$par[-(1:4)])),
         time_global = t1, time_local = t2)
}

run_sample <- build_run_sample("lag_2_cal_W", trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
RcppParallel::setThreadOptions(numThreads = 10)

LAMBDAS <- c(2.15e-8, 1e-7, 1e-6)   # same 3 points as 1232, for direct comparability

results <- list()
for (lam in LAMBDAS) {
    fit <- fit_one_lambda_A_global(lam, run_sample, n_burn = 500, n_keep = 1000)
    cat(sprintf("== lambda=%.3g == Lhat=%.6g (DIRECT_L pass1=%.6g, %.1fs; BOBYQA pass2 %.1fs), delta1=%.4g, delta2=%.4g, conv=%d, iter=%d, max|gamma|=%.4g\n",
                lam, fit$value, fit$value1, fit$time_global, fit$time_local, fit$par[2], fit$par[3], fit$convergence, fit$iter, fit$max_abs_gamma))
    results[[as.character(lam)]] <- fit
}

save(results, LAMBDAS, file = "Code/Products/1233-stage2-lag2W-directl-check.RData")
cat("\nSaved: Code/Products/1233-stage2-lag2W-directl-check.RData\n")
