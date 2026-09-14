## Multi-start diagnostic for lag_2_cal_W, moment set A (9 moments), trim=0.5% ---
## (2026-09-08) lag_2_cal_W's coarse grid came back with a shallow, barely-
## bracketed minimum and delta1,delta2<0 throughout -- unlike lag_m, whose
## sharp dip flipped both positive. Before concluding this is a genuine
## feature of lag_2_cal_W's own likelihood surface (vs. the naive ascending
## warm-start chain getting stuck in a worse local optimum), check whether
## DIFFERENT starting points at the SAME lambda land in meaningfully
## different places. Three starts x three lambdas (one at/near the current
## passing band's lower edge, one at the argmin, one at/near the upper edge):
##   (1) naive-chain baseline -- already computed (the existing coarse-grid
##       fit at that lambda, itself descended from the plain GMM warmstart
##       file via sequential ascending chaining) -- NOT recomputed here.
##   (2) lag_m's own best-fit par (full 13-dim vector, lambda=1e-6, the
##       "sensible sign" fit) used AS-IS as x0 -- not economically motivated
##       (different instrument's own gamma), purely an exploration probe.
##   (3) a random vector, SAVED for reproducibility (delta0-2 ~ Unif(-5,5),
##       eta ~ Unif(0,0.05), gamma[1:9] ~ N(0,1), fixed seed).
## Same fit_one_lambda_A machinery as the main driver (1211), copied inline
## since 1211 isn't structured as a sourceable module.

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
         max_abs_gamma = max(abs(res2$par[-(1:4)])))
}

run_sample <- build_run_sample("lag_2_cal_W", trim_top_pct = 0.005) %>% mutate(.row_id = dplyr::row_number())
RcppParallel::setThreadOptions(numThreads = 10)

LAMBDAS <- c(2.15e-8, 1e-7, 1e-6)   # below / at argmin / above (current passing-band edges)

## Start 2: lag_m's own best-fit par (verbatim, exploration probe only)
par_lagm <- c(3.2411576554474, 4.1538810604154, 0.508582490563503, 0.00064447638164912,
              0.000266931500722254, -0.00320709266345527, 0.0173026816261523, 0.0671075607612255,
              0.0361581141572298, -0.331783440956923, 8.08433700184273e-06, 3.21236234943358e-11,
              0.0933072166415534)

## Start 3: random, saved for reproducibility
set.seed(20260908)
par_random <- c(runif(3, -5, 5), runif(1, 0, 0.05), rnorm(9, 0, 1))
writeLines(paste(par_random, collapse = ","), "Code/Products/1232-stage2-lag2W-multistart-randompar.txt")
cat("Random start (seed=20260908), saved to Code/Products/1232-stage2-lag2W-multistart-randompar.txt:\n")
cat(paste(round(par_random, 6), collapse = ", "), "\n\n")

results <- list()
for (lam in LAMBDAS) {
    cat(sprintf("== lambda=%.3g ==\n", lam))
    for (start_name in c("lag_m_fit", "random")) {
        par0 <- if (start_name == "lag_m_fit") par_lagm else par_random
        t0 <- Sys.time()
        fit <- fit_one_lambda_A(lam, run_sample, n_burn = 500, n_keep = 1000, par_init = par0)
        dt <- as.numeric(Sys.time() - t0, units = "secs")
        cat(sprintf("  [%s] Lhat=%.6g (pass1=%.6g), delta1=%.4g, delta2=%.4g, conv=%d, iter=%d, max|gamma|=%.4g, time=%.1fs\n",
                     start_name, fit$value, fit$value1, fit$par[2], fit$par[3], fit$convergence, fit$iter, fit$max_abs_gamma, dt))
        results[[paste(lam, start_name, sep = "_")]] <- fit
    }
}

save(results, par_lagm, par_random, LAMBDAS, file = "Code/Products/1232-stage2-lag2W-multistart-check.RData")
cat("\nSaved: Code/Products/1232-stage2-lag2W-multistart-check.RData\n")
