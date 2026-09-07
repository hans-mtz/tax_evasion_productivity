## Stage-2 ELVIS -- cheap concave-q diagnostic (2026-09-05) -----------------
## Tests whether the linear-q result (lambda pinned near 0, apparently by a
## few extreme-e firms whose ceiling 2*lambda*e->1 makes h(e) blow up --
## confirmed this session via the firm-exclusion trim check, CLAUDE.md) is
## specific to the linear detection function, or survives under the concave
## exponential q=1-exp(-lambda*e), whose own ceiling (lambda*e<1) sits at
## TWICE the lambda*e value of linear's (lambda*e<1/2) -- see CLAUDE.md's
## same-date derivative-comparison entry.
##
## Deliberately cheap and standalone: a coarse widely-spaced grid on the
## UNTRIMMED sample (no trimming -- the question is whether concave q ALONE
## fixes the identification problem, without also discarding data), with a
## PROPER test inversion (same Theorem F.1 chi^2 shortcut used throughout
## this project) computed within this run's own sample/grid -- not a
## point-estimate comparison. This is the "before committing to a full level
## sim" cheap pass; only escalate to a Phase-0-style continuous-chain sweep
## if this shows a real, order-of-magnitude shift.
##
## Standalone rather than sourcing 1211-stage2-elvis-driver-AB.R directly:
## that file's CLI tail executes unconditionally when sourced (would run a
## default toy fit as a side effect). Minimal re-implementation of just what's
## needed (build_run_sample-equivalent filter, CUE objective, fit_one_lambda)
## to avoid that, at the cost of some duplication -- acceptable for a
## diagnostic script, not the production driver.

library(tidyverse)
library(Rcpp)
library(nloptr)
library(RcppParallel)

source("Code/Deconvolution/utils-cli.R")
sourceCpp("Code/Rcpp/1200-stage2-elvis-concave.cpp")
load("Code/Products/1200-stage2-data.RData")            # stage2_data
load("Code/Products/1205-stage2-warmstart.RData")       # warmstart_lag_m, warmstart_lag2W (delta0-2 reused as a starting point)

DEFAULTS <- list(
    ins         = "lag_m",
    n_burn      = 500,
    n_keep      = 1000,
    n_cores     = 3,
    maxeval_b   = 1000,
    lambda_grid = c(1e-8, 3.6e-8, 1e-7, 1e-6, 1e-5, 1e-4)  # CLI-overridable, comma-separated
)
opt <- parse_cli_args(DEFAULTS)
opt$n_burn    <- as.integer(opt$n_burn)
opt$n_keep    <- as.integer(opt$n_keep)
opt$n_cores   <- as.integer(opt$n_cores)
opt$maxeval_b <- as.integer(opt$maxeval_b)
log_run_header("1220-stage2-concave-diag.R", opt)
RcppParallel::setThreadOptions(numThreads = opt$n_cores)

## %% Sample -- corner_mode="include_zero", untrimmed, same as Phase 0 -------
base <- stage2_data %>% dplyr::filter(ins == opt$ins, !corp)
interior <- base %>% dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>% mutate(corner = 0L)
corner_obs <- base %>% dplyr::filter(is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases == 0) %>% mutate(corner = 1L)
run_sample <- bind_rows(interior, corner_obs) %>%
    dplyr::select(plant, year, sic_3, M_star, cal_V, tilde_cal_W, sales_tax_rate_purchases, beta, corner) %>%
    mutate(.row_id = dplyr::row_number())
cat(sprintf("[%s] n=%d (%d corner)\n", opt$ins, nrow(run_sample), sum(run_sample$corner)))

## %% CUE objective (identical construction to 1211's, moment-count-agnostic) -
cue_objective_from_moments <- function(dvec, Omega) {
    eig <- eigen(Omega, symmetric = TRUE)
    tol <- max(eig$values) * 1e-10
    pos <- eig$values > tol
    A <- eig$vectors[, pos, drop = FALSE]
    d2 <- t(A) %*% dvec
    0.5 * sum(d2^2 / eig$values[pos])
}
cue_objective_common <- function(Ghat) cue_objective_from_moments(colMeans(Ghat), cov(Ghat))

cue_objective_A_concave <- function(par, lambda, dat, n_burn, n_keep) {
    delta0 <- par[1]; delta1 <- par[2]; delta2 <- par[3]; eta <- par[4]
    gamma  <- par[-(1:4)]
    Ghat <- mh_tilted_average_A_concave_cpp(
        dat$M_star, dat$cal_V, dat$tilde_cal_W, dat$sales_tax_rate_purchases,
        dat$.row_id, dat$beta, lambda, delta0, delta1, delta2, eta, gamma, dat$corner,
        n_burn, n_keep
    )
    cue_objective_common(Ghat)
}

DELTA_BOUND <- 20

fit_one_lambda_A_concave <- function(lambda, run_sample, n_burn, n_keep, par_init) {
    obj <- function(par) cue_objective_A_concave(par, lambda, run_sample, n_burn, n_keep)
    lower <- c(-DELTA_BOUND, -DELTA_BOUND, -DELTA_BOUND, 0, rep(-Inf, 8))
    upper <- c( DELTA_BOUND,  DELTA_BOUND,  DELTA_BOUND, 0.999, rep( Inf, 8))
    par_init <- pmin(pmax(par_init, lower), upper)
    res <- nloptr::bobyqa(x0 = par_init, fn = obj, lower = lower, upper = upper,
                           control = list(xtol_rel = 1e-4, maxeval = 1000))
    list(lambda = lambda, value = res$value, par = res$par, n = nrow(run_sample),
         convergence = res$convergence, iter = res$iter,
         max_abs_gamma = max(abs(res$par[-(1:4)])))
}

## %% Coarse grid -- centered where CONCAVE's ceiling (lambda*e<1) starts to
## matter, i.e. roughly 2x the linear-q values used in the trim check (whose
## ceiling is lambda*e<1/2) -- not reusing the exact same points blind.
lambda_grid <- sort(as.numeric(opt$lambda_grid))
par_init0 <- c(warmstart_lag_m$par[["delta0"]], warmstart_lag_m$par[["delta1"]], warmstart_lag_m$par[["delta2"]], 0, rep(0, 8))
if (opt$ins == "lag_2_cal_W") par_init0 <- c(warmstart_lag2W$par[["delta0"]], warmstart_lag2W$par[["delta1"]], warmstart_lag2W$par[["delta2"]], 0, rep(0, 8))

fits <- vector("list", length(lambda_grid))
par_init <- par_init0
for (i in seq_along(lambda_grid)) {
    fits[[i]] <- fit_one_lambda_A_concave(lambda_grid[i], run_sample, opt$n_burn, opt$n_keep, par_init)
    if (!is.na(fits[[i]]$value)) par_init <- fits[[i]]$par
    f <- fits[[i]]
    cat(sprintf("  lambda=%.3g: Lhat=%.6g, conv=%d, iter=%d, max|gamma|=%.4g\n",
                f$lambda, f$value, f$convergence, f$iter, f$max_abs_gamma))
}

## %% Test inversion, same Theorem F.1 chi^2 shortcut as everywhere else -----
d_g <- 8
n <- nrow(run_sample)
Lhat <- sapply(fits, `[[`, "value")
Lhat_min <- min(Lhat)
crit <- qchisq(0.95, df = d_g)
threshold <- Lhat_min + crit / (2 * n)
passing <- lambda_grid[Lhat <= threshold]
cat(sprintf("\n---- Concave-q test inversion [%s], d_g=%d, n=%d ----\n", opt$ins, d_g, n))
cat(sprintf("Lhat_min=%.6g at lambda=%.3g; threshold=%.6g\n", Lhat_min, lambda_grid[which.min(Lhat)], threshold))
cat(sprintf("Largest not-rejected lambda: %.4g\n", max(passing)))
cat(sprintf("Smallest not-rejected lambda (within this grid): %.4g\n", min(passing)))

save(fits, run_sample_n = n, lambda_grid, Lhat, threshold, opt,
     file = sprintf("Code/Products/1220-stage2-concave-diag-%s.RData", opt$ins))
cat(sprintf("\nSaved: Code/Products/1220-stage2-concave-diag-%s.RData\n", opt$ins))
