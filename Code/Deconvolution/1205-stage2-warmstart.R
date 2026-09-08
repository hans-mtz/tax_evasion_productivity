## Stage-2 ELVIS warm start ------------------------------------------------
## Crude, biased-but-cheap first guess for theta=(lambda,delta0,delta1,delta2,
## sigma2_psi), in the spirit of GNR's own first-stage regression before its
## GMM step, and AK2020's convex-QP warm start before their MCMC/BOBYQA loop.
##
## 2026-08-28 redesign: replaces the single global constant (median epsilon
## from ALL corp firms, shared by every unincorp firm regardless of industry
## or idiosyncratic shock) with S per-firm draws of eps, bootstrap-resampled
## from THAT firm's own industry's CORP epsilon pool (falling back to the
## ins-level pooled pool for thin industries, using the SAME classification
## 1206-stage2-eps-targets.R already computed -- loaded here rather than
## re-derived, so the two files can't silently disagree on which industries
## are "thin"). This is the naive-averaging construction CLAUDE.md documents
## as INVALID for the FINAL estimator (the simulated average converges to a
## population-averaged psi, not the firm's true realized psi_it, since psi is
## nonlinear in eps and the average is missing ELVIS's posterior tilt) --
## used here ONLY to get BOBYQA a reasonable starting point, which doesn't
## need to be consistent, only close. Firm-varying draws (instead of one
## constant) directly address the 2026-08-27 finding that a single global
## median-epsilon plug-in gave e_hat<0 (clipped to 0) for 44.4% of unincorp
## firm-periods: that 44.4% was driven by each firm's OWN unobserved
## eps_it differing from the population median, which per-firm sampling now
## approximates instead of ignoring.
##
## sigma2_psi is NOT separately optimized in the fit -- it's psi's own
## defining moment (E[psi^2]=sigma2_psi in the real ELVIS system), so once
## delta0-2 are fit, sigma2_psi_init falls out as mean(psi_hat^2) on the
## fitted residuals, a free byproduct, no extra parameter to search over here.
##
## Moments used, explicit GMM (2026-08-28 revision): this warm start is
## structurally more GMM than ELVIS -- no gamma/tilt per moment, just
## solve/minimize an equal-weighted g(theta) directly for (lambda,delta0,
## delta1,delta2), exactly identified (4 moments, 4 parameters). Picked
## E[psi]=0, E[om*psi]=0, E[psi*om^2]=0, E[psi*ln(M)]=0 -- 3 of these
## (om*psi, psi*om^2) previously fell out "for free" as an unstated
## byproduct of minimizing sum(psi^2) via OLS normal equations (omega,
## omega^2 are literally the linear regressors in that SSR) -- true, but
## implicit and not something the code actually named or computed, and
## psi*ln(M) genuinely wasn't there at all (ln M isn't a term in psi's own
## formula, so no analogous free lunch). All four are now explicit: g(theta)
## is computed by name, and the objective is sum(g(theta)^2).

library(tidyverse)

source("Code/Deconvolution/utils-cli.R")

DEFAULTS <- list(S = 250, seed = 20260828, maxeval_nm = 2000, ins_only = "")
opt <- parse_cli_args(DEFAULTS)
opt$S          <- as.integer(opt$S)
opt$seed       <- as.integer(opt$seed)
opt$maxeval_nm <- as.integer(opt$maxeval_nm)   # nloptr::neldermead's maxeval; 2000 default, raised via CLI
                                                # to test how long lag_2_cal_W needs to actually reach xtol
                                                # (2026-09-07: hit the 2000 cap at exp(h')'s first try)
                                                # ins_only: "" = run both ins choices (default); "lag_m" or
                                                # "lag_2_cal_W" = run just that one (for a targeted rerun,
                                                # e.g. testing a higher maxeval_nm without paying for both)
log_run_header("1205-stage2-warmstart.R", opt)

load("Code/Products/1200-stage2-data.RData")           # stage2_data
load("Code/Products/1206-stage2-eps-targets.RData")    # eps_targets (also gives the thin-industry classification)

## Per-ins seed offsets: get_warmstart() calls set.seed() itself (below),
## keyed off this table, so each ins choice's draws are reproducible in
## isolation regardless of call order -- fixes a real bug (not just a missed
## nicety): with a single global set.seed(opt$seed) call up top (the prior
## version), the two get_warmstart() calls consumed one shared RNG stream
## sequentially, so re-running just "lag_2_cal_W" alone with the same seed
## gave DIFFERENT draws than it got when "lag_m" ran first.
INS_SEED_OFFSET <- c(lag_m = 1L, lag_2_cal_W = 2L)

get_warmstart <- function(ins_choice, S = opt$S, data = stage2_data) {
    set.seed(opt$seed + INS_SEED_OFFSET[[ins_choice]])
    run_data <- data %>% dplyr::filter(ins == ins_choice)

    ## %% Per-industry CORP epsilon resampling pools -------------------------
    ## Reuses 1206's pooled_fallback classification (thin industries, n<30
    ## CORP obs, use the ins-level pooled pool instead of their own).
    corp_eps    <- run_data %>% dplyr::filter(corp, is.finite(epsilon))
    pooled_pool <- corp_eps$epsilon
    pools_by_sic <- split(corp_eps$epsilon, corp_eps$sic_3)

    fallback_sics <- eps_targets %>%
        dplyr::filter(ins == ins_choice, pooled_fallback) %>%
        dplyr::pull(sic_3) %>%
        as.character()

    get_pool <- function(sic_chr) {
        if (sic_chr %in% fallback_sics) return(pooled_pool)
        p <- pools_by_sic[[sic_chr]]
        if (is.null(p) || length(p) == 0) pooled_pool else p
    }

    ## %% h(e) benefit shifter: sales_tax_rate_purchases (tau_P) -- unchanged
    ## from the prior version (Paper/sections/9999-tax-wedge.qmd).
    unincorp <- run_data %>%
        dplyr::filter(!corp, is.finite(sales_tax_rate_purchases), sales_tax_rate_purchases > 0) %>%
        mutate(.row_id = dplyr::row_number(), sic_chr = as.character(sic_3))

    n_firms <- nrow(unincorp)

    ## %% Draw S eps per firm, vectorized by industry (loop over ~29 sic_3 -----
    ## groups, not n_firms individual firms): for each industry, draw an
    ## (n_j x S) matrix of bootstrap samples from that industry's pool in one
    ## call, then stack. as.vector() on a matrix goes column-major (draw 1 for
    ## every firm, then draw 2 for every firm, ...), matching rep(idx, times=S).
    stacked_list <- vector("list", length(unique(unincorp$sic_chr)))
    names(stacked_list) <- unique(unincorp$sic_chr)
    for (sic_chr in names(stacked_list)) {
        idx  <- which(unincorp$sic_chr == sic_chr)
        pool <- get_pool(sic_chr)
        n_j  <- length(idx)
        # eps_mat <- matrix(sample(pool, n_j * S, replace = TRUE), nrow = n_j, ncol = S)
        eps_mat <- matrix((quantile(pool, probs = runif(n_j * S), names = FALSE, na.rm = TRUE)), nrow = n_j, ncol = S)  # alternative: quantile-based resampling (2026-08-29)
        stacked_list[[sic_chr]] <- unincorp[rep(idx, times = S), ] %>%
            mutate(eps_s = as.vector(eps_mat))
    }
    stacked <- bind_rows(stacked_list) %>%
        mutate(
            M_hat     = pmin(M_star * exp(-(cal_V + eps_s)), M_star),
            e_hat     = M_star - M_hat,
            omega_hat = tilde_cal_W - (1 - beta) * eps_s
        ) %>%
        dplyr::filter(is.finite(e_hat), e_hat >= 0)

    cat(sprintf(
        "  [%s] %d unincorp firms x S=%d draws = %d rows, %d (%.1f%%) dropped (e_hat<0 draws)\n",
        ins_choice, n_firms, S, n_firms * S,
        n_firms * S - nrow(stacked), 100 * (n_firms * S - nrow(stacked)) / (n_firms * S)
    ))

    ## %% Feasible starting lambda -------------------------------------------
    ## Still a one-sided feasibility constraint (2*lambda*e_hat<1) that needs
    ## to hold for nearly the whole (now firm-and-draw-varying) sample.
    ## unname() matters here, not just style: quantile()'s result carries a
    ## "99%" name, and c(lambda=<named value>) would otherwise concatenate to
    ## "lambda.99%" instead of "lambda" -- silently breaking fit$par[["lambda"]]
    ## below (this exact bug existed in the pre-redesign version too, just
    ## never exercised since nothing there did name-based lookup on lambda).
    ##
    ## 2026-08-31: reverted a same-day median-denominator experiment
    ## (lambda_min=0.001/median(e_hat), lambda0=0.25/median(e_hat)) that
    ## started the search ~46% clipped (arg=pmax(1-2*lambda*e_hat,1e-6) at the
    ## floor) and had lambda_max exactly 50% clipped by construction -- a
    ## heavily kinked, half-flat objective that made L-BFGS-B's numerical
    ## gradient grind for ~40min instead of the usual ~1min (measured directly
    ## on real data before reverting). Back to the P99-based start, which sits
    ## deep in the feasible region (<1% clipped).
    lambda0    <- unname(1 / (2 * quantile(stacked$e_hat, 0.75, na.rm = TRUE))) # 0.99
    lambda_max <- unname(0.999 / (2 * median(stacked$e_hat, na.rm = TRUE)))
    lambda_min <- unname(1 / (2 * max(stacked$e_hat, na.rm = TRUE))) # 1e-12  # historical fixed lower bound; lambda is gridded independently downstream (1211), not seeded from here
    ## %% Explicit 4-moment GMM fit on the stacked (firm x draw) sample -------
    ## g(theta) = (E[psi], E[om*psi], E[psi*om^2], E[psi*ln M]) -- exactly
    ## identified (4 moments, 4 parameters), equal-weighted (no gamma/tilt:
    ## this warm start is plain GMM, not ELVIS). ln(M_hat), not raw M_hat --
    ## matches the real system (Code/Rcpp/1200-stage2-elvis.cpp): log scale,
    ## consistent with every other row.
    lnM_hat <- log(stacked$M_hat)

    gmm_obj <- function(par) {
        lambda <- par[1]; delta0 <- par[2]; delta1 <- par[3]; delta2 <- par[4]
        arg     <- pmax(1 - 2*lambda*stacked$e_hat, 1e-6)   # clip infeasible draws instead of erroring
        h       <- log(stacked$sales_tax_rate_purchases) + log(arg)
        ## h_prime reuses the SAME clipped `arg` as h (2026-09-07 fix), not the
        ## raw unclipped denominator -- the two must agree, or h_prime reports a
        ## huge nonzero slope exactly where the clip has made h locally flat in
        ## lambda. This also removes the near-zero-denominator blowup: lambda_min
        ## is defined as 1/(2*max(e_hat)), which is an EXACT root of the raw
        ## denominator for the firm/draw achieving max(e_hat) (1-2*lambda_min*
        ## e_max = 1-1 = 0) -- the old `== 0` guard never caught this in practice
        ## (floating-point rounding lands near, not at, exactly 0, so h_prime
        ## came back finite-but-astronomical, e.g. ~1e13-1e23, silently passing
        ## every is.finite() check while still swamping mean(h_prime)/sd(h_prime)
        ## for every other observation). Bounded now by construction: |h_prime|
        ## <= 2*e_hat/1e-6, large for extreme e_hat but never singular.
        h_prime   <- -2 * stacked$e_hat / arg
        ## exp(h_prime), not z-score standardization (2026-09-07): with arg
        ## floored above, h_prime <= 0 always (e>=0, arg>0), so exp(h_prime) is
        ## bounded in (0,1] with NO dependence on the rest of the sample -- a
        ## pointwise transform of this one observation's own e, unlike the
        ## z-score version (mean(h_prime)/sd(h_prime) recomputed at every
        ## candidate lambda, which changed the objective's shape at every
        ## draw whose denom fell below the floor simultaneously and made
        ## L-BFGS-B grind, see chat). The earlier "exp blew up" failure was
        ## from exponentiating the RAW unclipped h_prime, which could be
        ## unboundedly POSITIVE for draws past the ceiling (raw denom<0) --
        ## the floor removes that possibility entirely, since arg can't go
        ## negative.
        h_prime_exp <- exp(h_prime)
        stopifnot(
            "1/lambda is not finite"=is.finite(1/lambda),
            "e_hat is not finite"=all(is.finite(stacked$e_hat)),
            "h' is not finite"=all(is.finite(h_prime)),
            "exp(h') is not finite"=all(is.finite(h_prime_exp))
            )
        psi_hat <- h - delta0 + delta1*stacked$omega_hat - delta2*stacked$omega_hat^2
        g <- c(
            mean(psi_hat),
            mean(psi_hat * stacked$omega_hat),
            mean(psi_hat * stacked$omega_hat*stacked$omega_hat), # was ^2 (faster?)
            mean(psi_hat * h_prime_exp) # score moment for lambda (bounded exp(h') transform); was psi*lnM_hat
        )
        sum(g^2)
    }

    ## nloptr::neldermead, not stats::optim(method="L-BFGS-B") (2026-09-07):
    ## L-BFGS-B's line search failed outright for lag_2_cal_W (convergence=52,
    ## "ABNORMAL_TERMINATION_IN_LNSRCH") even after the h_prime floor+exp fix --
    ## the arg<-pmax(...,1e-6) floor still introduces a kink (a threshold lambda
    ## per observation where it crosses onto the floor), and with millions of
    ## distinct e_hat values scattered across the search range, the numerically
    ## finite-differenced gradient L-BFGS-B relies on can be locally unreliable
    ## near enough of them. Nelder-Mead never estimates a gradient (compares
    ## function VALUES at simplex vertices only), so it has no line search to
    ## fail -- it can stall or converge slowly on a rough surface, but not error
    ## out this way. Using nloptr's (not base R's) implementation specifically
    ## because stats::optim(method="Nelder-Mead") silently IGNORES lower/upper
    ## (base R only respects bounds for L-BFGS-B/Brent) -- would have quietly
    ## stopped enforcing lambda_min/lambda_max and the delta box.
    fit0 <- nloptr::neldermead(
        x0     = c(lambda0, 0, 0, 0),
        fn     = gmm_obj,
        lower  = c(lambda_min, -50, -50, -50),
        upper  = c(lambda_max, 50, 50, 50),
        control = list(maxeval = opt$maxeval_nm)  # generous vs. bobyqa's typical ~150-250 evals: Nelder-Mead
                                         # usually needs more function calls than a quasi-Newton method
                                         # on a smooth problem, but each call has no extra gradient
                                         # evaluations (L-BFGS-B's finite-difference gradient costs
                                         # n_par+1 extra calls per step on top of the line search itself)
    )
    fit <- list(par = setNames(fit0$par, c("lambda", "delta0", "delta1", "delta2")),
                convergence = fit0$convergence, message = fit0$message, iter = fit0$iter)

    ## sigma2_psi: not searched, falls out of the fitted residuals directly --
    ## it's psi's own defining moment (E[psi^2]=sigma2_psi), no extra param.
    lambda_f <- fit$par[["lambda"]]; delta0_f <- fit$par[["delta0"]]
    delta1_f <- fit$par[["delta1"]]; delta2_f <- fit$par[["delta2"]]
    arg     <- pmax(1 - 2*lambda_f*stacked$e_hat, 1e-6)
    h       <- log(stacked$sales_tax_rate_purchases) + log(arg)
    psi_hat <- h - delta0_f + delta1_f*stacked$omega_hat - delta2_f*stacked$omega_hat^2
    sigma2_psi_init <- mean(psi_hat^2)

    list(
        par         = c(fit$par, sigma2_psi = sigma2_psi_init),
        beta_range  = range(unincorp$beta),
        n_firms     = n_firms,
        n_rows      = nrow(stacked),
        S           = S,
        convergence = fit$convergence,
        message     = fit$message,
        iter        = fit$iter
    )
}

## ins_only (2026-09-07): "" runs both and overwrites the shared product file
## (1210/1211's default warm-start input); a specific ins name runs just that
## one, timed, and writes to a SEPARATE -ins_only-tagged file so a targeted
## rerun (e.g. testing a higher maxeval_nm) can never clobber the shared one.
run_lag_m  <- opt$ins_only %in% c("", "lag_m")
run_lag2W  <- opt$ins_only %in% c("", "lag_2_cal_W")

if (run_lag_m) {
    t0 <- Sys.time(); warmstart_lag_m <- get_warmstart("lag_m"); t_lag_m <- Sys.time() - t0
    cat("Warm start (ins = lag_m):\n"); print(warmstart_lag_m$par)
    cat(sprintf("  elapsed: %s\n", format(t_lag_m)))
}
if (run_lag2W) {
    t0 <- Sys.time(); warmstart_lag2W <- get_warmstart("lag_2_cal_W"); t_lag2W <- Sys.time() - t0
    cat("Warm start (ins = lag_2_cal_W):\n"); print(warmstart_lag2W$par)
    cat(sprintf("  elapsed: %s\n", format(t_lag2W)))
}

if (opt$ins_only == "") {
    save(warmstart_lag_m, warmstart_lag2W, file = "Code/Products/1205-stage2-warmstart.RData")
} else {
    out_file <- sprintf("Code/Products/1205-stage2-warmstart-%s-maxeval%d.RData", opt$ins_only, opt$maxeval_nm)
    save(list = ls(pattern = "^warmstart_"), file = out_file)
    cat(sprintf("Saved: %s\n", out_file))
}
