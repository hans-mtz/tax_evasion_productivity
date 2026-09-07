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

DEFAULTS <- list(S = 250, seed = 20260828)
opt <- parse_cli_args(DEFAULTS)
opt$S    <- as.integer(opt$S)
opt$seed <- as.integer(opt$seed)
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
        h_prime <- ifelse(1 - 2*lambda*stacked$e_hat == 0, -9e200, -2*stacked$e_hat / (1 - 2*lambda*stacked$e_hat))  # derivative of h wrt lambda
        h_prime_c <- (h_prime - mean(h_prime)) / sd(h_prime)  # standardized h' for numerical stability (2026-08-31)
        stopifnot(
            "1/lambda is not finite"=is.finite(1/lambda),
            "e_hat is not finite"=all(is.finite(stacked$e_hat)),
            # "1/2e=lambda"=all(2*lambda*stacked$e_hat != 1, na.rm = TRUE),
            "h' is not finite"=all(is.finite(h_prime)),
            "h' centered is not finite"=all(is.finite(h_prime_c))
            )
        # stopifnot("e_hat is not finite"=all(is.finite(stacked$e_hat)))
        # stopifnot("h' is not finite"=all(is.finite(h_prime)))
        # stopifnot("exp(-h') is not finite"=all(is.finite(exp(-h_prime))))
        psi_hat <- h - delta0 + delta1*stacked$omega_hat - delta2*stacked$omega_hat^2
        g <- c(
            mean(psi_hat),
            mean(psi_hat * stacked$omega_hat),
            mean(psi_hat * stacked$omega_hat*stacked$omega_hat), # was ^2 (faster?)
            mean(psi_hat * h_prime_c) # Naive moment: psi not independent of e; Was psi*lnM_hat
        )
        sum(g^2)
    }

    fit <- optim(
        par    = c(lambda = lambda0, delta0 = 0, delta1 = 0, delta2 = 0),
        fn     = gmm_obj,
        method = "L-BFGS-B",
        lower  = c(lambda_min, -50, -50, -50),
        upper  = c(lambda_max, 50, 50, 50),
        control = list(maxit = 200)  # safety net: this is a crude warm start (doesn't need to be
                                      # consistent, just close), so a bad bounds/starting-point
                                      # region should never be allowed to run unbounded again
    )

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
        convergence = fit$convergence
    )
}

warmstart_lag_m <- get_warmstart("lag_m")
warmstart_lag2W <- get_warmstart("lag_2_cal_W")

cat("Warm start (ins = lag_m):\n");       print(warmstart_lag_m$par)
cat("Warm start (ins = lag_2_cal_W):\n"); print(warmstart_lag2W$par)

save(warmstart_lag_m, warmstart_lag2W, file = "Code/Products/1205-stage2-warmstart.RData")
