## %% Stage-2 MSM ------------------------------------------------
# Sep 11, 2026: Meeting with Tim Conley: Tim could not see why MSM would fail
# Tim suggested simulating from joint distribution of epsilon and omega,
# then for a guess of the parameters, compute the moments, weighted 
# by the the variance (GMM obj function with efficient weighting matrix). 
# That object has a chi squared limiting distribution, so we can use that to test the fit.

## %% Setup ----------------------------------------------------------------
library(tidyverse)
library(nloptr)
library(parallel)

source("Code/Deconvolution/utils-cli.R")

DEFAULTS <- list(
    S = 250, #Number of simulations
    seed = 66636, #reproducibility
    maxeval_nm = 2000, #optimizer parameter: max iterations
    ins_only = "",
    include_lnM = TRUE #FALSE reproduces the exactly-identified 4-moment case
                        #(d_g=d_theta=4) under the CORRECTED objective, for
                        #comparison against the original (buggy-objective) fit
)

opt <- parse_cli_args(DEFAULTS)
opt$S          <- as.integer(opt$S)
opt$seed       <- as.integer(opt$seed)
opt$maxeval_nm <- as.integer(opt$maxeval_nm)
log_run_header("1201-stage2-MSM.R", opt) #Header log

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

## %% Functions  ------------------------------------------------


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

    ## Redraw eps_s (S=1) for every row of df in place, no filtering -- used
    ## by the retry loop below on whatever subset is currently failing.
    ## Row order must survive the per-industry split/recombine, since the
    ## caller assigns the result back into a fixed set of row positions.
    redraw_once <- function(df) {
        df$.tmp_ord <- seq_len(nrow(df))
        sics <- unique(df$sic_chr)
        pieces <- lapply(sics, function(sic_chr) {
            idx  <- which(df$sic_chr == sic_chr)
            pool <- get_pool(sic_chr)
            sub  <- df[idx, ]
            sub$eps_s <- quantile(pool, probs = runif(length(idx)), names = FALSE, na.rm = TRUE)
            sub
        })
        out <- bind_rows(pieces)
        out <- out[order(out$.tmp_ord), ]
        out$.tmp_ord <- NULL
        out %>%
            mutate(
                M_hat     = M_star * exp(-(cal_V + eps_s)),
                e_hat     = M_star - M_hat,
                omega_hat = tilde_cal_W - (1 - beta) * eps_s
            )
    }

    draw_eps <- function(unicorp, S) {
        ## Draw S eps per firm, consistent with inequalities, vectorized by industry
        ## (loop over ~29 sic_3 groups, not n_firms individual firms): for each
        ## industry, draw an (n_j x S) matrix of bootstrap samples from that
        ## industry's pool in one call, then stack. as.vector() on a matrix goes
        ## column-major (draw 1 for every firm, then draw 2 for every firm, ...),
        ## matching rep(idx, times=S).
        ## Tried mclapply(mc.cores=11) over this loop (2026-09-12) and, separately,
        ## over redraw_once()'s equivalent loop below: both were SLOWER than plain
        ## lapply, even for this one-time call (2.20s vs 2.05s wall, S=100, 29
        ## groups; redraw_once's version, called up to 700x in the retry loop, was
        ## 5-10x+ slower end to end) -- fork() on a multi-GB tidyverse process
        ## dominates the cost of these small, fast per-industry tasks. Don't
        ## re-parallelize either loop without a fresh benchmark showing it helps.
        sics <- unique(unicorp$sic_chr)
        stacked_list <- lapply(sics, function(sic_chr) {
            idx  <- which(unicorp$sic_chr == sic_chr)
            pool <- get_pool(sic_chr)
            n_j  <- length(idx)
            eps_mat <- matrix((quantile(pool, probs = runif(n_j * S), names = FALSE, na.rm = TRUE)), nrow = n_j, ncol = S)  # alternative: quantile-based resampling (2026-08-29)
            unicorp[rep(idx, times = S), ] %>%
                mutate(eps_s = as.vector(eps_mat))
        })
        tmp_stacked <- bind_rows(stacked_list) %>%
            mutate(
                M_hat     = M_star * exp(-(cal_V + eps_s)),
                e_hat     = M_star - M_hat,
                omega_hat = tilde_cal_W - (1 - beta) * eps_s
            )

        ## Check inequalities: e_hat>0 and M_hat>0 (M_hat is always >0 by
        ## construction -- M_star*exp(.) -- so this is really just e_hat>0,
        ## i.e. eps_s > -cal_V). Redraw only the failing rows, in place, until
        ## none fail or the iteration cap is hit. Some rows can be structurally
        ## infeasible (their required eps_s exceeds their industry pool's max),
        ## in which case they never clear and are dropped by the final filter.
        ctrl_iter <- 0
        repeat {
            id_fails <- which(tmp_stacked$e_hat <= 0 | tmp_stacked$M_hat <= 0)
            if (length(id_fails) == 0 || ctrl_iter >= 700) break
            ctrl_iter <- ctrl_iter + 1
            if (ctrl_iter %% 100 == 0) cat(sprintf("  Redraw iteration: %d (%d rows still failing)\n", ctrl_iter, length(id_fails)))
            tmp_stacked[id_fails, ] <- redraw_once(tmp_stacked[id_fails, ])
        }

        tmp_stacked %>% dplyr::filter(e_hat > 0, M_hat > 0)  # final filter to ensure only valid rows are returned
    }
    stacked <- draw_eps(unincorp, S = S)

    cat(sprintf(
        "  [%s] %d unincorp firms x S=%d draws = %d rows, %d (%.1f%%) dropped (e_hat<0 draws)\n",
        ins_choice, n_firms, S, n_firms * S,
        n_firms * S - nrow(stacked), 100 * (n_firms * S - nrow(stacked)) / (n_firms * S)
    ))

    ## %% Feasible starting lambda -------------------------------------------

    lambda0    <- unname(1 / (2 * quantile(stacked$M_star, 0.75, na.rm = TRUE))) # Start at the m
    lambda_max <- 1/(2*min(stacked$M_star, na.rm = TRUE)) # Extreme: All reported materials are overreporting, all is evasion
    lambda_min <- unname(1 / (2 * max(stacked$M_star, na.rm = TRUE))) #Extreme: Nothing is evasion. All reported materials are true materials.
    stopifnot(
        "lambda_min>lambda_max" = lambda_min < lambda_max,
        "lambda0 not in [lambda_min, lambda_max]" = lambda0 >= lambda_min && lambda0 <= lambda_max
    )
    ## %% Overidentified GMM fit on the stacked (firm x draw) sample ---------
    ## g(theta) = (E[psi], E[psi*om], E[psi*om^2], E[psi*lnM], E[eps*h'_bounded])
    ## -- 5 moments, 4 parameters (lambda,delta0,delta1,delta2) -> d_g-d_theta=1
    ## overidentifying restriction, so the CUE objective at the optimum has a
    ## genuine (non-degenerate) chi-sq reference distribution (2026-09-13,
    ## following up on Tim Conley's suggestion -- see Research-log.md).
    lnM_hat <- log(stacked$M_hat)
    firm_id <- stacked$.row_id   # rows sharing a firm share every covariate
                                  # (M_star,cal_V,tilde_cal_W,tau) except the
                                  # resampled eps_s -- NOT iid draws, so the
                                  # moment covariance below is built from
                                  # per-firm averages, not raw stacked rows

    ## Collapses S draws/firm to one row per firm BEFORE estimating the moment
    ## covariance (2026-09-13 fix). Pooling all n_firms*S rows into a single
    ## cov() call, as the original version did, mixes genuine between-firm
    ## sampling variance with pure simulation noise from resampling eps_s. The
    ## asymptotically correct object to pair with n=n_firms is the CROSS-FIRM
    ## covariance of each firm's own S-draw average (Pakes & Pollard 1989;
    ## McFadden 1989 SMM asymptotics) -- S integrates out eps by simulation,
    ## it is not additional real sample size.
    build_moments <- function(par) {
        lambda <- par[1]; delta0 <- par[2]; delta1 <- par[3]; delta2 <- par[4]
        arg     <- pmax(1 - 2*lambda*stacked$e_hat, 1e-6)   # clip infeasible draws instead of erroring
        h       <- log(stacked$sales_tax_rate_purchases) + log(arg)
        h_prime   <- -2 * stacked$e_hat / arg
        h_prime_bounded <- h_prime/(1-h_prime) # bounded transform of h_prime (2026-09-07)
        stopifnot(
            "1/lambda is not finite"=is.finite(1/lambda),
            "e_hat is not finite"=all(is.finite(stacked$e_hat)),
            "h' is not finite"=all(is.finite(h_prime)),
            "h'_bounded is not finite"=all(is.finite(h_prime_bounded))
            )
        psi_hat <- h - delta0 + delta1*stacked$omega_hat - delta2*stacked$omega_hat^2
        g <- cbind(
            psi_hat,
            psi_hat * stacked$omega_hat,
            psi_hat * stacked$omega_hat*stacked$omega_hat, # was ^2 (faster?)
            if (opt$include_lnM) psi_hat * lnM_hat else NULL, # overidentifying moment so d_g > d_theta (2026-09-13); include_lnM=FALSE reproduces the exactly-identified case under the fixed objective
            stacked$eps_s * h_prime_bounded # score moment for lambda (bounded h' transform); was psi*h_prime_bounded
        )
        g_sum_by_firm <- rowsum(g, group = firm_id)
        n_by_firm     <- as.vector(table(firm_id))
        g_sum_by_firm / n_by_firm   # one row per firm: that firm's own S-draw average
    }

    gmm_obj <- function(par) {
        g_by_firm <- build_moments(par)
        dvec  <- colMeans(g_by_firm, na.rm = TRUE)
        Omega <- cov(g_by_firm)
        eigen_Omega <- eigen(Omega, symmetric = TRUE)
        pos_eigen <- eigen_Omega$values > 1e-8
        A <- eigen_Omega$vectors[, pos_eigen, drop = FALSE]
        d2 <- drop(t(A) %*% dvec)
        V2 <- t(A) %*% Omega %*% A
        Omega_inv <- solve(V2)

        return(drop(t(d2) %*% Omega_inv %*% d2))  # scalar CUE objective, both
        # d2 and Omega_inv expressed in the SAME (eigenvector) basis -- the
        # prior version paired Omega_inv with the raw, un-rotated dvec
        # instead, which is dimensionally legal but silently wrong whenever
        # Omega is non-diagonal (verified empirically: ~15% error on a
        # synthetic check, worse here given Omega's eigenvalues span 3+
        # orders of magnitude). 2026-09-13 fix.
    }

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
                convergence = fit0$convergence, message = fit0$message, iter = fit0$iter,
                value = fit0$value)

    ## sigma2_psi: not searched, falls out of the fitted residuals directly --
    ## it's psi's own defining moment (E[psi^2]=sigma2_psi), no extra param.
    lambda_f <- fit$par[["lambda"]]; delta0_f <- fit$par[["delta0"]]
    delta1_f <- fit$par[["delta1"]]; delta2_f <- fit$par[["delta2"]]
    arg     <- pmax(1 - 2*lambda_f*stacked$e_hat, 1e-6)
    h       <- log(stacked$sales_tax_rate_purchases) + log(arg)
    psi_hat <- h - delta0_f + delta1_f*stacked$omega_hat - delta2_f*stacked$omega_hat^2
    sigma2_psi_init <- mean(psi_hat^2)

    ## d_g_eff/n_eff recomputed at the fitted par, rather than hardcoded, so
    ## the degrees of freedom self-updates if eigenvalue-trimming ever drops a
    ## near-collinear moment, or the moment count changes again later.
    g_by_firm_f <- build_moments(fit$par)
    eigen_f     <- eigen(cov(g_by_firm_f), symmetric = TRUE)
    d_g_eff     <- sum(eigen_f$values > 1e-8)
    n_eff       <- nrow(g_by_firm_f)   # firms actually contributing a valid
                                         # moment row (can be < n_firms if a
                                         # firm's draws never clear the
                                         # feasibility filter)
    df_test     <- d_g_eff - length(fit$par)

    J_stat <- n_eff * fit$value   # n = n_eff (firms), NOT n_firms*S: S only
    # integrates out eps by simulation, it is not additional real sample size
    # (McFadden 1989; Pakes & Pollard 1989 SMM asymptotics)

    rej_rule <- if (df_test <= 0) {
        sprintf("d_g_eff(%d) <= d_theta(%d): NOT overidentified -- no valid chi-sq test", d_g_eff, length(fit$par))
    } else {
        crit <- qchisq(0.95, df = df_test)
        sprintf("%s H_0: E[g(theta)]=0  (J=%.3f, chi2_{%d,.95}=%.3f)",
                ifelse(J_stat > crit, "Reject", "Fail to reject"), J_stat, df_test, crit)
    }

    list(
        par         = c(fit$par, sigma2_psi = sigma2_psi_init),
        L_n         = fit$value,
        J_stat      = J_stat,
        d_g_eff     = d_g_eff,
        df_test     = df_test,
        n_eff       = n_eff,
        rej_rule    = rej_rule,
        beta_range  = range(unincorp$beta),
        n_firms     = n_firms,
        n_rows      = nrow(stacked),
        S           = S,
        convergence = fit$convergence,
        message     = fit$message,
        iter        = fit$iter
    )
}

run_lag_m  <- opt$ins_only %in% c("", "lag_m")
run_lag2W  <- opt$ins_only %in% c("", "lag_2_cal_W")

if (run_lag_m) {
    t0 <- Sys.time(); warmstart_lag_m <- get_warmstart("lag_m"); t_lag_m <- Sys.time() - t0
    cat("Convergence code (lag_m):", warmstart_lag_m$convergence, warmstart_lag_m$message, "\n")
    cat("Warm start (ins = lag_m):\n"); print(warmstart_lag_m$par)
    cat(sprintf("d_g_eff=%d, d_theta=4, df_test=%d, n_eff=%d, n_firms=%d, L_n=%.6f, J_stat=%.4f\n",
                warmstart_lag_m$d_g_eff, warmstart_lag_m$df_test, warmstart_lag_m$n_eff,
                warmstart_lag_m$n_firms, warmstart_lag_m$L_n, warmstart_lag_m$J_stat))
    cat("Rejection rule:", warmstart_lag_m$rej_rule, "\n")
    cat(sprintf("  elapsed: %s\n", format(t_lag_m)))
}
if (run_lag2W) {
    t0 <- Sys.time(); warmstart_lag2W <- get_warmstart("lag_2_cal_W"); t_lag2W <- Sys.time() - t0
    cat("Convergence code (lag_2_cal_W):", warmstart_lag2W$convergence, warmstart_lag2W$message, "\n")
    cat("Warm start (ins = lag_2_cal_W):\n"); print(warmstart_lag2W$par)
    cat(sprintf("d_g_eff=%d, d_theta=4, df_test=%d, n_eff=%d, n_firms=%d, L_n=%.6f, J_stat=%.4f\n",
                warmstart_lag2W$d_g_eff, warmstart_lag2W$df_test, warmstart_lag2W$n_eff,
                warmstart_lag2W$n_firms, warmstart_lag2W$L_n, warmstart_lag2W$J_stat))
    cat("Rejection rule:", warmstart_lag2W$rej_rule, "\n")
    cat(sprintf("  elapsed: %s\n", format(t_lag2W)))
}

lnM_tag <- if (opt$include_lnM) "" else "-exactID"   # exactly-identified 4-moment
                                                       # variant, run under the
                                                       # fixed objective for
                                                       # comparison (2026-09-14)
if (opt$ins_only == "") {
    save(warmstart_lag_m, warmstart_lag2W, file = sprintf("Code/Products/1201-stage2-MSM%s.RData", lnM_tag))
} else {
    out_file <- sprintf("Code/Products/1201-stage2-MSM-%s-maxeval%d%s.RData", opt$ins_only, opt$maxeval_nm, lnM_tag)
    save(list = ls(pattern = "^warmstart_"), file = out_file)
    cat(sprintf("Saved: %s\n", out_file))
}
