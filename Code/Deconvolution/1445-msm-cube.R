## %% Naive MSM cube (Tim Conley): J test at 7 points around the optimizer solution
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
    center_lambda = 6.847224e-4, center_delta1 = 14.9643, center_delta2 = 3.424686,
    wfix = FALSE, #FALSE = legacy (unweighted, clipped): reproduces the published cubes. TRUE = UNTESTED/shelved 2026-09-21: per-industry normal f_omega weights + infeasible draws weight 0 (a nonparametric deconvolved f_omega was planned instead, then dropped)
    tag = "", #"" = MSM optimum (legacy file names); "elvis" = ELVIS point
    include_lnM = TRUE #FALSE reproduces the exactly-identified 4-moment case
                        #(d_g=d_theta=4) under the CORRECTED objective, for
                        #comparison against the original (buggy-objective) fit
)

opt <- parse_cli_args(DEFAULTS)
opt$S          <- as.integer(opt$S)
opt$seed       <- as.integer(opt$seed)
opt$maxeval_nm <- as.integer(opt$maxeval_nm)
opt$wfix <- as.logical(opt$wfix); opt$center_lambda <- as.numeric(opt$center_lambda); opt$center_delta1 <- as.numeric(opt$center_delta1); opt$center_delta2 <- as.numeric(opt$center_delta2)
log_run_header("1445-msm-cube.R", opt) #Header log

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
    ## %% Unconditional f_omega weights (theta-free, precomputed once) ----------
    ## Per-industry normal, moment-deconvolved from W_tilde on ALL firms
    ## (same formulas as 1401-msl-export.R, Version A; thin industries use the
    ## pooled values). w0 = f_omega(omega_hat) normalized by the firm's own max
    ## (log-scale, avoids underflow); only the feasibility indicator changes with theta.
    et <- eps_targets %>% dplyr::filter(ins == ins_choice) %>%
        transmute(sic_chr = as.character(sic_3), mu_eps = mu1, sd_eps = sqrt(var), pooled_fallback)
    allw <- run_data %>% dplyr::filter(is.finite(tilde_cal_W), is.finite(beta)) %>%
        mutate(sic_chr = as.character(sic_3)) %>% left_join(et, by = "sic_chr")
    om_ind <- allw %>% group_by(sic_chr) %>%
        summarise(m_w = mean(tilde_cal_W), v_w = var(tilde_cal_W), bo = 1 - mean(beta),
                  mu_eps = first(mu_eps), sd_eps = first(sd_eps), pooled_fallback = first(pooled_fallback), .groups = "drop") %>%
        mutate(mu_om = m_w - bo * mu_eps, var_om = pmax(v_w - bo^2 * sd_eps^2, 0.05 * v_w))
    pool_row <- allw %>% summarise(m_w = mean(tilde_cal_W), v_w = var(tilde_cal_W), bo = 1 - mean(beta),
                                   mu_eps = mean(et$mu_eps), sd_eps = sqrt(mean(et$sd_eps^2))) %>%
        mutate(mu_om = m_w - bo * mu_eps, var_om = pmax(v_w - bo^2 * sd_eps^2, 0.05 * v_w))
    om_ind <- om_ind %>% mutate(mu_om = ifelse(pooled_fallback, pool_row$mu_om, mu_om),
                                sd_om = sqrt(ifelse(pooled_fallback, pool_row$var_om, var_om)))
    om_idx <- match(stacked$sic_chr, om_ind$sic_chr)
    stopifnot("industry missing from f_omega table" = !anyNA(om_idx))
    logfw <- dnorm(stacked$omega_hat, mean = om_ind$mu_om[om_idx], sd = om_ind$sd_om[om_idx], log = TRUE)
    fmax  <- tapply(logfw, firm_id, max)
    w0    <- exp(logfw - fmax[as.character(firm_id)])
    rm(logfw)

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
        if (!opt$wfix) {
            g_sum_by_firm <- rowsum(g, group = firm_id)
            n_by_firm     <- as.vector(table(firm_id))
            return(g_sum_by_firm / n_by_firm)   # legacy: unweighted, clipped
        }
        w  <- w0 * (1 - 2*lambda*stacked$e_hat > 0)       # f_omega weight x FOC-domain indicator
        sw <- rowsum(w, group = firm_id)[, 1]
        gw <- rowsum(g * w, group = firm_id)
        keep <- sw > 1e-300                               # firms with no feasible weight drop out at this theta
        out  <- gw[keep, , drop = FALSE] / sw[keep]
        attr(out, "frac_infeas") <- mean(1 - 2*lambda*stacked$e_hat <= 0)
        out
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


    ## %% CUBE: center + 6 adjacent points (lambda x/÷2, delta1 +-1, delta2 +-0.2),
    ## delta0 PROFILED (1-D) at each point, same draws (stacked) for all points.
    center <- c(lambda = opt$center_lambda, delta1 = opt$center_delta1, delta2 = opt$center_delta2)
    pts <- rbind(
        center,
        center * c(2, 1, 1), center * c(0.5, 1, 1),
        center + c(0, 1, 0), center - c(0, 1, 0),
        center + c(0, 0, 0.2), center - c(0, 0, 0.2)
    )
    rownames(pts) <- c("center", "lambda_up", "lambda_dn", "d1_up", "d1_dn", "d2_up", "d2_dn")

    profile_d0 <- function(l, d1, d2) {
        f <- function(d0) gmm_obj(c(l, d0, d1, d2))
        grid <- seq(-50, 50, length.out = 41)
        fv <- vapply(grid, f, numeric(1))
        k <- which.min(fv); lo <- grid[max(k - 1, 1)]; hi <- grid[min(k + 1, length(grid))]
        o <- optimize(f, c(lo, hi), tol = 1e-6)
        c(delta0 = o$minimum, Lhat = o$objective)
    }
    res <- do.call(rbind, parallel::mclapply(seq_len(nrow(pts)), mc.cores = nrow(pts), function(i) {
        p <- pts[i, ]
        pr <- profile_d0(p[["lambda"]], p[["delta1"]], p[["delta2"]])
        gb <- build_moments(c(p[["lambda"]], pr[["delta0"]], p[["delta1"]], p[["delta2"]]))
        ev <- eigen(cov(gb), symmetric = TRUE)$values
        r <- data.frame(point = rownames(pts)[i], lambda = p[["lambda"]], delta0 = pr[["delta0"]],
                        delta1 = p[["delta1"]], delta2 = p[["delta2"]], Lhat = pr[["Lhat"]],
                        n_eff = nrow(gb), n_firms = n_firms, frac_infeas = if (is.null(attr(gb, "frac_infeas"))) NA_real_ else attr(gb, "frac_infeas"), d_g_eff = sum(ev > 1e-8))
        r$J <- r$n_eff * r$Lhat; r$crit95 <- qchisq(0.95, r$d_g_eff); r$reject <- r$J > r$crit95
        print(r); r
    }))
    res$ins <- ins_choice
    res
}

res <- get_warmstart("lag_m")
out <- sprintf("Code/Products/1445-msm-cube%s-lag_m.csv", if (opt$tag == "") "" else paste0("-", opt$tag))
write.csv(res, out, row.names = FALSE)
cat("Saved:", out, "\n")
