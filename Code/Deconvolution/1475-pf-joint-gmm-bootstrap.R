## Bootstrap SEs for the joint efficient-GMM PF estimates (2026-09-21), using the approved bootstrap design from
## 490-boot-pf-prod.R / 240-fs-boot.R: resample_by_group() (block-bootstrap plants WITH replacement, stratified by
## sic_3 x Corp/Other, whole plant history kept -> preserves within-plant serial correlation), re-run
## first_stage_panel_me PER REPLICATE (re-estimates beta_b and refreshes cal_V/cal_W -- this is what propagates
## stage-1 sampling variance into the PF estimates, same as GNR's own bootstrap), then refit the joint (m*_{it-1} +
## W~_{it-2}) efficient-GMM estimator of 1474 on the resampled fs_list. CI convention matches 490 exactly:
## bias-corrected percentile, CI = point_estimate - quantile(boot - point_estimate, c(.975,.025)).
library(tidyverse); library(parallel)
load("Code/Products/global_vars.RData"); load("Code/Products/deconv_funs.Rdata")
load("Code/Products/931.1-fs-se-het.RData")   # df (raw panel + current cal_V/cal_W/epsilon), fs_all_ls (point-estimate fits)
five <- c("331", "322", "369", "313", "321")
base_df <- df %>% select(-any_of(c("cal_V", "cal_W", "epsilon")))   # same rows first_stage_panel_me sees (930.1's wip_df), re-filtered inside resample_by_group/first_stage_panel_me anyway

## %% Joint efficient-GMM fit, refactored to take an explicit fs_list (was hardcoded in 1474) -----------------
prep <- function(fs_list, x) {
    d <- fs_list[[x]]$data %>% ungroup() %>% filter(is.finite(cal_W), is.finite(k), is.finite(l), is.finite(m)) %>% arrange(plant, year)
    n <- nrow(d); pl <- d$plant
    lagi <- function(j) { i <- seq_len(n) - j; ok <- i >= 1; i[!ok] <- NA; i[ok][pl[pmax(i[ok], 1)] != pl[ok]] <- NA
                          if (j == 2) { i1 <- seq_len(n) - 1; bad <- is.na(i) | i1 < 1 | pl[pmax(i1, 1)] != pl; i[bad] <- NA }; i }
    list(d = d, p1 = lagi(1), p2 = lagi(2))
}
fit_joint <- function(fs_list, x, starts_extra = list()) {
    P <- tryCatch(prep(fs_list, x), error = function(e) NULL); if (is.null(P) || is.null(P$d) || nrow(P$d) < 30) return(NULL)
    d <- P$d; S <- which(!is.na(P$p1) & !is.na(P$p2)); if (length(S) < 30) return(NULL)
    p1 <- P$p1[S]; p2 <- P$p2[S]; pl <- d$plant[S]; n <- length(S)
    build <- function(aK, aL) { w <- d$cal_W - aK * d$k - aL * d$l; y <- w[S]; xlag <- w[p1]
        list(M = cbind(1, d$m[p1], w[p2], d$k[S], d$l[S]), y = y, xlag = xlag) }
    gamma_given <- function(aK, aL, W) { mm <- build(aK, aL); M <- mm$M; y <- mm$y; x <- mm$xlag
        a <- colMeans(M * y); B <- crossprod(M, cbind(1, x)) / n
        gam <- tryCatch(solve(t(B) %*% W %*% B, t(B) %*% W %*% a), error = function(e) c(NA, NA))
        if (anyNA(gam)) return(NULL); eta <- y - gam[1] - gam[2] * x; list(gam = gam, G = M * eta) }
    crit <- function(par, W) { if (any(par < 0 | par > 1)) return(1e10 * sum(pmax(0 - par, par - 1, 0)^2) + 1e6)
        r <- gamma_given(par[1], par[2], W); if (is.null(r)) return(1e12); gbar <- colMeans(r$G); n * drop(t(gbar) %*% W %*% gbar) }
    search <- function(W, starts) { best <- NULL
        for (s0 in starts) { o1 <- tryCatch(optim(s0, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 200)), error = function(e) NULL)
            if (is.null(o1)) next
            o2 <- tryCatch(optim(o1$par, crit, W = W, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control = list(maxit = 200)), error = function(e) o1)
            if (is.null(best) || o2$value < best$value) best <- o2 }
        best }
    clustered_Omega <- function(G) { rs <- rowsum(G, pl); np <- as.vector(table(pl))[match(rownames(rs), names(table(pl)))]
        Gc <- rs - outer(np, colMeans(G)); crossprod(Gc) / n }
    ols0 <- tryCatch(pmin(pmax(coef(lm(cal_W ~ k + l, d))[-1], 0.01), 0.99), error = function(e) c(0.3, 0.3))
    starts <- unique(c(list(as.numeric(ols0), c(0.3, 0.3)), starts_extra))
    r0 <- gamma_given(ols0[1], ols0[2], diag(5)); if (is.null(r0)) return(NULL)
    v0 <- apply(r0$G, 2, var); if (any(!is.finite(v0)) || any(v0 <= 0)) return(NULL); W_id <- diag(1 / v0)
    fit1 <- search(W_id, starts); if (is.null(fit1)) return(NULL); a1 <- fit1$par
    r1 <- gamma_given(a1[1], a1[2], W_id); if (is.null(r1)) return(NULL)
    Om <- clustered_Omega(r1$G); W_eff <- tryCatch(solve(Om), error = function(e) tryCatch(solve(Om + 1e-6 * diag(5)), error = function(e2) NULL))
    if (is.null(W_eff)) return(NULL)
    fit2 <- search(W_eff, unique(c(starts, list(a1)))); if (is.null(fit2)) return(NULL)
    tibble(alpha_K = fit2$par[1], alpha_L = fit2$par[2], beta = fs_list[[x]]$beta, n = n)
}

## %% Point estimate (t0), for reference / bias correction -------------------------------------------------------
t0 <- bind_rows(lapply(five, \(x) fit_joint(fs_all_ls, x) %>% mutate(sic_3 = x)))
cat("== Point estimate (full sample) ==\n"); print(t0)

## %% Time ONE bootstrap replicate before committing to the full run --------------------------------------------
time_one <- function(seed) {
    set.seed(seed)
    resampled <- resample_by_group(base_df, sic_3)
    fs_b <- setNames(lapply(five, \(x) first_stage_panel_me(x, "log_mats_share", "materials", resampled)), five)
    bind_rows(lapply(five, \(x) { r <- fit_joint(fs_b, x, starts_extra = list(as.numeric(t0[t0$sic_3 == x, c("alpha_K", "alpha_L")]))); if (is.null(r)) return(NULL); r %>% mutate(sic_3 = x) }))
}
t_start <- Sys.time(); rep1 <- time_one(66636 + 1); elapsed <- as.numeric(Sys.time() - t_start, units = "secs")
cat(sprintf("\nOne bootstrap replicate: %.1f sec (5 industries). Estimated wall time for R=250 on %d cores: %.1f min\n",
            elapsed, max(1, detectCores() - 2), elapsed * 250 / max(1, detectCores() - 2) / 60))
print(rep1)
save(t0, rep1, elapsed, file = "Code/Products/1475-pf-bootstrap-timing.RData")
